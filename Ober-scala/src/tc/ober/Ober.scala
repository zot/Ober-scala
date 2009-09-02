/*
(C) 2009 Bill Burdick

ar.ober.OberDragWidget

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

import java.io.File;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.awt.Color;
import java.awt.Container;
import javax.swing.border.LineBorder;
import javax.swing.text.JTextComponent;
import java.io.IOException;
import scala.io.Source;
import scala.collection.mutable.{HashMap => MMap, HashSet => MSet, StringBuilder, ArrayBuffer};
import scala.collection.Sequence;
import scala.util.matching.Regex;
import scala.tools.nsc.{Interpreter, Settings, InterpreterResults => IR}
import scala.actors.Future
import scala.actors.Futures
import scala.actors.Actor._
import scala.swing.Swing._
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.SourceFile
import scala.tools.nsc.util.BatchSourceFile

trait AbstractOberWindow

trait ScalaViewer[T] extends AbstractViewer[T] {
	def focus
	def gainedFocus
	def lostFocus
	def properties: MMap[Any, Any]
	def errorFromProcess(line: String)
	def outputFromProcess(line: String)
	def delete
	def isTrack: Boolean
	def parent: T
	def createNewViewer: ScalaViewer[_]
	def createNewTrack: ScalaViewer[_]
	def find(name: String): ScalaViewer[_]
	def find(predicate: ScalaViewer[_] => Boolean): ScalaViewer[_]
	def clear
	def append(str: String)
	def name: String
	def name_=(newName: String)
	def load(str: String = null)
	def get(src: Source = null)
	def getHtml(src: Source = null, context: Any = null)
	def put
	def defaultComponent: JTextComponent
	def filename: java.io.File
	def run(ctx: SimpleContext)
	def surf
//	def surf(ctx: SimpleContext)
	def surfTo(newName: String)
	def topViewer: OberViewer
	def trackPosition(x: Int)
	def trackWidth(w: Int)
	def viewerPosition(y: Int)
	def viewerHeight(h: Int)
	def namespaces: Option[String]
	def namespaces_=(str: String)
	def tag: JTextComponent
	def dirty: Boolean
	def dirty_=(dirt: Boolean)
	def memento: ViewerMemento
}

trait ViewerMemento {
	def restore
	def viewer: ScalaViewer[_]
}

class SimpleContext(val comp: JTextComponent, var viewer: ScalaViewer[_ <: ScalaViewer[_]], var word: String, val wStart: Int, val wEnd: Int, var myMatcher: ArgMatcher = null, txt: String = null) {
	var surfReplace = true
	def text(start: Int, len: Int): String = if (txt != null) txt.slice(start, start + len) else comp.getDocument.getText(start, len)
	def matcher = {
		if (myMatcher == null) {
			val doc = comp.getDocument
			myMatcher = new ArgMatcher(doc.getText(wEnd, doc.getLength - wEnd), true, viewer)
		}
		myMatcher
	}
	def error(str: String) = viewer.errorFromProcess(str)
	def find(name: String) = viewer.topViewer.find(name)
	def namespaceList = viewer.namespaces.getOrElse("").split(" +")
	def namespaces = namespaceList.map(Ober.namespaces.get(_)).flatMap(_.get.fullPath).reverse.removeDuplicates.reverse
}

object Ober {
	type Cmd = SimpleContext => Any

	var currentContext: SimpleContext = null
	var windows = List[AbstractOberWindow]()
	var focus: ScalaViewer[_] = null
	var idCounter = 0
	var interpError = ""
	val wordPlusRest = """^([-a-zA-Z0-9_<>|!.:$/+]+) *([^ \n](?:[^\n"]|"(?:[^"]|\n|\\")*")*)?(?:\n(?:.|\n)*)?$""".r
	val wordPattern = """[-a-zA-Z0-9_<>|!.:$/+]+""" r
	val namespacePattern = """([-a-zA-Z0-9_<>|!.:$/{}+]*) *\[(([-a-zA-Z0-9_<>|!.:$/{}+]+)?( +[-a-zA-Z0-9_<>|!.:$/{}+]+)*)]""" r
	val namePattern = """^[^:]*: *([-a-zA-Z0-9_<>|!.:$/{}+]+)([^-a-zA-Z0-9_<>|!.:$/{}+]|$)""" r
	val defaultOberTagText = "[Ober] New Newcol Help Back Forward Quit"
	val defaultTrackTagText = "[Track] New Delcol"
	val defaultViewerTagText = "./New [File] Get GetHTML Put Del Clean"
	def defaultErrorViewerTagText = "Err [File] Del Clean"
	val namespaces = MMap[String, Namespace]()
	val redBorder = new LineBorder(Color.red)
	val grayBorder = LineBorder.createGrayLineBorder
	val blackBorder = LineBorder.createBlackLineBorder
	val classNs = new ClassNamespace("Class")
	val systemNs = new SystemNamespace("System")
	val oberNs = new ClosureNamespace("Ober", noSurf,
		"Quit" -> {_ => System.exit(0)},
		"New" -> (_.viewer.createNewViewer),
		"Newcol" -> (_.viewer.createNewTrack),
		"Load" -> (Utils.load(_)),
		"Echo" -> (ctx => ctx.error(ctx.matcher.toSequence.mkString(" "))),
		"Help" -> (ctx => Utils.help(ctx.viewer)),
		"Rename" -> (Utils.rename(_)),
		"Run" -> (Utils.run(_)),
		"Append" -> (Utils.appendText(_)),
		"Exec" -> (Utils.execCmd(_)),
		"Namespaces" -> (Utils.setNamespaces(_)),
		"Back" -> (Utils.back(_)),
		"Forward" -> (Utils.forward(_)),
		"Clean" -> (_.viewer.dirty_=(false)),
		"LoadScript" -> (Utils.loadScript(_))
	)
	val trackNs = new ClosureNamespace("Track", noSurf,
		"Delcol" -> {ctx =>
			val v = if (ctx.viewer.isTrack) ctx.viewer else ctx.viewer.parent
			if (v.isTrack) v.delete},
		"Width" -> (Utils.trackWidth(_)),
		"TrackPosition" -> (Utils.trackPosition(_))
	)
	val fileNs = new ClosureNamespace("File", Utils.surfFile,
		"Del" -> (_.viewer.delete),
		"Height" -> (Utils.viewerHeight(_)),
		"ViewerPosition" -> (Utils.viewerPosition(_)),
		"Get" -> (_.viewer.get()),
		"GetHTML" -> (_.viewer.getHtml()),
		"Put" -> (_.viewer.put)
	)

	def noSurf(ctx: SimpleContext): Option[(SimpleContext, ScalaViewer[_]) => Any] = None
	def onEDTFuture[T](block: => T) = {
		val res = new Array[Future[T]](1)
		onEDTWait(res(0) = Futures.future(block))
		res(0)
	}
	def toFile(obj: Any): File = {
		obj match {
			case s: String => new File(s)
			case f: java.io.File => f
			case other => new File(other.toString)
		}
	}
	def run(ctx: SimpleContext) = Utils.handlerFor(ctx.word, ctx)
	def surf(ctx: SimpleContext) = {
		var found = false
		var seen = MSet[Namespace]()

		ctx.namespaces.view.map(_.surf(ctx)).find(opt => opt.isDefined).map {block =>
			val viewer = viewerForSurf(ctx.word, ctx)
			(block get)(ctx, viewer)
			viewer.focus
		}.isDefined
	}
	def viewerForSurf(name: String, ctx: SimpleContext) = {
		if (ctx.surfReplace) {
			ctx.viewer.name = name
			ctx.viewer
		} else Utils.findOrCreateViewer(name, ctx.viewer, true)
	}
	def nextId = {
		idCounter += 1
		"ID: " + idCounter
	}
	def tryUri(s: Any): Option[java.net.URI] = {
		s match {
			case u: java.net.URL => Some(u.toURI)
			case u: java.net.URI => Some(u)
			case f: java.io.File => Some(f.toURI)
			case s: String => try {
				Some(new java.net.URI(s))
			} catch {
				case _ => None
			}
		}
	}
	def condensePath(path: String) = {
		var p = normalizePath(path)

		if (p.startsWith(ViewerImports.HOME)) {
			"{HOME}"+p.drop(ViewerImports.HOME.length)
		} else p
	}
	def normalizePath(path: String): String = {
		try {
			var u = new URI(path)
	
			if (u.getPath == "") {
				u = new URI(path + "/")
			}
			if (u.getScheme == "file") u.toString.drop("file:".length)
			else u.toString
		} catch {
			case _ => path
		}
	}
	def contextForName(name: String, child: String = null) = {
		Ober.tryUri(resolve(name)) match {
		case Some(uri) if (uri.getScheme == null || uri.getScheme == "file") => new java.io.File(uri.getPath)
		case Some(uri) => uri.toURL
		case None => null
		}
	}
	def path(parent: String, child: String): (Boolean, String) = {
		contextForName(child) match {
			case u: URL => (true, condensePath(new URL(u, child).toExternalForm))
			case ch: File => contextForName(parent) match {
				case u: URL => (true, condensePath(new URL(u, child).toExternalForm))
				case f: File =>
					(if (ch.isAbsolute) ch else new File(f, child)) match {
						case f if (f.isDirectory) => (f.exists, condensePath(f.getAbsolutePath + File.separator))
						case f => (f.exists, condensePath(f.getAbsolutePath))
					}
				case _ => (ch.exists, ch.getAbsolutePath)
			}
			case _ => (false, null)
		}
	}
	def urlForContext(context: Any) = context match {
		case u: URL => u
		case u: URI => u.toURL
		case f: File => f.toURL
		case _ => null
	}
	def resolve(str: String) = ArgMatcher(str).toSequence.mkString("")
	def gainFocus(v: ScalaViewer[_]) {
		if (focus != v) {
			if (focus != null) focus.lostFocus
			focus = v
			focus gainedFocus
		}
	}
	def eval(str: String, viewer: ScalaViewer[_]) = exec("result(0) = {"+str+";}", viewer, Utils.interp)
	def exec(str: String, viewer: ScalaViewer[_], i: Interpreter) = {
		interpError = ""
		i.beQuietDuring(i.interpret(str)) match {
			case IR.Success => Some(Utils.interpResult(0))
			case IR.Error | IR.Incomplete =>
				if (viewer != null) viewer.errorFromProcess(Ober.interpError)
				else println(interpError)
				None
		}
	}

	Utils.init
}
object ViewerImports {
	def OBER = Ober
	def HOME = System.getProperty("user.home")
}
object Utils {
	val initialized = MSet[Class[_]]()
	val history = ArrayBuffer[ViewerMemento]()
	var historyPos = 0
	val interpF = Ober.onEDTFuture {
		val i = new Interpreter(new Settings(str => Ober.interpError = str));
		i.beQuietDuring(i.bind("result", "Array[Any]", interpResult))
		Ober.exec("import tc.ober.ViewerImports._", null, i)
		myInterp = i
	}
	val interpResult = new Array[Any](1)
	var myInterp: Interpreter = null
	def interp = {
		if (myInterp == null) {
			println("Waiting for interpreter to initialize")
			interpF()
			println("Interpreter initialized")
		}
		myInterp
	}
	def init {
		Ober.oberNs.parents = List(Ober.classNs, Ober.systemNs)
		Ober.trackNs.parents = List(Ober.oberNs)
		Ober.fileNs.parents = List(Ober.trackNs)
		add(
			Ober.oberNs,
			Ober.trackNs,
			Ober.systemNs,
			Ober.fileNs,
			Ober.classNs
		)
		Ober.oberNs.handleSurf = true
	}
	def pushHistory(viewer: ScalaViewer[_]): Unit = pushHistory(viewer.memento)
	def pushHistory(memento: ViewerMemento) {
		println("push")
		if (historyPos < history.size) history.reduceToSize(historyPos)
		history + memento
		historyPos += 1
		println("history size: "+history.size+", pos: "+historyPos)
	}
	def back(ctx: SimpleContext) {
		if (historyPos > 0) {
			val backMem = history(historyPos - 1)

			if (historyPos >= history.size - 1) {
				val mem = backMem.viewer.memento

				if (mem != history.last) {
					pushHistory(mem)
					historyPos -= 1
				}
			}
			historyPos -= 1
			history(historyPos).restore
			println("history size: "+history.size+", pos: "+historyPos)
		}
	}
	def forward(ctx: SimpleContext) {
		if (historyPos < history.size - 1) {
			historyPos += 1
			history(historyPos).restore
			println("history size: "+history.size+", pos: "+historyPos)
		}
	}
	def error(str: String) {
		println("Error: "+str)
	}
	def add(spaces: Namespace*) {
		for (ns <- spaces) Ober.namespaces(ns.name) = ns
	}
	def viewerDo(ctx: SimpleContext)(implicit block: (ScalaViewer[_]) => Any) {
		val m = ctx.matcher
		if (m hasNext) {
			val viewerName = m.next
			val viewer = ctx.find(viewerName)

			if (viewer == null) {
				ctx.viewer.errorFromProcess("Couldn't find viewer '"+viewerName+"'")
			} else if (!m.hasNext) {
				block(null)
			} else {
				block(viewer)
			}
		} else {
			ctx.viewer.errorFromProcess("Not given a viewer name")
		}
	}
	def setNamespaces(ctx: SimpleContext) = ctx.viewer.namespaces = ctx.matcher.mkString(" ").trim
	def trackPosition(ctx: SimpleContext) {
		if (ctx.matcher.hasNext) {
			try {
				ctx.viewer.trackPosition(ctx.matcher.next.toInt)
			} catch {
				case _ =>
			}
		}
	}
	def trackWidth(ctx: SimpleContext) {
		if (ctx.matcher.hasNext) {
			try {
				ctx.viewer.trackWidth(ctx.matcher.next.toInt)
			} catch {
				case _ =>
			}
		}
	}
	def viewerPosition(ctx: SimpleContext) {
		if (ctx.matcher.hasNext) {
			try {
				ctx.viewer.viewerPosition(ctx.matcher.next.toInt)
			} catch {
				case _ =>
			}
		}
	}
	def viewerHeight(ctx: SimpleContext) {
		if (ctx.matcher.hasNext) {
			try {
				ctx.viewer.viewerHeight(ctx.matcher.next.toInt)
			} catch {
				case _ =>
			}
		}
	}
	def rename(ctx: SimpleContext) {
		if (ctx.matcher.hasNext) ctx.viewer.name = ctx.matcher.next
	}
	def appendText(ctx: SimpleContext) {
		if (!ctx.matcher.hasNext) {
			ctx.viewer.errorFromProcess("Not given text to insert into '"+ctx.viewer.name+"'")
		} else {
			ctx.viewer.append(ctx.matcher.next)
		}
	}
	def load(ctx: SimpleContext) = ctx.viewer.load()
	def help(ctxViewer: ScalaViewer[_]) {
		val helpDoc = Ober.condensePath(classOf[OberWindow].getResource("help.html").toExternalForm)

		findOrCreateViewer(helpDoc, ctxViewer, false).getHtml()
	}
	def findOrCreateViewer(name: String, ctxViewer: ScalaViewer[_], recordHistory: Boolean) = {
		var viewer = ctxViewer.topViewer.find(name)

		if (viewer == null) {
			viewer = ctxViewer.createNewViewer
			viewer.name = name
			viewer
		} else viewer
	}
	def run(ctx: SimpleContext) {
		if (ctx.matcher.hasNext) {
			val name = ctx.matcher.next
			var viewer = ctx.find(name)

			if (viewer == null) {
				viewer = ctx.viewer.createNewViewer
				viewer.name = Ober.condensePath(name)
			}
			if (ctx.matcher.hasNext) {
				ctx.viewer = viewer.asInstanceOf[ScalaViewer[_ <: ScalaViewer[_]]]
				ctx.word = ctx.matcher.next
				viewer.run(ctx)
			}
		}
	}
	/**
	 * execute the command that exists in the first available namespace in the list
	 */
	def handlerFor(cmd: String, ctx: SimpleContext): Any = {
		var found = false
		var seen = MSet[Namespace]()

//		println("Namespaces: "+namespaceSeq.map(namespaces.get(_)).flatMap(x=>x).flatMap(_.fullPath))
		Ober.currentContext = ctx
		for (space <- ctx.namespaceList.map(Ober.namespaces.get(_)).flatMap(x=>x).flatMap(_.fullPath)) {
			if (!found && !seen(space)) {
				seen.add(space)
				space.handlerFor(ctx).foreach {handler =>
					found = true
					handler.apply(ctx)
				}
			}
		}
		if (!found) {
			println("No command '"+cmd+"' found")
		}
	}
	def execCmd(ctx: SimpleContext) = Ober.exec(ctx.matcher.toSequence.mkString(" "), ctx.viewer, interp)
	def surfFile(ctx: SimpleContext) = {
		val (exists, name) = Ober.path(ctx.viewer.name, ctx.word)

		if (exists) Some((ctx: SimpleContext, viewer: ScalaViewer[_]) => viewer.surfTo(name))
		else None
	}
	def loadScript(ctx: SimpleContext) {
		val script = if (ctx.matcher.hasNext) ctx.matcher.next else ""
		val (exists, name) = Ober.path(ctx.viewer.name, script)

		if (exists) {
//			interp.compileSources(new BatchSourceFile(AbstractFile.getFile(name)))
			Ober.exec(Source.fromPath(name).mkString, ctx.viewer, interp)
		} else {
			ctx.error(name + " does not exist")
		}
	}
}
trait Namespace {
	var parents: List[Namespace] = Nil
	def name: String

	def handlerFor(ctx: SimpleContext): Option[Ober.Cmd]
	def fullPath: List[Namespace] = fullPath(Nil)
	def fullPath(soFar: List[Namespace]): List[Namespace] = this :: parents.foldRight(soFar)((space, res) => space.fullPath(res))
	override def toString() = {
		"Namespace "+name
	}
//	def surf(ctx: SimpleContext) = false
	val surf: (SimpleContext) => Option[(SimpleContext, ScalaViewer[_]) => Any] = _ => None
}
trait OberCommand {
	def runCommand(ctx: SimpleContext)

	def install(ctx: SimpleContext) {
		val companion = Class.forName(getClass.getName.dropRight(1))

		if (!Utils.initialized(companion)) {
			val name = commandName

			Utils.initialized.add(companion)
			name.lastIndexOf('.') match {
			case -1 => ctx.error("")
			case pos =>
				val ns = Ober.namespaces(name.take(pos))
			
				if (ns.isInstanceOf[ClosureNamespace]) {
					ns.asInstanceOf[ClosureNamespace].closures(name.drop(pos + 1)) = {ctx: SimpleContext => runCommand(ctx)}
				}
			}
		}
	}
	//You can either use a @CommandName annotation or override this.  Salt to taste.
	def commandName: String = {
		getClass().getAnnotations() foreach {ann => ann match {
			case ann: CommandName => return ann.value
			case _ =>
		}}
		throw new Exception("No CommandName annotation")
	}
}
class ClosureNamespace(var name: String, override val surf: (SimpleContext) => Option[(SimpleContext, ScalaViewer[_]) => Any], entries: (String, Ober.Cmd)*) extends Namespace {
	val closures = MMap[String, Ober.Cmd](entries: _*)
	var handleSurf = false

	def handlerFor(ctx: SimpleContext) = closures.get(ctx.word)
//	override def surf(ctx: SimpleContext) = {
//		if (handleSurf) {
//			val (exists, name) = Ober.path(ctx.viewer.name, ctx.word)
//			
//			if (exists) {
//				val viewer = viewerForSurf(name, ctx)
//				viewer.surfTo(name)
//				viewer.focus
//				true
//			} else false
//		} else false
//	}
}
class ClassNamespace(var name: String) extends Namespace {
	def handlerFor(ctx: SimpleContext): Option[Ober.Cmd] = {
		var cl: java.lang.Class[_] = null
		var method: java.lang.reflect.Method = null
		var nm: String = null

		if (ctx.word == "Import") Some(addImport(_))
		else {
			for (i <- "" :: getImports(ctx).toList; if (method == null)) {
				nm = i + ctx.word
				try {
					cl = safeClass(nm);
					method = cl.getMethod("main", classOf[Array[String]]);
				} catch {case _ =>}
			}
			if (cl == null) {
				None
			} else {
				Some({ctx: SimpleContext => run(cl, method, ctx.matcher.toSequence.toArray[String])})
			}
		}
	}
	def safeClass(name: String) = {
		try {
			Class.forName(name)
		} catch {
			case _ => null
		}
	}
	def run(cl: Class[_], method: java.lang.reflect.Method, args: Array[String]) {
		if (!Utils.initialized(cl)) {
			val companion = safeClass(cl.getName + "$")

			if (companion != null) {
				try {
					val mod = companion.getDeclaredField("MODULE$").get(null)
					
					if (mod.isInstanceOf[OberCommand]) {
						mod.asInstanceOf[OberCommand].install(Ober.currentContext)
					}
				} catch {
				case ex => ex.printStackTrace()
				}
			}
			//do this down here in case the command didn't initialize
			Utils.initialized.add(cl)
		}
		if (method != null) {
			method.invoke(null, Array[Object](args): _*)
		}
	}
	def getImports(ctx: SimpleContext) = {
		ctx.viewer.properties.getOrElseUpdate('imports, MSet[String]()).asInstanceOf[MSet[String]]
	}
	def addImport(ctx: SimpleContext) = if (ctx.matcher.hasNext) getImports(ctx).add(ctx.matcher.next + ".")
}
class SystemNamespace(var name: String) extends Namespace {
	def handlerFor(ctx: SimpleContext): Option[Ober.Cmd] = {
		Some({ctx: SimpleContext =>
			try {
				val seq = ctx.matcher.toSequence
				val end = if (seq.isEmpty) ctx.wEnd else ctx.matcher.end + ctx.wEnd
//				println("start: "+ctx.wStart+", len: "+(end - ctx.wStart))
				val process = Runtime.getRuntime.exec(ctx.text(ctx.wStart, end - ctx.wStart))
				val output = process.getInputStream
				val error = process.getErrorStream

				new Thread("processInput") {
					override def run {
						Source.fromInputStream(output).getLines() foreach {line =>
							ctx.viewer.outputFromProcess(line)
						}
					}
				}.start
				new Thread("processErorr") {
					override def run {
						Source.fromInputStream(error).getLines() foreach {line => println(line); ctx.viewer.errorFromProcess(line)}
					}
				}.start
				process
			} catch {case e: IOException =>if (!Ober.surf(ctx)) {ctx.viewer.errorFromProcess("Unknown command: " + ctx.word + "\n")}}})
	}
}
