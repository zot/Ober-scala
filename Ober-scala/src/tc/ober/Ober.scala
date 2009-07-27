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
import scala.collection.mutable.{HashMap => MMap, HashSet => MSet, StringBuilder};
import scala.collection.Sequence;
import scala.util.matching.Regex;
import scala.tools.nsc.{Interpreter, Settings, InterpreterResults => IR}

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
	def load
	def get(src: Source = null)
	def getHtml(src: Source = null, context: Any = null)
	def put
	def defaultComponent: JTextComponent
	def filename: java.io.File
	def run(ctx: SimpleContext)
	def surf
	def surf(ctx: SimpleContext)
	def surfTo(newName: String)
	def topViewer: OberViewer
	def trackPosition(x: Int)
	def trackWidth(w: Int)
	def viewerPosition(y: Int)
	def viewerHeight(h: Int)
	def namespaces: Option[String]
	def namespaces_=(str: String)
}

class SimpleContext(val comp: JTextComponent, var viewer: ScalaViewer[_ <: ScalaViewer[_]], var word: String, val wStart: Int, val wEnd: Int, var myMatcher: ArgMatcher = null) {
	def matcher = {
		if (myMatcher == null) {
			val doc = comp.getDocument
			myMatcher = new ArgMatcher(doc.getText(wEnd, doc.getLength - wEnd), true, viewer)
		}
		myMatcher
	}
	def error(str: String) = viewer.errorFromProcess(str)
	def find(name: String) = viewer.topViewer.find(name)
}

object Ober {
	type Cmd = SimpleContext => Any

	var windows = List[AbstractOberWindow]()
	var focus: ScalaViewer[_] = null
	var idCounter = 0

	val wordPlusRest = """^([-a-zA-Z0-9_<>|!.:$/]+) *([^ \n](?:[^\n"]|"(?:[^"]|\n|\\")*")*)?(?:\n(?:.|\n)*)?$""".r
	val wordPattern = """[-a-zA-Z0-9_<>|!.:$/]+""" r
	val namespacePattern = """([-a-zA-Z0-9_<>|!.:$/{}]*) *\[(([-a-zA-Z0-9_<>|!.:$/{}]+)?( +[-a-zA-Z0-9_<>|!.:$/{}]+)*)]""" r
	val namePattern = """^[^:]*: *([-a-zA-Z0-9_<>|!.:$/{}]+)([^-a-zA-Z0-9_<>|!.:$/{}]|$)""" r
	val defaultOberTagText = "[Ober] New Newcol Help Quit"
	val defaultTrackTagText = "[Track] New Delcol"
	val defaultViewerTagText = "./New [File] Get GetHTML Put Del"
	def defaultErrorViewerTagText = "Err [Viewer] Del"
	val namespaces = MMap[String, Namespace]()
	val redBorder = new LineBorder(Color.red)
	val grayBorder = LineBorder.createGrayLineBorder
	val blackBorder = LineBorder.createBlackLineBorder
	val interp = new Interpreter(new Settings(str => interpError))
	val interpResult = new Array[Any](1)
	var interpError = ""
	val classNs = new ClassNamespace("Class")
	val systemNs = new SystemNamespace("System")
	val oberNs = new ClosureNamespace("Ober",
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
		"Namespaces" -> (Utils.setNamespaces(_))
	)
	val trackNs = new ClosureNamespace("Track",
		"Delcol" -> {ctx =>
			val v = if (ctx.viewer.isTrack) ctx.viewer else ctx.viewer.parent
			if (v.isTrack) v.delete},
		"Width" -> (Utils.trackWidth(_)),
		"TrackPosition" -> (Utils.trackPosition(_))
	)
	val viewerNs = new ClosureNamespace("Viewer",
		"Del" -> (_.viewer.delete),
		"Height" -> (Utils.viewerHeight(_)),
		"ViewerPosition" -> (Utils.viewerPosition(_))
	)
	val fileNs = new ClosureNamespace("File",
		"Get" -> (_.viewer.get()),
		"GetHTML" -> (_.viewer.getHtml()),
		"Surf" -> (_.viewer.surf),
		"Put" -> (_.viewer.put)
	)

	def toFile(obj: Any): File = {
		obj match {
			case s: String => new File(s)
			case f: java.io.File => f
			case other => new File(other.toString)
		}
	}
	def run(namespaces: Array[String], ctx: SimpleContext) {
		Utils.handlerFor(namespaces, ctx.word, ctx)
	}
	def surf(namespaces: Array[String], ctx: SimpleContext) = Utils.surf(namespaces, ctx)
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
		var u = new URI(path)

		if (u.getPath == "") {
			u = new URI(path + "/")
		}
		if (u.getScheme == "file") u.toString.drop("file:".length)
		else u.toString
	}
	def contextForName(name: String) = {
		Ober.tryUri(resolve(name)) match {
		case Some(uri) if (uri.getScheme == null || uri.getScheme == "file") => new java.io.File(uri.getPath)
		case Some(uri) => uri.toURL
		case None => null
		}
	}
	def resolve(str: String) = ArgMatcher(str).toSequence.mkString("")

	Utils.init
}
object ViewerImports {
	def OBER = Ober
	def HOME = System.getProperty("user.home")
}
object Utils {
	def init {
		Ober.interp.beQuietDuring(Ober.interp.bind("result", "Array[Any]", Ober.interpResult))
		Utils.exec("import tc.ober.ViewerImports._", null)
		Ober.oberNs.parents = List(Ober.classNs, Ober.systemNs)
		Ober.trackNs.parents = List(Ober.oberNs)
		Ober.viewerNs.parents = List(Ober.trackNs)
		Ober.fileNs.parents = List(Ober.viewerNs)
		add(
			Ober.oberNs,
			Ober.trackNs,
			Ober.viewerNs,
			Ober.systemNs,
			Ober.fileNs,
			Ober.classNs
		)
		Ober.oberNs.handleSurf = true
	}
	def error(str: String) {
		println("Error: "+str)
	}
	def gainFocus(v: ScalaViewer[_]) {
		if (Ober.focus != v) {
			if (Ober.focus != null) Ober.focus.lostFocus
			Ober.focus = v
			Ober.focus gainedFocus
		}
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
	def load(ctx: SimpleContext) = ctx.viewer.load
	def help(ctxViewer: ScalaViewer[_]) {
		val helpDoc = Ober.condensePath(classOf[OberWindow].getResource("help.html").toExternalForm)

		findOrCreateViewer(helpDoc, ctxViewer).getHtml()
	}
	def findOrCreateViewer(name: String, ctxViewer: ScalaViewer[_]) = {
		var viewer = ctxViewer.topViewer.find(name)

		if (viewer == null) {
			viewer = ctxViewer.createNewViewer
			viewer.name = name
		}
		viewer
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
	def handlerFor(namespaceSeq: Sequence[String], cmd: String, ctx: SimpleContext): Any = {
		var found = false
		var seen = MSet[Namespace]()

//		println("Namespaces: "+namespaceSeq.map(namespaces.get(_)).flatMap(x=>x).flatMap(_.fullPath))
		for (space <- namespaceSeq.map(Ober.namespaces.get(_)).flatMap(x=>x).flatMap(_.fullPath)) {
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
	def surf(namespaceSeq: Sequence[String], ctx: SimpleContext): Boolean = {
		var found = false
		var seen = MSet[Namespace]()

//		println("Namespaces: "+namespaceSeq.map(namespaces.get(_)).flatMap(x=>x).flatMap(_.fullPath))
		for (space <- namespaceSeq.map(Ober.namespaces.get(_)).flatMap(x=>x).flatMap(_.fullPath)) {
			if (!found && !seen(space)) {
				seen.add(space)
				found = space.surf(ctx)
			}
		}
		if (!found) {
			println("No handler could surfe to '"+ctx.word+"'.")
		}
		found
	}
	def execCmd(ctx: SimpleContext) = exec(ctx.matcher.toSequence.mkString(" "), ctx.viewer)
	def eval(str: String, viewer: ScalaViewer[_]) = exec("result(0) = {"+str+";}", viewer)
	def exec(str: String, viewer: ScalaViewer[_]) = {
		Ober.interpError = ""
		Ober.interp.beQuietDuring(Ober.interp.interpret(str)) match {
			case IR.Success => Some(Ober.interpResult(0))
			case IR.Error | IR.Incomplete =>
				if (viewer != null) viewer.errorFromProcess(Ober.interpError)
				else println(Ober.interpError)
				None
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
	def surf(ctx: SimpleContext) = false
}
class ClosureNamespace(var name: String, entries: (String, Ober.Cmd)*) extends Namespace {
	val closures = MMap[String, Ober.Cmd](entries: _*)
	var handleSurf = false

	def handlerFor(ctx: SimpleContext) = closures.get(ctx.word)
	override def surf(ctx: SimpleContext) = {
		if (handleSurf) {
			val name = Ober.condensePath(ctx.word)
			var viewer = ctx.find(name)

			if (viewer == null) {
				viewer = ctx.viewer.createNewViewer
				viewer.surfTo(Ober.condensePath(name))
			}
			viewer.focus
			true
		} else false
	}
}
class ClassNamespace(var name: String) extends Namespace {
	def handlerFor(ctx: SimpleContext): Option[Ober.Cmd] = {
		var method: java.lang.reflect.Method = null
		var nm: String = null

		if (ctx.word == "Import") Some(addImport(_))
		else {
			for (i <- "" :: getImports(ctx).toList; if (method == null)) {
				nm = i + ctx.word
				try {
					method = Class.forName(nm).getMethod("main", classOf[Array[String]]);
				} catch {case _ =>}
			}
			if (method == null) {
				None
			} else {
				Some({ctx: SimpleContext => run(method, ctx.matcher.toSequence.toArray[String])})
			}
		}
	}
	def run(method: java.lang.reflect.Method, args: Array[String]) {
		method.invoke(null, Array[Object](args): _*)
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
				val process = Runtime.getRuntime.exec(ctx.comp.getDocument.getText(ctx.wStart, end - ctx.wStart))
				val output = process.getInputStream
				val error = process.getErrorStream

				new Thread("processInput") {
					override def run {
						Source.fromInputStream(output).getLines foreach {line =>
							ctx.viewer.outputFromProcess(line.dropRight(1))
						}
					}
				}.start
				new Thread("processErorr") {
					override def run {
						Source.fromInputStream(error).getLines foreach {line =>
							ctx.viewer.errorFromProcess(line.dropRight(1))
						}
					}
				}.start
				process
			} catch {case e: IOException => ctx.viewer.errorFromProcess("Unknown command: " + ctx.word + "\n")}
		})
	}
}
