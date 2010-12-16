/*
(C) 2009-2010 Bill Burdick

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
import scala.collection.Seq
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
	var userCmd = true
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
	def namespaceList = viewer.namespaces.getOrElse("File").split(" +")
	def namespaces = namespaceList.map(name => Some(Ober.namespaces.getOrElse(name, new AdhocNamespace(name)))).flatMap(_.get.fullPath).reverse.distinct.reverse
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
	val systemNs = new SystemNamespace("System", Nil)
	val classNs = new ClassNamespace("Class", List(systemNs))
	val oberNs = new ClosureNamespace("Ober", List(classNs), noSurf,
		"Quit" -> {_ => System.exit(0)},
		"New" -> (_.viewer.createNewViewer),
		"Newcol" -> (_.viewer.createNewTrack),
		"Load" -> (Utils.load(_)),
		"Control" -> {ctx =>
			for {
				file <- Some(ctx.viewer.name)
				cmd <- ctx.matcher.nextOpt
				subcmd <- ctx.matcher.nextOpt
			} {
				Utils.control(ctx, Array(cmd, subcmd, file))
			}
		},
		"Echo" -> (ctx => ctx.error(ctx.matcher.toSeq.mkString(" "))),
		"Help" -> (ctx => Utils.help(ctx.viewer)),
		"Rename" -> (Utils.rename(_)),
		"Run" -> {ctx => ctx.userCmd = false; Utils.run(ctx)},
		"Append" -> (Utils.appendText(_)),
		"Exec" -> (Utils.execCmd(_)),
		"Namespaces" -> (Utils.setNamespaces(_)),
		"Back" -> (Utils.back(_)),
		"Forward" -> (Utils.forward(_)),
		"Clean" -> (_.viewer.dirty_=(false)),
		"LoadScript" -> (Utils.loadScript(_)),
		"Del" -> (_.viewer.delete),
		"Tag" -> (ctx => ctx.viewer.asInstanceOf[SimpleViewer[_,_]].tagText = ctx.matcher.next)
	)
	val trackNs = new ClosureNamespace("Track", List(oberNs), noSurf,
		"Delcol" -> {ctx =>
			val v = if (ctx.viewer.isTrack) ctx.viewer else ctx.viewer.parent
			if (v.isTrack) v.delete},
		"Width" -> (Utils.trackWidth(_)),
		"TrackPosition" -> (Utils.trackPosition(_))
	)
	val fileNs = new ClosureNamespace("File", List(oberNs), Utils.surfFile,
		"Height" -> (Utils.viewerHeight(_)),
		"ViewerPosition" -> (Utils.viewerPosition(_)),
//		"Get" -> (_.viewer.get()),
		"Get" -> (Utils.get(_)),
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
	def resolve(str: String) = ArgMatcher(str).toSeq.mkString("")
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
