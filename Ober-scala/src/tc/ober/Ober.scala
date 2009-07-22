/*
(C) 2009 Bill Burdick

ar.ober.OberDragWidget

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

import scala.collection.mutable.{HashMap => MMap, HashSet => MSet};
import scala.collection.Sequence;
import javax.swing.border.LineBorder;
import javax.swing.text.JTextComponent;
import java.awt.Color;
import java.awt.Container;
import scala.io.Source;
import java.io.IOException;

trait AbstractOberWindow

trait ScalaViewer[T <: ScalaViewer[_]] extends AbstractViewer[T] {
	def gainedFocus: Unit
	def lostFocus: Unit
	def properties: MMap[Any, Any]
	def errorFromProcess(line: String)
	def outputFromProcess(line: String)
	def delete: Unit
	def isTrack: Boolean
	def parent: T
	def createNewViewer: Unit
	def createNewTrack: Unit
}


class SimpleContext(val comp: JTextComponent, val viewer: ScalaViewer[_ <: ScalaViewer[_]], val word: String, val args: String, wStart: Int, wEnd: Int, lStart: Int, lEnd: Int)

object Ober {
	type Cmd = SimpleContext => Any
	val wordPattern = """([-a-zA-Z0-9_<>|!.:$/]|\\\[)+|\[""" r
	val namePattern = """^[^:]*: *(([-a-zA-Z0-9_<>|!.:$/]|\\\[)+)( |$)""" r
	val defaultOberTagText = "Ober: New, Newcol, Quit"
	val defaultTrackTagText = "Track: New, Delcol"
	val defaultViewerTagText = "File: $PWD/New Del"
	val defaultErrorViewerTagText = "Viewer: Err Del"
	val namespaces = MMap[String, Namespace]()
	var windows = List[AbstractOberWindow]()
	var focus: ScalaViewer[_] = null
	val redBorder = new LineBorder(Color.red)
	val grayBorder = LineBorder.createGrayLineBorder
	val blackBorder = LineBorder.createBlackLineBorder
	var idCounter = 0
	val classNs = new ClassNamespace("Class")
	val systemNs = new SystemNamespace("System")
	val oberNs = new ClosureNamespace("Ober",
		"Quit" -> {_ => System.exit(0)},
		"Import" -> (classNs.addImport(_)),
		"New" -> (_.viewer.createNewViewer),
		"Del" -> (_.viewer.delete),
		"Delcol" -> {ctx =>
			val v = if (ctx.viewer.isTrack) ctx.viewer else ctx.viewer.parent
			if (v.isTrack) v.delete},
		"Newcol" -> (_.viewer.createNewTrack)
	)
	val trackNs = new ClosureNamespace("Track")
	val viewerNs = new ClosureNamespace("Viewer")
	val fileNs = new ClosureNamespace("File")

	init
	def init {
		oberNs.parents = List(classNs, systemNs)
		trackNs.parents = List(oberNs)
		viewerNs.parents = List(trackNs)
		fileNs.parents = List(viewerNs)
		add(
			oberNs,
			trackNs,
			viewerNs,
			systemNs,
			fileNs,
			classNs
		)
	}

	def nextId = {
		idCounter += 1
		"ID: " + idCounter
	}
	def gainFocus(v: ScalaViewer[_]) {
		if (focus != v) {
			if (focus != null) focus.lostFocus
			focus = v
			focus gainedFocus
		}
	}
	def add(spaces: Namespace*) {
		for (ns <- spaces) namespaces(ns.name) = ns
	}
	/**
	 * execute the command that exists in the first available namespace in the list
	 * 
	 * it uses 'foreach' because handlerFor returns an Option
	 */
	def handlerFor(namespaceSeq: Sequence[String], cmd: String, ctx: SimpleContext): Any = {
		var found = false
		var seen = MSet[Namespace]()

//		println("Namespaces: "+namespaceSeq.map(namespaces.get(_)).flatMap(x=>x).flatMap(_.fullPath))
		for (space <- namespaceSeq.map(namespaces.get(_)).flatMap(x=>x).flatMap(_.fullPath)) {
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
}
class ClosureNamespace(var name: String, entries: (String, Ober.Cmd)*) extends Namespace {
	val closures = MMap[String, Ober.Cmd](entries: _*)
	def handlerFor(ctx: SimpleContext) = {
		closures.get(ctx.word)
	}
}
class ClassNamespace(var name: String) extends Namespace {
	def handlerFor(ctx: SimpleContext): Option[Ober.Cmd] = {
		var method: java.lang.reflect.Method = null
		var nm: String = null

		for (i <- "" :: getImports(ctx).toList; if (method == null)) {
			nm = i + ctx.word
			try {
				method = Class.forName(nm).getMethod("main", classOf[Array[String]]);
			} catch {case _ =>}
		}
		if (method == null) {
			None
		} else
			Some({ctx: SimpleContext => method.invoke(null, Array[Object](ctx.args.trim.split("  *")): _*)})
	}
	def getImports(ctx: SimpleContext) = {
		ctx.viewer.properties.getOrElseUpdate('imports, MSet[String]()).asInstanceOf[MSet[String]]
	}
	def addImport(ctx: SimpleContext) {
		val args = ctx.args.trim.split("  *")

		if (args.length > 0) {
			getImports(ctx).add(args(0) + ".")
		}
	}
}
class SystemNamespace(var name: String) extends Namespace {
	def handlerFor(ctx: SimpleContext): Option[Ober.Cmd] = {
		Some({ctx: SimpleContext =>
			try {
				val process = Runtime.getRuntime.exec(ctx.word + ctx.args)
				val output = process.getInputStream
				val error = process.getErrorStream

				new Thread("processInput") {
					override def run {
						Source.fromInputStream(output).getLines foreach {line =>
						ctx.viewer.outputFromProcess(line)
						}
					}
				}.start
				new Thread("processErorr") {
					override def run {
						Source.fromInputStream(error).getLines foreach {line =>
						ctx.viewer.errorFromProcess(line)
						}
					}
				}.start
				process
			} catch {case e: IOException => ctx.viewer.errorFromProcess("Unknown command: " + ctx.word + "\n")}
		})
	}
}
