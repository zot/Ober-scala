package tc.ober

import scala.collection.mutable.{HashMap => MMap, HashSet => MSet};
import scala.collection.Sequence;
import javax.swing.border.LineBorder;
import java.awt.Color;
import java.awt.Container;
import scala.io.Source;
import java.io.IOException;

object Ober {
	type AnyViewer = SimpleViewer[_ <: Container]
	type AnyLeafViewer = LeafViewer[_ <: Container]
	type Cmd = SimpleContext => Any
	val wordPattern = """([-a-zA-Z0-9_<>|!.:$/]|\\\[)+|\[""" r
	val namePattern = """^[^:]*: *([-a-zA-Z0-9_<>|!.:$/]|\\\[)+( |$)""" r
	val defaultOberTagText = "Ober: New, Newcol, Quit, Help"
	val defaultTrackTagText = "Track: New, Delcol"
	val defaultViewerTagText = "File: $PWD/New Del"
	val defaultErrorViewerTagText = "Viewer: Err Del"
	val namespaces = MMap[String, Namespace]()
	var windows = List[OberWindow]()
	var focus: SimpleViewer[_] = null
	val redBorder = new LineBorder(Color.red)
	val grayBorder = LineBorder.createGrayLineBorder
	val blackBorder = LineBorder.createBlackLineBorder

	val classNs = new ClassNamespace("Class")
	val systemNs = new SystemNamespace("System")
	val oberNs = new ClosureNamespace("Ober",
		"Quit" -> {ctx => System.exit(0)},
		"Import" -> {ctx => classNs.addImport(ctx)}
	)
	oberNs.parents = List(classNs, systemNs)
	val trackNs = new ClosureNamespace("Track")
	trackNs.parents = List(oberNs)
	val viewerNs = new ClosureNamespace("Viewer")
	viewerNs.parents = List(trackNs)
	val fileNs = new ClosureNamespace("File")
	fileNs.parents = List(viewerNs)
	add(
		oberNs,
		trackNs,
		viewerNs,
		systemNs,
		fileNs,
		classNs
	)

	def gainFocus(v: SimpleViewer[_]) {
		if (focus != v) {
			if (focus != null) focus.lostFocus
			focus = v
			focus gainedFocus
		}
	}
	def add(spaces: Namespace*) {
		for (ns <- spaces) namespaces(ns.name) = ns
	}
	def createWindow {
		windows ::= new OberWindow
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
trait ViewerContext
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
			} catch {case e: IOException => ctx.viewer.errorFromProcess("Unknown command: " + ctx.word)}
		})
	}
}
