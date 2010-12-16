/*
(C) 2009-2010 Bill Burdick

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

import scala.collection.mutable.{HashMap => MMap, HashSet => MSet, StringBuilder, ArrayBuffer};
import scala.io.Source;
import java.io.IOException;
import scala.actors.Actor._

trait Namespace {
	val parents: List[Namespace] = Nil
	def name: String

	def handlerFor(ctx: SimpleContext): Option[Ober.Cmd]
	def fullPath: List[Namespace] = fullPath(Nil)
	def fullPath(soFar: List[Namespace]): List[Namespace] = this :: parents.foldRight(soFar)((space, res) => space.fullPath(res))
	override def toString() = "Namespace "+name
//	def surf(ctx: SimpleContext) = false
	val surf: (SimpleContext) => Option[(SimpleContext, ScalaViewer[_]) => Any] = _ => None
}

class AdhocNamespace(var name: String) extends Namespace {
	override val parents = List(Ober.fileNs)
	def handlerFor(ctx: SimpleContext) = {println("tag text: "+ctx.viewer.asInstanceOf[SimpleViewer[_,_]].tagText); if (Ober.fileNs.handlerFor(ctx).orElse(Ober.oberNs.handlerFor(ctx)) != None) None else Some((ctx: SimpleContext) => Utils.control(ctx, Array(name, ctx.word, ctx.viewer.name)))}
}

class ClosureNamespace(var name: String, override val parents: List[Namespace], override val surf: (SimpleContext) => Option[(SimpleContext, ScalaViewer[_]) => Any], entries: (String, Ober.Cmd)*) extends Namespace {
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
class ClassNamespace(var name: String, override val parents: List[Namespace]) extends Namespace {
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
					if (cl != null) {
						method = cl.getMethod("main", classOf[Array[String]]);
					}
				} catch {case _ =>}
			}
			if (cl == null) {
				None
			} else {
				Some({ctx: SimpleContext => run(cl, method, ctx.matcher.toSeq.toArray[String])})
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
class SystemNamespace(var name: String, override val parents: List[Namespace]) extends Namespace {
	def handlerFor(ctx: SimpleContext): Option[Ober.Cmd] = {
		Some({ctx: SimpleContext =>
			try {
				val seq = ctx.matcher.toSeq
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
