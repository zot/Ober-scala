/*
(C) 2009-2010 Bill Burdick

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

import java.io.ByteArrayInputStream
import java.io.File
import scala.collection.mutable.{HashMap => MMap, HashSet => MSet, StringBuilder, ArrayBuffer};
import scala.io.Source;
import java.io.IOException;
import scala.tools.nsc.{Interpreter, Settings, InterpreterResults => IR}

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
	def get(ctx: SimpleContext) {
		if (!ctx.userCmd && ctx.matcher.hasNext) {
			val filename = ctx.matcher.next

			try {
				ctx.viewer.get(Source.fromInputStream(new File(filename).toURL.openStream))
			} catch {
				case _ => ctx.viewer.errorFromProcess("Could not load file: " + filename)
			}
		} else {
			ctx.viewer.get()
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
	def cmdSurf(ctx: SimpleContext) {
		try {
			val seq = ctx.matcher.toSeq
			val end = if (seq.isEmpty) ctx.wEnd else ctx.matcher.end + ctx.wEnd
			val process = Runtime.getRuntime.exec(ctx.word)
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
		} catch {case e: IOException =>if (!Ober.surf(ctx)) {ctx.viewer.errorFromProcess("Unknown command: " + ctx.word + "\n")}}
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
	def load(ctx: SimpleContext) = ctx.viewer.load(if (!ctx.userCmd && ctx.matcher.hasNext) ctx.matcher.next else null)
	def control(ctx: SimpleContext, cmd: Array[String]) {
		val name = ctx.viewer.name

		try {
//			println("start: "+ctx.wStart+", len: "+(end - ctx.wStart))
			val process = Runtime.getRuntime.exec(cmd)
			val output = process.getInputStream
			val error = process.getErrorStream
			var text: String = null
			var docStream = new ByteArrayInputStream(ctx.viewer.asInstanceOf[DocumentViewer].getText.getBytes)

			new Thread("processErorr") {
				override def run {
					Source.fromInputStream(error).getLines() foreach {line => println(line); ctx.viewer.errorFromProcess(line)}
				}
			}.start
			new Thread("outputContents") {
				override def run {
					var remaining = Integer.MAX_VALUE
					var totalBytes = 0
					var out = process.getOutputStream
					var count = 0
					var buffer = new Array[Byte](2048)
					
					while (count != -1 && remaining > 0) {
						count = docStream.read(buffer, 0, Math.min(buffer.length, remaining));
						if (count > 0) {
							out.write(buffer, 0, count);
							remaining -= count;
							totalBytes += count;
						}
					}
					out.close
				}
			}.start
			try {
				text = Source.fromInputStream(output).mkString
			} catch {case e: IOException => ctx.viewer.errorFromProcess("Unknown command: " + name + "\n")}
			process.waitFor
			if (process.exitValue != 0) {
				ctx.viewer.errorFromProcess("Error running command: " + name + "\n")
			} else {
				ctx.viewer.asInstanceOf[SimpleViewer[_,_]].eval(text)
//				ctx.viewer.eval(text)
			}
			process
		} catch {case e: IOException => ctx.viewer.errorFromProcess("Unknown command: " + name + "\n")}
	}
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
		for (space <- ctx.namespaceList.map(name => Some(Ober.namespaces.getOrElse(name, new AdhocNamespace(name)))).flatMap(x=>x).flatMap(_.fullPath)) {
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
	def execCmd(ctx: SimpleContext) = Ober.exec(ctx.matcher.toSeq.mkString(" "), ctx.viewer, interp)
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
			Ober.exec(Source.fromFile(name).mkString, ctx.viewer, interp)
		} else {
			ctx.error(name + " does not exist")
		}
	}
}
