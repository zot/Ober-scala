/*
(C) 2009 Bill Burdick

ar.ober.OberDragWidget

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober;

import java.io.File;
import java.io.InputStream;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.Container;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import javax.swing.JTextField;
import javax.swing.JComponent;
import javax.swing.AbstractAction;
import javax.swing.text.Document;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.JTextComponent;
import scala.collection.mutable.{HashMap => MMap};
import scala.io.Source;
import Ober._;

object SimpleViewer {
	type AnyViewer = SimpleViewer[_ <: Container, _ <: SimpleViewer[_, _]]
}
import SimpleViewer._
abstract class SimpleViewer[PANEL_TYPE <: Container, PARENT <: AnyViewer] extends ScalaViewer[PARENT] {
	val tag = new JTextField
	val properties = MMap[Any, Any]()
	var viewerPanel: PANEL_TYPE = null.asInstanceOf[PANEL_TYPE]
	var children = List[AnyViewer]()
	var myParent: PARENT = null.asInstanceOf[PARENT]
	var myDirty = false

	def dirty = myDirty
	def dirty_=(dirt: Boolean): Unit = {myDirty = dirt}
	def parent = myParent
	def parent_=(p: PARENT) = myParent = p
	def createNewTrack = newTrack
	def createNewViewer = newViewer
	def defaultComponent: JTextComponent = tag
	def clear {}
	def name = namespacePattern.findFirstMatchIn(tag.getText).map(_.group(1)).getOrElse("")
	def name_=(newName: String) {
		val txt = tag.getText

		for (m <- namespacePattern.findFirstMatchIn(txt)) {
			tag.setText(txt.take(m.start(1)) + newName + txt.drop(m.end(1)))
		}
	}
	def memento: ViewerMemento = null
	def find(childName: String): ScalaViewer[_] = find(_.name == childName)
	def find(pred: (ScalaViewer[_]) => Boolean): ScalaViewer[_] = if (pred(this)) this else subFind(pred, children)
	def subFind(pred: (ScalaViewer[_]) => Boolean, rem: List[ScalaViewer[_]]): ScalaViewer[_] = {
		if (rem == Nil) {
			null
		} else {
			val ret: ScalaViewer[_] = rem.head.find(pred)

			if (ret != null) {
				ret
			} else {
				subFind(pred, rem.tail)
			}
		}
	}
	def resizeDirection = (0, 0)
	def isTrack = false
	def wordAtPosition(comp: JTextComponent, pos: Int) = {
		var doc = comp.getDocument
		var word: Option[SimpleContext] = None
		val (start, end) = doc match {
		case d: DefaultStyledDocument =>
			val para = d getParagraphElement pos
			(para getStartOffset, para getEndOffset)
		case _ => (0, doc.getLength + 1)
		}

		if (start < end - 1) {
			var found = false
			val text = doc.getText(start, end - start)
			val matcher = wordPattern.findAllIn(text)
			val offsetPos = pos - start

			matcher find {_ =>
				if (matcher.start <= offsetPos && offsetPos <= matcher.end) {
					word = Some(new SimpleContext(comp, this, matcher matched, matcher.start + start, matcher.end + start))
					true
				} else false
			}
		}
		word
	}
	def namespaces = namespacePattern.findFirstMatchIn(tag.getText).map(_.group(2))
	def namespaces_=(ns: String) {
		val txt = tag.getText

		namespacePattern.findFirstMatchIn(txt) match {
		case Some(m) =>	tag.setText(txt.take(m.start(2))+ns+txt.drop(m.end(2)))
		case None=>
		}
	}
	def bindEvents(comp: JTextComponent) {
		comp addMouseListener new MouseAdapter {
			override def mouseClicked(e: MouseEvent) {
				if ((e.getButton == 3 && (e.isControlDown || dirty)) || e.getButton == 2) {
					val comp: JTextComponent = e.getSource.asInstanceOf[JTextComponent]

					for (ctx <- wordAtPosition(comp, comp.viewToModel(e.getPoint))) {
						if (comp.modelToView(ctx.wStart).union(comp.modelToView(ctx.wEnd)).contains(e.getPoint)) {
							ctx.surfReplace = false
							run(ctx)
						}
					}
				} else if (e.getButton == 3) {
					val comp: JTextComponent = e.getSource.asInstanceOf[JTextComponent]

					for (ctx <- wordAtPosition(comp, comp.viewToModel(e.getPoint))) {
						if (comp.modelToView(ctx.wStart).union(comp.modelToView(ctx.wEnd)).contains(e.getPoint)) {
							run(ctx)
						}
					}
				}
			}
		}
		comp addFocusListener new FocusAdapter {
			override def focusGained(e: FocusEvent) {
				gainFocus(SimpleViewer.this)
			}
		}
	}
	def run(ctx: SimpleContext) = Ober.run(ctx)
	def surf = surfTo(name)
	def surfTo(newName: String) {
		name = newName
		get(null)
	}
	def layout {
		viewerPanel.invalidate
		topViewer.viewerPanel.validate
	}
	def topViewer: OberViewer = parent.topViewer
	def outputFromProcess(str: String) = errorFromProcess(str)
	def errorFromProcess(str: String) {
		var viewer = topViewer.find{child: ScalaViewer[_] => child.name.startsWith("Err") && child.isInstanceOf[DocumentViewer]}.asInstanceOf[DocumentViewer]

		if (viewer == null) {
			viewer = topViewer.newDocumentViewer
			viewer.tag.setText(defaultErrorViewerTagText)
		}
		viewer.append(str + "\n")
	}
	def append(str: String) {}
	def widestTrack: Option[TrackViewer] = topViewer.widestTrack
	def newTrack = {
		val tr = new TrackViewer
		topViewer.addTrack(tr)
		tr
	}
	def trackForNewViewer = widestTrack getOrElse newTrack
	//Create a document viewer by default
	def newViewer: ScalaViewer[_] = newDocumentViewer
	def newDocumentViewer: DocumentViewer = {
		val viewer = new DocumentViewer()
		viewer.name = name
		trackForNewViewer.addViewer(viewer)
		viewer
	}
	def invalidate {
		viewerPanel.invalidate
		for (child <- children) {
			child.invalidate
		}
	}
	def delete {
		val p = viewerPanel.getParent
		p.remove(viewerPanel)
		parent.removeChild(this)
		layout
		p.invalidate
		p.layout
		p.asInstanceOf[JComponent].repaint()
	}
	def removeChild(child: AnyViewer) {
		children = children - child
	}
	def trackPosition(x: Int) {}
	def trackWidth(w: Int) {}
	def viewerPosition(y: Int) {}
	def viewerHeight(h: Int) {}
	def load(str: String = null) {
		Ober.contextForName(if (str == null) name else str) match {
		case f: java.io.File => eval(Source.fromFile(f).mkString)
		case u: java.net.URL => eval(Source.fromURL(u).mkString)
		case _ => errorFromProcess("Couldn't resolve name: "+name)
		}
	}
	def eval(txt: String) {
		var pos = 0

		while (pos < txt.length) {
			val m = ArgMatcher(txt.drop(pos), true, this)

			if (m.hasNext) {
				val cmd = m.next

				Ober.run(new SimpleContext(defaultComponent, this, cmd, m.start + pos, m.end + pos, m, txt))
				pos = if (m.end == -1) txt.length else pos + m.end
			} else {
				pos += 1
			}
		}
	}
	def read = {
		tryUri(name) match {
		case None => readFromFile
		case Some(uri) if (uri.getScheme == null || uri.getScheme == "file") => readFromFile
		case Some(uri) => Some(Source.fromInputStream(uri.toURL.openStream).mkString(""))
		case _ => None
		}
	}
	def focus = defaultComponent.grabFocus
	def nameForFileSort(f: File): String = {
		val n = f.getName.toLowerCase

		if (n.startsWith(".")) n.drop(1) else n
	}
	def readFromFile = {
		val file = filename

		if (file != null && file.exists) {
			Some(if (file.isDirectory) file.listFiles.toList.sort(nameForFileSort(_) <= nameForFileSort(_)).map {f => if (f.isDirectory) f.getName + File.separator else f.getName}.mkString("\n") else scala.io.Source.fromFile(file).mkString(""))
		} else None
	}
	def getHtml(src: Source = null, context: Any = null) {
		if (src != null) getHtmlFromString(src.mkString(""), context)
		else for (str <- read) getHtmlFromString(str, Ober.contextForName(name))
	}
	def getHtmlFromString(str: String, context: Any) = getFromString(str)
	def get(src: Source = null) {
		if (src != null) getFromString(src.mkString(""))
		else read match {
		case Some(str) => getFromString(str)
		case None => errorFromProcess("No file: '"+name+"'")
		}
	}
	def getFromString(str: String) {
		clear
		append(str)
		//TODO the following code is generating a compiler error
		// dirty = false
		dirty_=(false)
	}
	def put {}
	def filename = {
		val n = name
		if (n == "") null
		else {
			val m = ArgMatcher(name, false, this)
			val buf = new StringBuilder
			var end = 0

			for (word <- m) {
				if (m.start > end) {
					buf.appendAll((1 to end - m.start).map(_ => ' '))
				}
				buf.append(word)
			}
			new java.io.File(buf.toString)
		}
	}
}
