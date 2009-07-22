/*
(C) 2009 Bill Burdick

ar.ober.OberDragWidget

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.Container;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import javax.swing.JTextField;
import javax.swing.AbstractAction;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXFrame;
import javax.swing.text.Document;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.JTextComponent;
import scala.collection.mutable.{HashMap => MMap};
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
	var dirty = false

	def parent = myParent
	def parent_=(p: PARENT) {
//		val par = p.asInstanceOf[PARENT]
//
//		myParent = par
		myParent = p
	}
	def createNewTrack = newTrack
	def createNewViewer = newViewer
	def nameMatcher = namePattern.findAllIn(tag.getText)
	def name = {
		val nm = nameMatcher

		if (nm.hasNext) {
			nm.next
			nm.group(1)
		} else {
			""
		}
	}
	def name_=(newName: String) {
		val nm = nameMatcher.matchData

		if (nm.hasNext) {
			val txt = tag.getText
			val firstMatch = nm.next
			tag.setText(txt.take(firstMatch.start(1))+txt.drop(firstMatch.end(1)))
		} else {
			val txt = tag.getText
			val colonPos = txt.indexOf(':')

			if (colonPos != -1) {
				tag.setText(txt.take(colonPos) + " " + newName + " " + txt.drop(colonPos))
			}
		}
	}
	def find(childName: String): AnyViewer = find((x: AnyViewer) => x.name == childName)
	def find(pred: AnyViewer => Boolean): AnyViewer = if (pred(this)) this else subFind(pred, children)
	def subFind(pred: AnyViewer => Boolean, rem: List[AnyViewer]): AnyViewer = {
		if (rem == Nil) {
			null
		} else {
			val ret = rem.head.find(pred)

			if (ret != null) {
				ret
			} else {
				subFind(pred, rem.tail)
			}
		}
	}
	def resizeDirection = (0, 0)
	def isTrack = false
	def wordAtPosition(doc: Document, pos: Int) = {
		var word: Option[(String, String, Int, Int, Int, Int)] = None
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
					word = Some((matcher matched, text.drop(matcher.end), matcher.start + start, matcher.end + start, start, end))
					true
				} else false
			}
		}
		word
	}
	def namespaces = {
		val txt = tag.getText
		val colonPos = txt indexOf ':'

		if (colonPos == -1) {
			None
		} else {
			Some((txt take colonPos) mkString)
		}
	}
	def bindEvents(comp: JTextComponent) {
		comp addMouseListener new MouseAdapter {
			override def mouseClicked(e: MouseEvent) {
				if (e.getButton == 3) {
					val comp: JTextComponent = e.getSource.asInstanceOf[JTextComponent]

					for ((word, args, start, end, lineStart, lineEnd) <- wordAtPosition(comp.getDocument, comp.viewToModel(e.getPoint))) {
						if (comp.modelToView(start).union(comp.modelToView(end)).contains(e.getPoint)) {
							Ober.handlerFor(namespaces.getOrElse("").split("  *"), word, new SimpleContext(comp, SimpleViewer.this, word, args, start, end, lineStart, lineEnd))
						}
					}
				}
			}
		}
		comp addFocusListener new FocusAdapter {
			override def focusGained(e: FocusEvent) {
				Ober gainFocus SimpleViewer.this
			}
		}
	}
	def layout {
		viewerPanel.invalidate
		topViewer.viewerPanel.validate
	}
	def topViewer: OberViewer = parent.topViewer
	def outputFromProcess(str: String) = errorFromProcess(str)
	def errorFromProcess(str: String) {
		var viewer = topViewer.find{child: AnyViewer => child.name.startsWith("Err") && child.isInstanceOf[DocumentViewer]}.asInstanceOf[DocumentViewer]

		if (viewer == null) {
			viewer = topViewer.newDocumentViewer
			viewer.tag.setText(defaultErrorViewerTagText)
			println("name: '"+viewer.name+"'")
			println("child: "+topViewer.find("Err"))
			println(viewer.name.startsWith("Err") && viewer.isInstanceOf[DocumentViewer])
			println(topViewer.find(child => child.name.startsWith("Err") && child.isInstanceOf[DocumentViewer]).asInstanceOf[DocumentViewer])
		}
		viewer.append(str)
	}
	def widestTrack: Option[TrackViewer] = topViewer.widestTrack
	def newTrack = {
		val tr = new TrackViewer
		topViewer.addTrack(tr)
	}
	def trackForNewViewer = widestTrack getOrElse newTrack
	//Create a document viewer by default
	def newViewer: AnyViewer = newDocumentViewer
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
		viewerPanel.getParent.remove(viewerPanel)
		parent.removeChild(this)
		layout
	}
	def removeChild(child: AnyViewer) {
		children = children - child
	}
}
