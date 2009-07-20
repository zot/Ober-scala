package tc.ober

import java.awt.Color
import java.awt.Component
import java.awt.Container;
import org.jdesktop.swingx.JXEditorPane
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXFrame;
import javax.swing.text.DefaultStyledDocument
import java.util.regex.Pattern
import javax.swing.JTextField;
import javax.swing.JScrollPane;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.ViewFactory;
import net.miginfocom.swing.MigLayout;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.event.KeyAdapter
import javax.swing.text.Element;
import javax.swing.text.ParagraphView;
import javax.swing.text.JTextComponent;
import javax.swing.text.Document;
import javax.swing.border.LineBorder;
import scala.collection.mutable.{HashMap => MMap}
import Ober._

class SimpleContext(val comp: JTextComponent, val viewer: SimpleViewer[_], val word: String, val args: String, wStart: Int, wEnd: Int, lStart: Int, lEnd: Int) extends ViewerContext
abstract class SimpleViewer[PANEL_TYPE <: Container] {
	val tag = new JTextField
	val properties = MMap[Any, Any]()
	var viewerPanel: PANEL_TYPE = null.asInstanceOf[PANEL_TYPE]
	var children = List[AnyViewer]()
	var parent: AnyViewer = null

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
				
			}
		}
	}
	def findChild(childName: String): AnyViewer = findChild(_.name == childName)
	def findChild(predicate: AnyViewer => Boolean): AnyViewer = {
		if (predicate(this)) {
			this
		} else {
			for (v <- children) {
				val child = v.findChild(predicate)

				if (child != null) {
					child
				}
			}
			null
		}
	}
	def resizeDirection = (0, 0)
	def gainedFocus: Any
	def lostFocus: Any
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

//		println("colonpos"+colonPos)
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
	def topViewer: OberViewer = parent.topViewer
	def outputFromProcess(str: String) {
		errorFromProcess(str)
	}
	def errorFromProcess(str: String) {
		var viewer = topViewer.findChild(child => child.name.startsWith("Err") && child.isInstanceOf[DocumentViewer]).asInstanceOf[DocumentViewer]

		if (viewer == null) {
			viewer = newDocumentViewer
			viewer.tag.setText(defaultErrorViewerTagText)
		}
		print(str)
	}
	def widestTrack: Option[TrackViewer] = topViewer.widestTrack
	def newDocumentViewer: DocumentViewer = {
		val viewer = new DocumentViewer()
		var track = widestTrack getOrElse {
			val tr = new TrackViewer
			topViewer.addTrack(tr)
			tr
		}
		viewer.name = name
		track.addViewer(viewer)
		viewer
	}
}
class OberViewer(vp: JXFrame) extends SimpleViewer[JXFrame] {
	viewerPanel = vp
	viewerPanel setLayout new MigLayout("fill")
	tag setText Ober.defaultOberTagText
	viewerPanel.add(tag, "dock north")
	bindEvents(tag)
	lostFocus

	override def topViewer = this
	override def name = "Ober"
	override def widestTrack = {
		if (children == Nil) None
		else Some(children.reduceLeft((a, b) =>
			if (a.viewerPanel.getWidth >= b.viewerPanel.getWidth) a else b).asInstanceOf[TrackViewer])
	}
	def addTrack(tr: TrackViewer) = {
		viewerPanel.add(tr.viewerPanel, "push,grow")
		children ::= tr
		tr.parent = this
		tr
	}
	def gainedFocus {}
	def lostFocus {}
}
class TrackViewer extends SimpleViewer[JXPanel] {
	val resizer = new Resizer(this)

	viewerPanel = new JXPanel
	tag setText Ober.defaultTrackTagText
	viewerPanel setLayout new MigLayout("fill, gap 0 0, ins 0")
	viewerPanel.add(resizer.panel, "split, w 16:16:16, h 16:16:16, span")
	viewerPanel.add(tag, "wrap, width 100%")
	bindEvents(tag)
	tag setBackground Color.cyan
	lostFocus

	override def resizeDirection = (1, 0)
	def addViewer(viewer: AnyLeafViewer) = {
		viewerPanel.add(viewer.viewerPanel, "pushy,grow,wrap")
		children ::= viewer
		viewer.parent = this
		viewer
	}
	def gainedFocus {
		viewerPanel setBorder blackBorder
	}
	def lostFocus = viewerPanel setBorder grayBorder
}
abstract class LeafViewer[T <: Container] extends SimpleViewer[T] {
	override def resizeDirection = (0, 1)
}
class DocumentViewer extends LeafViewer[JXPanel] {
	val resizer = new Resizer(this)
	var wrap = false
	val editor = new JXEditorPane
	val document = new DefaultStyledDocument

	createViewerPanel

	def createViewerPanel = {
		viewerPanel = new JXPanel
		tag setText Ober.defaultViewerTagText
		tag setBackground Color.lightGray
		viewerPanel setLayout new MigLayout("gap 0 0, ins 0")
		val scroll = new JScrollPane(editor)
		editor.setEditorKit(new StyledEditorKit {
			def superGetViewFactory = super.getViewFactory
			override def getViewFactory = fact
			val fact = new ViewFactory {
				def create(elem: Element) = {
					val view = superGetViewFactory.create(elem)
					view match {
						case p : ParagraphView => new ParagraphView(elem) {
							override def layout(width: Int, height: Int) {
								if (wrap) {
									super.layout(width, height);
								} else {
									super.layout(java.lang.Short.MAX_VALUE, height);
								}
							}
							override def getMinimumSpan(axis: Int) = {
								if (wrap) {
									super.getMinimumSpan(axis)
								} else {
									super.getPreferredSpan(axis);
								}
							}
						}
						case other => other
					}
				}
			}
		})
		editor setDocument document
		viewerPanel.add(resizer.panel, "split, w 16:16:16, h 16:16:16, span")
		viewerPanel.add(tag, "wrap, width 100%")
		viewerPanel.add(scroll, "push, grow")
		bindEvents(tag)
		bindEvents(editor)
		lostFocus
	}
	def gainedFocus {
		viewerPanel setBorder redBorder
	}
	def lostFocus = viewerPanel setBorder grayBorder
}
class Resizer(viewer: AnyViewer) {
	val panel = new JXPanel
	val originalColor = panel getBackground
	
	panel setBorder blackBorder
	panel addMouseListener new MouseAdapter {
		override def mouseEntered(e: MouseEvent) {
			if (viewer.parent != null && viewer.parent.children.tail != Nil) {
				panel setBackground Color.green
			}
		}
		override def mouseExited(e: MouseEvent) {
			panel setBackground originalColor
		}
	}
}
