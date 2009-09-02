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
import java.io.InputStreamReader;
import java.io.StringReader;
import java.net.{URI,URL};
import java.awt.Dimension;
import java.awt.Color;
import java.awt.Point;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import javax.swing.JScrollPane;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.EditorKit;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.ViewFactory;
import javax.swing.text.Element;
import javax.swing.text.ParagraphView;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.JPanel;
import javax.swing.JEditorPane;
import scala.io.Source;
import Ober._
import scala.swing.Swing._;

case class DocViewerMemento(viewer: DocumentViewer, doc: DefaultStyledDocument, editorKit: EditorKit, wasDirty: Boolean) extends ViewerMemento {
	def restore {
		println("pop")
		viewer.myDoc = doc
		viewer.editor.setEditorKit(editorKit)
		viewer.editor.setDocument(doc)
		viewer.dirty_=(wasDirty)
	}
}

class DocumentViewer extends LeafViewer[JPanel] {
	val resizer = new OberDragWidget(this)
	var wrap = false
	val editor = new JEditorPane
	val scroll = new JScrollPane(editor)
	var myDoc: DefaultStyledDocument = null
	var hasContents = false

	document = new DefaultStyledDocument
	createViewerPanel

	override def dirty_=(newState: Boolean) {
		if (dirty != newState) {
			hasContents = true
			super.dirty_=(newState)
			resizer.repaint()
		}
	}
	override def memento = new DocViewerMemento(this, myDoc, editor.getEditorKit, dirty)
	def document = myDoc
	def document_=(doc: DefaultStyledDocument) {
		myDoc = doc
		doc.addDocumentListener(new DocumentListener() {
//		    def insertUpdate(e: DocumentEvent) = if (editor.getDocument == e.getDocument) dirty = true
		    def insertUpdate(e: DocumentEvent) = if (editor.getDocument == e.getDocument) dirty_=(true)
//		    def changedUpdate(e: DocumentEvent) = if (editor.getDocument == e.getDocument) dirty = true
		    def changedUpdate(e: DocumentEvent) = if (editor.getDocument == e.getDocument) dirty_=(true)
//		    def removeUpdate(e: DocumentEvent) = if (editor.getDocument == e.getDocument) dirty = true
		    def removeUpdate(e: DocumentEvent) = if (editor.getDocument == e.getDocument) dirty_=(true)
		})
	}
	override def append(text: String) = append(text, true)
	def append(text: String, preserveScroll: Boolean = true) {
		val len = document.getLength
		val pos = scroll.getViewport.getViewPosition

		document.insertString(len, text, document.getLogicalStyle(len - 1))
		repositionViewer(pos)
	}
	//Arcane, but needed because of the delayed effect of setting the document's contents
	//there's probably a cleaner way to do this
	def repositionViewer(pos: Point) {
		onEDT {
			onEDT {
				scroll.getViewport.setViewPosition(pos)
			}
		}
	}
	override def clear {
		document.remove(0, document.getLength)
	}
	override def defaultComponent = editor
	def createViewerPanel = {
		viewerPanel = new JPanel
		tag setText Ober.defaultViewerTagText
		tag setBackground Color.lightGray
		setPlainEditorKit
		viewerPanel.setLayout(new GridBagLayout)
		val con = new GridBagConstraints
		val sz = tag.getPreferredSize
		con.fill = GridBagConstraints.BOTH
		resizer.setPreferredSize(new Dimension(sz.height, sz.height))
		resizer.setMinimumSize(new Dimension(sz.height, sz.height))
		viewerPanel.add(resizer, con)
		con.weightx = 1
		viewerPanel.add(tag, con)
		con.gridx = 0
		con.gridwidth = 2
		con.weighty = 1
		viewerPanel.add(scroll, con)
		editor setDocument document
		bindEvents(tag)
		bindEvents(editor)
		lostFocus
	}
	def setPlainEditorKit {
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
	}
	def gainedFocus {
		viewerPanel setBorder redBorder
	}
	def lostFocus = viewerPanel setBorder grayBorder
	override def put {
		val file = filename

		if (file != null && (!file.exists || !file.isDirectory)) {
			val out = new java.io.FileOutputStream(file)

			try {
				out.write(document.getText(0, document.getLength).getBytes)
				out.close
//				dirty = false
				dirty_=(false)
			} catch {
				case e: java.io.IOException => errorFromProcess(e.getStackTraceString)
			}
		}
	}
	def newDocument {
		if (hasContents) Utils.pushHistory(this)
		hasContents = true
		document = new DefaultStyledDocument
		setPlainEditorKit
		editor.setDocument(document)
	}
	override def getFromString(str: String) {
		newDocument
		super.getFromString(str)
	}
	def newHtmlDocument {
		if (hasContents) Utils.pushHistory(this)
		hasContents = true
		val kit = new HTMLEditorKit() {
			override def createDefaultDocument: javax.swing.text.Document = {
				val doc = super.createDefaultDocument.asInstanceOf[HTMLDocument]

				doc.putProperty("IgnoreCharsetDirective", true)
				doc
			}
		}
		val doc = kit.createDefaultDocument.asInstanceOf[HTMLDocument]

		editor.setEditorKit(kit)
		document = editor.getDocument.asInstanceOf[DefaultStyledDocument]
	}
	override def getHtmlFromString(str: String, context: Any) {
		val pos = scroll.getViewport.getViewPosition
		val n = name
		val nn = normalizePath(n)
		if (n != nn) {
			name = nn
		}
		newHtmlDocument
		editor.read(new StringReader(str), Ober.urlForContext(context))
		repositionViewer(pos)
//		dirty = false
		dirty_=(false)
	}
	override def surfTo(newName: String) {
		name = newName
		Ober.contextForName(newName) match {
		case null => ()
		case f: File => get(null)
		case url: URL =>
			val con = url.openConnection

			if (con.getContentType == "text/html") {
				getHtml(Source.fromInputStream(con.getInputStream), url)
			} else {
				get(Source.fromInputStream(con.getInputStream))
			}
		}
	}
}
