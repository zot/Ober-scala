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
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import javax.swing.JScrollPane;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.ViewFactory;
import javax.swing.text.Element;
import javax.swing.text.ParagraphView;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.JPanel;
import javax.swing.JEditorPane;
import scala.io.Source;
import Ober._

class DocumentViewer extends LeafViewer[JPanel] {
	val resizer = new OberDragWidget(this)
	var wrap = false
	val editor = new JEditorPane
	var document = new DefaultStyledDocument

	createViewerPanel

	override def append(text: String) = {
		val len = document.getLength

		document.insertString(len, text, document.getLogicalStyle(len - 1))
		()
	}
	override def clear {
		document.remove(0, document.getLength)
	}
	override def defaultComponent = editor
	def createViewerPanel = {
		viewerPanel = new JPanel
		tag setText Ober.defaultViewerTagText
		tag setBackground Color.lightGray
		val scroll = new JScrollPane(editor)
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
			} catch {
				case e: java.io.IOException => errorFromProcess(e.getStackTraceString)
			}
		}
	}
	override def getHtmlFromString(str: String, context: Any) {
		val n = name
		val nn = normalizePath(n)
		val kit = new HTMLEditorKit() {
			override def createDefaultDocument: javax.swing.text.Document = {
				val doc = super.createDefaultDocument.asInstanceOf[HTMLDocument]

				doc.putProperty("IgnoreCharsetDirective", true)
				doc
			}
		}
		val doc = kit.createDefaultDocument.asInstanceOf[HTMLDocument]

		if (n != nn) {
			name = nn
		}
		editor.setEditorKit(kit)
		editor.read(new StringReader(str), doc)
		document = editor.getDocument.asInstanceOf[DefaultStyledDocument]
	}
	override def surfTo(newName: String) {
		name = newName
		Ober.contextForName(newName) match {
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
