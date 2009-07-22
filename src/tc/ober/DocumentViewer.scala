/*
(C) 2009 Bill Burdick

ar.ober.OberDragWidget

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

import java.awt.Dimension;
import java.awt.Color;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import javax.swing.JScrollPane;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.ViewFactory;
import javax.swing.text.Element;
import javax.swing.text.ParagraphView;
import org.jdesktop.swingx.JXEditorPane;
import org.jdesktop.swingx.JXPanel;
import net.miginfocom.swing.MigLayout;
import Ober._

class DocumentViewer extends LeafViewer[JXPanel] {
	val resizer = new OberDragWidget(this)
	var wrap = false
	val editor = new JXEditorPane
	val document = new DefaultStyledDocument

	createViewerPanel

	def append(text: String) = {
		val len = document.getLength

		document.insertString(len, text, document.getLogicalStyle(len - 1))
		()
	}
	def createViewerPanel = {
		viewerPanel = new JXPanel
		tag setText Ober.defaultViewerTagText
		tag setBackground Color.lightGray
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
	def gainedFocus {
		viewerPanel setBorder redBorder
	}
	def lostFocus = viewerPanel setBorder grayBorder
}
