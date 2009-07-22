/*
(C) 2009 Bill Burdick

ar.ober.OberDragWidget

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

import java.awt.BorderLayout;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXFrame;

class OberViewer(vp: JXFrame) extends SimpleViewer[JXFrame, OberViewer] {
	val childrenPanel = new JXPanel
	val layoutManager = new OberLayout(this, false)

	init

	def init {
		viewerPanel = vp
		viewerPanel setLayout new BorderLayout
		viewerPanel.add(tag, BorderLayout.NORTH)
		viewerPanel.add(childrenPanel, BorderLayout.CENTER)
		tag setText Ober.defaultOberTagText
		childrenPanel.setLayout(layoutManager)
		bindEvents(tag)
		lostFocus
	}
	override def topViewer = this
	override def name = "Ober"
	override def widestTrack = {
		if (children == Nil) None
		else Some(children.reduceLeft((a, b) =>
			if (a.viewerPanel.getWidth >= b.viewerPanel.getWidth) a else b).asInstanceOf[TrackViewer])
	}
	def addTrack(tr: TrackViewer): TrackViewer = {
		if (tr.parent == null) {
			tr.parent = this
		} else {
			layoutManager.insert(tr, childrenPanel)
			children ::= tr
		}
		tr
	}
	def gainedFocus {}
	def lostFocus {}
}
