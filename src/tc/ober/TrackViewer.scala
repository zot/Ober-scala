/*
(C) 2009 Bill Burdick

ar.ober.OberDragWidget

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import org.jdesktop.swingx.JXPanel;
import net.miginfocom.swing.MigLayout;
import Ober._

class TrackViewer extends SimpleViewer[JXPanel, OberViewer] {
	val resizer = new OberDragWidget(this)
	val childrenPanel = new JXPanel
	val layoutManager = new OberLayout(this, true)

	viewerPanel = new JXPanel
	viewerPanel.setLayout(new GridBagLayout {
		override def layoutContainer(c: Container) {
			super.layoutContainer(c)
		}
	})
	val con = new GridBagConstraints
	con.fill = GridBagConstraints.BOTH
	val sz = tag.getPreferredSize
	resizer.setPreferredSize(new Dimension(sz.height, sz.height))
	resizer.setMinimumSize(new Dimension(sz.height, sz.height))
	viewerPanel.add(resizer, con)
	con.weightx = 1
	viewerPanel.add(tag, con)
	con.gridx = 0
	con.gridwidth = 2
	con.weighty = 1
	viewerPanel.add(childrenPanel, con)
	tag setText Ober.defaultTrackTagText
	bindEvents(tag)
	childrenPanel.setLayout(layoutManager)
	tag setBackground Color.cyan
	lostFocus

	override def isTrack = true
	override def parent_=(p: OberViewer) {
		super.parent_=(p)
		p.addTrack(this)
	}
	override def trackForNewViewer = this
	override def resizeDirection = (1, 0)
	def addViewer[T <: Container](viewer: LeafViewer[T]): LeafViewer[T] = {
		if (viewer.parent == null) {
			viewer.parent = this
		} else {
			layoutManager.insert(viewer, childrenPanel)
//			childrenPanel.add(viewer.viewerPanel)
			children ::= viewer
		}
		viewer
	}
	def gainedFocus = viewerPanel setBorder blackBorder
	def lostFocus = viewerPanel setBorder grayBorder
}
abstract class LeafViewer[T <: Container] extends SimpleViewer[T, TrackViewer] {
	override def resizeDirection = (0, 1)
	override def parent_=(p: TrackViewer) {
		super.parent_=(p)
		p.addViewer(this)
	}
}
