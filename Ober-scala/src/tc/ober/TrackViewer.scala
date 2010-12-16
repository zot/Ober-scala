/*
(C) 2009-2010 Bill Burdick

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
import javax.swing.JPanel;
import Ober._
import SimpleViewer._

class TrackViewer extends SimpleViewer[JPanel, OberViewer] {
	val resizer = new OberDragWidget(this)
	val childrenPanel = new JPanel
	val layoutManager = new OberLayout(this, true)

	viewerPanel = new JPanel
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
	override def trackPosition(x: Int) {
		val par = viewerPanel.getParent
		var pBounds = par.getBounds()
		val insets = par.getInsets
		val layout = par.getLayout.asInstanceOf[OberLayout[_]]

		layout.setPosition(this, new java.awt.Point(Math.min(pBounds.x + pBounds.width - insets.right, Math.max(pBounds.x + insets.left, x)), 0))
		par.invalidate
		par.validate
		for (v <- parent.children) {v.viewerPanel.invalidate; v.viewerPanel.validate}
	}
	override def trackWidth(w: Int) {
		if (parent.children.length > 1) {
			val par = viewerPanel.getParent
			var pBounds = par.getBounds()
			val insets = par.getInsets
			val layout = par.getLayout.asInstanceOf[OberLayout[_]]
			val bounds = viewerPanel.getBounds()
			var delta = Math.min(w, pBounds.width - insets.left - insets.right) - bounds.width

			if (delta != 0) {
				if (layout.components.get(0) == viewerPanel) {
					val Some(next: AnyViewer) = parent.children.find(_.viewerPanel == layout.components.get(1))
					val nBounds = next.viewerPanel.getBounds()

					layout.setPosition(next, new java.awt.Point(nBounds.x + delta, 0))
				} else {
					layout.setPosition(this, new java.awt.Point(bounds.x - delta, 0))
				}
				par.invalidate
				par.validate
				for (v <- parent.children) {v.viewerPanel.invalidate; v.viewerPanel.validate}
			}
		}
	}
}
abstract class LeafViewer[T <: Container] extends SimpleViewer[T, TrackViewer] {
	override def resizeDirection = (0, 1)
	override def parent_=(p: TrackViewer) {
		super.parent_=(p)
		p.addViewer(this)
	}
	override def trackPosition(x: Int) = parent.trackPosition(x)
	override def trackWidth(w: Int) = parent.trackWidth(w)
	override def viewerPosition(y: Int) {
		val par = viewerPanel.getParent
		var pBounds = par.getBounds()
		val insets = par.getInsets
		val layout = par.getLayout.asInstanceOf[OberLayout[_]]

		layout.setPosition(this, new java.awt.Point(0, Math.min(pBounds.y + pBounds.height - insets.bottom, Math.max(pBounds.y + insets.top, y))))
		par.invalidate
		par.validate
		for (v <- parent.children) {v.viewerPanel.invalidate; v.viewerPanel.validate}
	}
	override def viewerHeight(h: Int) {
		if (parent.children.length > 1) {
			val par = viewerPanel.getParent
			var pBounds = par.getBounds()
			val insets = par.getInsets
			val layout = par.getLayout.asInstanceOf[OberLayout[_]]
			val bounds = viewerPanel.getBounds()
			var delta = Math.min(h, pBounds.height - insets.top - insets.bottom) - bounds.height

			if (delta != 0) {
				if (layout.components.get(0) == viewerPanel) {
					val Some(next: AnyViewer) = parent.children.find(_.viewerPanel == layout.components.get(1))
					val nBounds = next.viewerPanel.getBounds()

					layout.setPosition(next, new java.awt.Point(0, nBounds.y + delta))
				} else {
					layout.setPosition(this, new java.awt.Point(0, bounds.y - delta))
				}
				par.invalidate
				par.validate
				for (v <- parent.children) {v.viewerPanel.invalidate; v.viewerPanel.validate}
			}
		}
	}
}
