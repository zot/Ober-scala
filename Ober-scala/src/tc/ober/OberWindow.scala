/*
(C) 2009 Bill Burdick

ar.ober.OberDragWidget

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;
import org.jdesktop.swingx.JXFrame;

class OberWindow {
	val frame = new JXFrame()
	val ob = new OberViewer(frame)
	val tr = new TrackViewer
	val doc = new DocumentViewer
	tr.parent = ob
	doc.parent = tr
//	new OberViewer(frame).addTrack(new TrackViewer).addViewer(new DocumentViewer)
	frame.setSize(300, 300)
	frame.addWindowListener(ScalaWindowAdapter(System.exit(0)))
	frame setVisible true
}
object ScalaWindowAdapter {
	def apply(closing: => Any) = new ScalaWindowAdapter(closing)
}
class ScalaWindowAdapter(closing: => Any = (null)) extends WindowAdapter {
	override def windowClosing(e: WindowEvent) {
		if (closing != null) {
			closing
		}
	}
}
