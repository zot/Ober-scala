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
import javax.swing.JFrame;
import scala.io.Source
import javax.swing.SwingUtilities;
import scala.swing.Swing._;

class OberWindow {
	val frame = new JFrame()
	val ob = new OberViewer(frame)
	val tr = new TrackViewer
	val doc = new DocumentViewer

	tr.parent = ob
	doc.parent = tr
	frame.setSize(800, 600)
	frame.addWindowListener(new ScalaWindowAdapter(doc, System.exit(0)))
	frame setVisible true
	val rc = new java.io.File(Ober.toFile(System.getProperty("user.home")), ".oberrc")
	if (rc.exists) {
		doc.name = "{HOME}"+java.io.File.separator+".oberrc"
		doc.load
	} else {
		Utils.help(doc)
	}
}
class ScalaWindowAdapter(doc: DocumentViewer, closing: => Any = (null)) extends WindowAdapter {
	var init = false;

	override def windowClosing(e: WindowEvent) {
		if (closing != null) {
			closing
		}
	}
}
