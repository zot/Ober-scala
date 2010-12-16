/*
(C) 2009-2010 Bill Burdick

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

	tr.parent = ob
	frame.setSize(800, 600)
	frame.addWindowListener(new ScalaWindowAdapter(System.exit(0)))
	frame setVisible true
	onEDT {
		val rc = new java.io.File(Ober.toFile(System.getProperty("user.home")), ".oberrc")
		if (rc.exists) {
			val doc = new DocumentViewer
			doc.parent = tr
			doc.name = rc.getAbsolutePath; //"{HOME}"+java.io.File.separator+".oberrc"
			doc.load(rc.getAbsolutePath)
		} else {
			Utils.help(tr)
		}
	}
}
class ScalaWindowAdapter(closing: => Any = (null)) extends WindowAdapter {
	var init = false;

	override def windowClosing(e: WindowEvent) {
		if (closing != null) {
			closing
		}
	}
}
