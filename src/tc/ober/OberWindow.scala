package tc.ober

import org.jdesktop.swingx.JXFrame
import org.jdesktop.swingx.JXButton
import org.jdesktop.swingx.JXEditorPane
import java.awt.event.WindowAdapter
import net.miginfocom.swing.MigLayout;

class OberWindow {
	val frame = new JXFrame
	new OberViewer(frame).addTrack(new TrackViewer).addViewer(new DocumentViewer)
	frame.setSize(300, 300)
	frame addWindowListener ScalaWindowAdapter(System.exit(0))
	frame setVisible true
}
