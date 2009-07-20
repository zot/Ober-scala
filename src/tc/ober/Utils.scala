package tc.ober

import javax.swing.AbstractAction;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

object ScalaAction {
	def apply(title: String)(block: => Any) = new ScalaAction("Hello", block)
}
class ScalaAction(name: String, block: => Any) extends AbstractAction(name) {
	def actionPerformed(evt: ActionEvent) {
		block
	}
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