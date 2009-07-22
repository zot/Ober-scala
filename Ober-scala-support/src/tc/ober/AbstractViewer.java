/*
(C) 2009 Bill Burdick

ar.ober.OberDragWidget

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober;

import java.awt.Container;

public interface AbstractViewer<V> {
	Container viewerPanel();
	boolean dirty();
	void parent_$eq(V v);
	AbstractViewer<?> topViewer();
	void layout();
}
