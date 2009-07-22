package tc.ober;

import java.awt.Container;

public interface AbstractViewer<V> {
	Container viewerPanel();
	boolean dirty();
	void parent_$eq(V v);
	AbstractViewer<?> topViewer();
	void layout();
}
