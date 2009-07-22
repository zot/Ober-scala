/*
(C) 2009 Bill Burdick

ar.ober.OberLayout

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager2;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;

public class OberLayout<T extends AbstractViewer<?>> implements LayoutManager2 {
	protected boolean vertical;
	protected ArrayList<Component> components = new ArrayList<Component>();
	protected float positions[] = new float[8];
	protected T viewer;
 
 	public OberLayout(T v, boolean vert) {
 		vertical = vert;
 		viewer = v;
 	}
	@SuppressWarnings("unchecked")
	public void addLayoutComponent(Component comp, Object constraints) {
		if (!(comp instanceof OberDragWidget.Tracker)) {
			float position = constraints == null ? 0 : ((Float)constraints).floatValue();
			int index = 0;
			float newPositions[] = positions;
			
			for (int i = components.size(); i-- > 0; ) {
				if (positions[i] < position) {
					index = i + 1;
					break;
				}
			}
			if (index == 0) {
				position = 0;
			}
			components.add(index, comp);
			while (newPositions.length < components.size()) {
				newPositions = new float[newPositions.length * 2];
			}
			if (index > 0) {
				System.arraycopy(positions, 0, newPositions, 0, index);
			}
			if (components.size() - index - 1 > 0) {
				System.arraycopy(positions, index, newPositions, index + 1, components.size() - index - 1);
			}
			newPositions[index] = position;
			positions = newPositions;
			positions[0] = 0;
		}
	}
	public Dimension maximumLayoutSize(Container target) {
		Dimension max = new Dimension(0, 0);
		
		for (int i = 0; i < components.size(); i++) {
			Dimension size = ((Component)components.get(i)).getMaximumSize();

			if (vertical) {
				max.height += size.height;
				max.width = Math.min(max.width, size.width);
			} else {
				max.width += size.width;
				max.height = Math.min(max.height, size.height);
			}
		}
		return max;
	}
	public float getLayoutAlignmentX(Container target) {
		return (float) 0.5;
	}
	public float getLayoutAlignmentY(Container target) {
		return (float) 0.5;
	}
	public void invalidateLayout(Container target) {}
	public void addLayoutComponent(String name, Component comp) {}
	@SuppressWarnings("unchecked")
	public void removeLayoutComponent(Component comp) {
		if (!(comp instanceof OberDragWidget.Tracker)) {
			int index = components.indexOf(comp);

			if (components.size() - index - 1 > 0) {
				System.arraycopy(positions, index + 1, positions, index, components.size() - index - 1);
			}
			components.remove(index);
			positions[0] = 0;
		}
	}
	public Dimension preferredLayoutSize(Container parent) {
		return minimumLayoutSize(parent);
	}
	public Dimension minimumLayoutSize(Container parent) {
		Dimension min = new Dimension(0, 0);
		
		for (int i = 0; i < components.size(); i++) {
			Dimension size = ((Component)components.get(i)).getMinimumSize();

			if (vertical) {
				min.height += size.height;
				min.width = Math.max(min.width, size.width);
			} else {
				min.width += size.width;
				min.height = Math.max(min.height, size.height);
			}
		}
		return min;
	}
	public void layoutContainer(Container parent) {
		Rectangle bounds = parent.getBounds();
		float prev = 1;

		for (int i = components.size(); i-- > 0; ) {
			Component comp = (Component) components.get(i);
			Insets insets = parent.getInsets();
			int height = bounds.height - insets.top - insets.bottom;
			int width = bounds.width - insets.left - insets.right;

			if (vertical) {
				comp.setBounds(insets.left, Math.round(height * positions[i]) + insets.top, width, Math.round(height * (prev - positions[i])));
			} else {
				comp.setBounds(Math.round(width * positions[i]) + insets.left, insets.top, Math.round(width * (prev - positions[i])), height);
			}
			prev = positions[i];
		}
	}
	public Container topComponent(Component comp) {
		Container cont = comp.getParent();

		while (cont.getParent() != null) {
			cont = cont.getParent();
		}
		return cont;
	}
	@SuppressWarnings("unchecked")
	public void constrain(int x, int y, OberDragWidget widget, OberDragWidget.Tracker tracker) {
		Container top = topComponent(tracker);
		Point splitterPoint = new Point(x, y);
		widget.transformPoint(splitterPoint, widget, widget.viewer.viewerPanel().getParent());
		Point windowPoint = new Point(x, y);
		widget.transformPoint(windowPoint, widget, top);
		Point widgetLocation = widget.getLocation();
		widget.transformPoint(widgetLocation, widget.getParent(), widget.viewer.viewerPanel().getParent());
		Component target = top.findComponentAt(windowPoint);
		Container targetParent = tracker.getParent();

		if (target != null) {
			OberLayout<?> contLayout = (OberLayout<?>)tracker.getParent().getLayout();

			if (target instanceof Container) {
				targetParent = (Container)target;
			} else {
				targetParent = target.getParent();
			}
			while (targetParent != null) {
				if (targetParent.getLayout() instanceof OberLayout && ((OberLayout<?>)targetParent.getLayout()).isVertical() == contLayout.isVertical()) {
					break;
				}
				targetParent = targetParent.getParent();
			}
			if (targetParent != null && targetParent != tracker.getParent()/* && ((OberLayout<?>)targetParent.getLayout()).isVertical() == ((OberLayout<?>)tracker.getParent().getLayout()).isVertical()*/) {
				tracker.getParent().remove(tracker);
				targetParent.add(tracker);
			} else {
				targetParent = tracker.getParent();
			}
		}
		if (vertical) {
			tracker.setLocation(widgetLocation.x, Math.max(targetParent.getInsets().top + widget.getInsets().top, Math.min(splitterPoint.y, targetParent.getHeight() - tracker.getHeight() - targetParent.getInsets().bottom - widget.getInsets().bottom)));
		} else {
			tracker.setLocation(Math.max(targetParent.getInsets().left + widget.getInsets().left, Math.min(splitterPoint.x, tracker.getParent().getWidth() - tracker.getWidth() - targetParent.getInsets().right - widget.getInsets().right)), widgetLocation.y);
		}
	}
	protected boolean isVertical() {
		return vertical;
	}
	public void insert(AbstractViewer<T> child, Container container) {
		float max = 0;
		int maxIndex = 0;

		for (int i = 0; i < components.size(); i++) {
			float next = (i == components.size() - 1 ? 1 : positions[i + 1]);

			if (next - positions[i] > max) {
				max = next - positions[i];
				maxIndex = i;
			}
		}
		float old = positions[maxIndex];
		positions[maxIndex] = (positions[maxIndex] + (maxIndex == components.size() - 1 ? 1 : positions[maxIndex + 1])) / 2;
		container.add(child.viewerPanel(), new Float(old));
		child.layout();
	}
	public void insert(AbstractViewer<T> child, Container container, Point point) {
		container.add(child.viewerPanel(), new Float(vertical ? point.y / (float)container.getHeight() : point.x / (float)container.getWidth()));
	}
	public void setPosition(AbstractViewer<T> viewer, Point point) {
		setPosition(viewer, vertical ? point.y / (float)viewer.viewerPanel().getParent().getHeight() : point.x / (float)viewer.viewerPanel().getParent().getWidth());
	}
	public void setPosition(AbstractViewer<T> viewer, float pos)  {
		removeLayoutComponent(viewer.viewerPanel());
		addLayoutComponent(viewer.viewerPanel(), new Float(pos));
	}
}
