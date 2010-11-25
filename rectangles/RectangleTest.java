import org.junit.*;
import static org.junit.Assert.*;


public class RectangleTest {

    @Test public void testQuadrants() {
	Rectangle r = new Rectangle(15, 15, 30, 30);
	Rectangle nw, ne, sw, se;

	nw = r.getNW();
	ne = r.getNE();
	sw = r.getSW();
	se = r.getSE();

	// (15, 22), (22, 30)
	assertEquals(nw.x0, 15);
	assertEquals(nw.y0, 22);
	assertEquals(nw.x1, 22);
	assertEquals(nw.y1, 30);

	// (22, 22), (30, 30)
	assertEquals(ne.x0, 22);
	assertEquals(ne.y0, 22);
	assertEquals(ne.x1, 30);
	assertEquals(ne.y1, 30);

	// (15, 15), (22, 22)
	assertEquals(sw.x0, 15);
	assertEquals(sw.y0, 15);
	assertEquals(sw.x1, 22);
	assertEquals(sw.y1, 22);

	// (22, 15), (30, 22)
	assertEquals(se.x0, 22);
	assertEquals(se.y0, 15);
	assertEquals(se.x1, 30);
	assertEquals(se.y1, 22);
    }

    @Test public void testOverlaps() {
	Rectangle r1 = new Rectangle(0, 0, 10, 10);

	// No overlap
	assertFalse(r1.overlaps(new Rectangle(20, 20, 30, 30)));

	// Touching is not an overlap
	assertFalse(r1.overlaps(new Rectangle(10, 0, 20, 10)));

	// Corner
	assertTrue(r1.overlaps(new Rectangle(5, 5, 15, 15)));

	// Contains
	assertTrue(r1.overlaps(new Rectangle(2, 2, 4, 4)));

	// Contained
	assertTrue(r1.overlaps(new Rectangle(-5, -5, 50, 50)));

	// Horizontal cross
	assertTrue(r1.overlaps(new Rectangle(-5, 2, 15, 4)));

	// Vertical cross
	assertTrue(r1.overlaps(new Rectangle(2, -5, 4, 15)));
    }

    @Test public void testContains() {
	Rectangle r1 = new Rectangle(0, 0, 10, 10);

	// Outside
	assertFalse(r1.contains(new Rectangle(20, 20, 30, 30)));

	// Overlap
	assertFalse(r1.contains(new Rectangle(5, 5, 15, 15)));

	// Within
	assertTrue(r1.contains(new Rectangle(2, 2, 4, 4)));

	// Exactly within
	assertTrue(r1.contains(r1));
    }

    @Test public void testExpandBy() {
	Rectangle r1 = new Rectangle(0, 0, 10, 10);
	r1.expandBy(new Rectangle(-5, 0, -3, 2));

	assertEquals(r1.x0, -5);
	assertEquals(r1.y0, 0);
	assertEquals(r1.x1, 10);
	assertEquals(r1.y1, 10);
    }

    public static void main(String args[]) {
      org.junit.runner.JUnitCore.main("RectangleTest");
    }
}
