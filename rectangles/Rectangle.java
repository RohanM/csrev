public class Rectangle {
    public int x0, y0, x1, y1;

    public Rectangle(int x0, int y0, int x1, int y1) {
	this.x0 = x0;
	this.y0 = y0;
	this.x1 = x1;
	this.y1 = y1;
    }

    public boolean overlaps(Rectangle r) {
	return ((r.x0 > x0 && r.x0 < x1) ||  (r.x1 > x0 && r.x1 < x1) || (x0 > r.x0 && x0 < r.x1))
	    && ((r.y0 > y0 && r.y0 < y1) ||  (r.y1 > y0 && r.y1 < y1) || (y0 > r.y0 && y0 < r.y1));
    }


    public boolean contains(Rectangle r) {
	return r.x0 >= x0 && r.x1 <= x1 && r.y0 >= y0 && r.y1 <= y1;
    }

    /**
     * Note: Requires that (x0, y0) is bottom-left and (x1, y1) is top-right.
     */
    public void expandBy(Rectangle r) {
	x0 = Math.min(x0, r.x0);
	y0 = Math.min(y0, r.y0);
	x1 = Math.max(x1, r.x1);
	y1 = Math.max(y1, r.y1);
    }

    /**
     * Get the north-west quadrant of this rectangle.
     */
    public Rectangle getNW() {
	return new Rectangle(x0, (y0+y1) / 2, (x0+x1) / 2, y1);
    }

    /**
     * Get the north-east quadrant of this rectangle.
     */
    public Rectangle getNE() {
	return new Rectangle((x0+x1) / 2, (y0+y1) / 2, x1, y1);
    }

    /**
     * Get the south-west quadrant of this rectangle.
     */
    public Rectangle getSW() {
	return new Rectangle(x0, y0, (x0+x1) / 2, (y0+y1) / 2);
    }

    /**
     * Get the south-east quadrant of this rectangle.
     */
    public Rectangle getSE() {
	return new Rectangle((x0+x1) / 2, y0, x1, (y0+y1) / 2);
    }

    public String toString() {
	return String.format("[(%d, %d) --> (%d, %d)]", x0, y0, x1, y1);
    }
}
