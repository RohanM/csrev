import java.util.ArrayList;

public class QTNode {
    private QTNode parent;
    private QTNode nw, ne, sw, se = null;
    private Rectangle bound;
    private ArrayList<Rectangle> rectangles = new ArrayList<Rectangle>();
    private boolean processed = false;
    

    public QTNode(Rectangle bound) {
	this.parent = null;
	this.bound = bound;
    }

    public QTNode(Rectangle bound, QTNode parent) {
	this.parent = parent;
	this.bound = bound;
    }

    public boolean add(Rectangle r) {
	// If this rectangle doesn't fit inside the bound, return false
	if(!bound.contains(r)) {
	    //System.out.println("Rectangle "+r+" doesn't fit inside bound "+bound+".");
	    return false;
	}

	//System.out.println("Rectangle "+r+" fits inside bound "+bound+", testing children...");

	// If this rectangle fits in one of the children's bounds, add it there
	createChildren();
	if(nw.add(r) || ne.add(r) || sw.add(r) || se.add(r)) {
	    //System.out.println("Rectangle "+r+" added to child.");
	    return true;
	}

	// Otherwise, add it here
	System.out.println("Rectangle "+r+" added to node with bound "+bound+".");
	rectangles.add(r);

	return true;
    }

    public ArrayList<Overlap> getOverlappingRectangles() {
	ArrayList<Overlap> matches = new ArrayList<Overlap>();

	if(!hasChildren()) {
	    // Leaf node, do tests

	    // Get all rectangles to compare
	    ArrayList<Rectangle> rectangleSet = new ArrayList<Rectangle>();
	    QTNode node = this;
	    while(node != null) {
		rectangleSet.addAll(node.rectangles);
		node = node.parent;
	    }

	    // Do n*n comparison
	    for(int i=0; i<rectangleSet.size(); i++) {
		for(int j=0; j<rectangleSet.size(); j++) {
		    if (i != j && rectangleSet.get(i).overlaps(rectangleSet.get(j))) {
			matches.add(new Overlap(rectangleSet.get(i), rectangleSet.get(j)));
		    }
		}
	    }

	} else {
	    // Recurse
	    matches.addAll(nw.getOverlappingRectangles());
	    matches.addAll(ne.getOverlappingRectangles());
	    matches.addAll(sw.getOverlappingRectangles());
	    matches.addAll(se.getOverlappingRectangles());
	}

	return matches;
    }
    

    /**
     * Create child nodes if required.
     */
    private void createChildren() {
	if(!hasChildren()) {
	    nw = new QTNode(bound.getNW(), this);
	    ne = new QTNode(bound.getNE(), this);
	    sw = new QTNode(bound.getSW(), this);
	    se = new QTNode(bound.getSE(), this);

	    /*
	    System.out.println("Node "+bound+" split into:");
	    System.out.println(nw);
	    System.out.println(ne);
	    System.out.println(sw);
	    System.out.println(se);
	    System.out.println("");
	    */
	}
    }

    public boolean hasChildren() {
	return nw != null || ne != null || sw != null || se != null;
    }

    public String toString() {
	String str = bound.toString()+": {";
	if(hasChildren()) {
	    str += nw;
	    str += ne;
	    str += sw;
	    str += se;
	}
	str += "}, ";

	return str;
    }


    public class Overlap {
	public Rectangle r1, r2;

	public Overlap(Rectangle r1, Rectangle r2) {
	    this.r1 = r1;
	    this.r2 = r2;
	}

	public String toString() {
	    return ""+r1+" <--> "+r2;
	}
    }
}
