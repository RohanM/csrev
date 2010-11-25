import java.io.*;
import java.util.*;
import java.util.regex.*;

public class Rectangles {

    public static void main(String[] args) {
	// Get filename to load
	if(args.length < 1) {
	    System.out.println("Usage: java Rectangles <filename>");
	    System.exit(1);
	}
	
	try {
	    // Open the file
	    BufferedReader file = new BufferedReader(new FileReader(new File(args[0])));

	    // Load all the rectangles
	    String line;
	    Pattern pattern = Pattern.compile("(\\d+) (\\d+) (\\d+) (\\d+)");
	    Matcher matcher;
	    ArrayList<Rectangle> rectangles = new ArrayList<Rectangle>();

	    while((line = file.readLine()) != null) {
		matcher = pattern.matcher(line);
		if(matcher.find() && matcher.groupCount() == 4) {
		    rectangles.add(new Rectangle(Integer.parseInt(matcher.group(1)),
						 Integer.parseInt(matcher.group(2)),
						 Integer.parseInt(matcher.group(3)),
						 Integer.parseInt(matcher.group(4))));
		}
	    }

	    // Compute the bound
	    Rectangle bound = rectangles.get(0);
	    for(int i=0; i<rectangles.size(); i++) {
		bound.expandBy(rectangles.get(i));
	    }

	    // Create the tree
	    QTNode root = new QTNode(bound);
	    for(int i=0; i<rectangles.size(); i++) {
		root.add(rectangles.get(i));
	    }

	    // Get the overlapping rectangles
	    ArrayList<QTNode.Overlap> overlaps = root.getOverlappingRectangles();

	    for(int i=0; i<overlaps.size(); i++) {
		System.out.println(overlaps.get(i));
	    }

	} catch(FileNotFoundException e) {
	    System.out.println("File was not found.");
	} catch(IOException e) {
	    System.out.println("An error was encountered when attempting to open that file.");
	}
    }

}