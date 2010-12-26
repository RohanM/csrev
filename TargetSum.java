import java.util.*;


public class TargetSum {
    protected static int[] numbers;
    protected static int target;

    public static void main(String[] args) {
	try {
	    processArgs(args);

	    Arrays.sort(numbers);

	    // Display the sorted array
	    System.out.print("Sorted numbers: ");
	    for(int i=0; i<numbers.length; i++) {
		System.out.print(""+numbers[i]+" ");
	    }
	    System.out.println("");


	    int i;
	    int j = -1;
	    for(i=0; i<numbers.length; i++) {
		int t = target - numbers[i];

		// Check if we're searching for another instance of numbers[i]
		// If we are, simply look to either side
		// Otherwise, do a binary search
		if (t == numbers[i]) {
		    if(i > 0 && numbers[i-1] == t) {
			j = i-1;
			break;
		    }
		    if(i+1 < numbers.length && numbers[i+1] == t) {
			j = i+1;
			break;
		    }

		} else {
		    j = Arrays.binarySearch(numbers, t);
		    if(j >= 0) {
			break;
		    }
		}
	    }

	    // Display result
	    if(j >= 0) {
		System.out.println("Found match: "+i+" and "+j+ " ("+numbers[i]+"+"+numbers[j]+")");
	    } else {
		System.out.println("No match found.");
	    }

	} catch(IllegalArgumentException e) {
	    System.out.println("Usage: java TargetSum [target] [n0 n1 n2 n3 ...]");
	}
    }


    protected static void processArgs(String[] args) throws IllegalArgumentException {
	// We require a target and two numbers to test
	if(args.length < 3) {
	    throw new IllegalArgumentException();
	}

	target = Integer.parseInt(args[0]);

	numbers = new int[args.length-1];
	for(int i=1; i < args.length; i++) {
	    numbers[i-1] = Integer.parseInt(args[i]);
	}
    }


    /**
     * Binary search sorted(numbers) for a number that sums with numbers[i] to give target
     */
    /*
    protected static void search(int i) {
	int j = numbers.length / 2;
	int t = target - numbers[i];

	if(t == numbers[j]) {
	}

	while(numbers[j] != t) {
	    if(numbers[j] > t) {
		j = (j + numbers.length) / 2;
	    } else {
		j = j / 2;
	    }
	}

	if(i == j) {
	}

	
    }
    */
}