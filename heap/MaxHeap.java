public class MaxHeap {
    protected int data[];
    protected int size;

    public MaxHeap(int capacity) {
	data = new int[capacity];
	size = 0;
    }


    public void push(int n) throws Exception {
	// Check for heap overflow
	if(size >= data.length) {
	    throw new Exception();
	}

	// Add as last element
	int i = size;
	data[i] = n;
	size += 1;

	// Float this element upward in the heap
	while(i > 0 && data[i] > data[parent(i)]) {
	    swap(i, parent(i));
	    i = parent(i);
	}
    }


    public int pop() {
	int n = data[0];

	// Move the last element into top position
	data[0] = data[size-1];
	size -= 1;

	// Float that element down the heap
	// TODO: Handle element at position i has one or no children

	int i = 0;
	boolean finished = false;
	while(!finished) {
	    int left, right, parent;
	    left = left(i) < size ? data[left(i)] : Integer.MIN_VALUE;
	    right = right(i) < size ? data[right(i)] : Integer.MIN_VALUE;
	    parent = data[i];
	    if(left >= right && left > parent) {
		// Swap parent and left
		swap(i, left(i));
		i = left(i);
		
	    } else if(right > left && right > parent) {
		// Swap parent and right
		swap(i, right(i));
		i = right(i);
		
	    } else {
		finished = true;
	    }
	}

	return n;
    }
    
    public boolean empty() {
	return size == 0;
    }


    private int parent(int i) {
	return (i-1)/2;
    }
    private int left(int i) {
	return i*2+1;
    }
    private int right(int i) {
	return i*2+2;
    }

    private void swap(int i, int j) {
	int temp = data[i];
	data[i] = data[j];
	data[j] = temp;
    }

    public void display() {
	for(int i=0; i<size; i++) {
	    System.out.println(data[i]);
	}
	System.out.println("");
    }


    public static void main(String[] args) {
	try {
	    MaxHeap h = new MaxHeap(10);

	    h.push(5);
	    h.push(2);
	    h.push(9);
	    h.push(4);
	    h.push(1);
	    h.push(3);
	    h.push(6);
	    h.push(7);
	    h.push(8);
	    h.push(8);

	    h.display();

	    while(!h.empty()) {
		System.out.println(h.pop());
	    }
	} catch(Exception e) {
	    System.out.println(e);
	}
    }
}
