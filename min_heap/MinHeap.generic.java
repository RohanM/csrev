import java.nio.BufferOverflowException;
import java.nio.BufferUnderflowException;


// TODO: Make this class accept any orderable data type, not only ints
// TODO: Make the heap auto-resizing

// TODO: How to exchange two variables without using a third (xor?)

public class MinHeap<E> {
    protected ArrayList<E> data;
    protected int size;

    public MinHeap(int max_size) {
	data = new ArrayList<E>(max_size);
	size = 0;
    }
    
    public int getSize() {
	return size;
    }

    public boolean empty() {
	return size == 0;
    }

    public void push(E value) throws BufferOverflowException {
	if(size == data.length) {
	    throw new BufferOverflowException();
	}

	try {
	    size++;
	    data[size-1] = E.MAX_VALUE;
	    decreaseValue(size-1, value);

	} catch(Exception e) {
	    System.out.println("Error when decreasing value in MinHeap.push(). This should not happen! "+e);
	}
    }

    /**
     * Pop the minimum value from the heap.
     * O(lg(n)) due to call to minHeapify().
     */
    public E pop() throws BufferUnderflowException {
	if(size < 1) {
	    throw new BufferUnderflowException();
	}

	E min = data[0];
	data[0] = data[size-1];
	size--;
	minHeapify(0);

	return min;
    }


    protected void minHeapify(int i) {
	int smallest = i;

	// Find the smallest element of i and its two children
	if(left(i) < size && data[left(i)] < data[i]) {
	    smallest = left(i);
	}
	if(right(i) < size && data[right(i)] < data[smallest]) {
	    smallest = right(i);
	}

	if(smallest != i) {
	    // Exchange smallest and i
	    E tmp = data[i];
	    data[i] = data[smallest];
	    data[smallest] = tmp;

	    minHeapify(smallest);
	}
    }

    protected void decreaseValue(int i, E value) throws Exception {
	if(value > data[i]) {
	    throw new Exception("When decreasing value, new value must be less than the old value.");
	}

	data[i] = value;
	E tmp;
	while(i > 0 && data[parent(i)] > data[i]) {
	    // Exchange i and parent(i)
	    tmp = data[i];
	    data[i] = data[parent(i)];
	    data[parent(i)] = tmp;

	    // Move upwards in the heap
	    i = parent(i);
	}
    }


    protected int parent(int i) {
	return i/2;
    }

    protected int left(int i) {
	return i*2;
    }

    protected int right(int i) {
	return i*2+1;
    }

    public static void main(String args[]) {
	MinHeap<int> h = new MinHeap<int>(5);

	try {
	    h.push(2);
	    h.push(4);
	    h.push(1);
	    h.push(5);
	    h.push(3);
	
	
	    while(!h.empty()) {
		System.out.println(h.pop());
	    }

	} catch(Exception e) {
	    System.out.println("Caught exception: "+e);
	}
    }
}