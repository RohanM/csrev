class Node:
    prev = next = None
    value = None

    def __init__(self, value, prev, next):
        self.value = value
        self.prev = prev
        self.next = next


class DLL:
    head = None

    def add(self, value):
        """ Add a value to the end of the list. """
        # Find the tail
        tail = self.head
        while tail and tail.next:
            tail = tail.next

        if tail:
            # Add a new node with the value
            tail.next = Node(value, tail, None)
        else:
            # Add first node to the list
            self.head = Node(value, None, None)


    def remove_recursive(self, value, node=None):
        """ Remove a node using a recursive algorithm. """
        if node == None:
            node = self.head

        if node.value == value:
            if node.prev:
                node.prev.next = node.next
            else:
                self.head = node.next
            if node.next:
                node.next.prev = node.prev
        elif node.next:
            self.remove_recursive(value, node.next)


    def remove_flat(self, value):
        """ Remove a node using a flat (non-recursive) algorithm. """
        node = self.head
        while node and node.value != value:
            node = node.next

        if node:
            if node.prev:
                node.prev.next = node.next
            else:
                self.head = node.next
            if node.next:
                node.next.prev = node.prev

    def as_list(self):
        """ Convert the linked list into a python list. """
        l = []
        node = self.head
        while node:
            l.append(node.value)
            node = node.next
        return l



##########################
# Test remove_recursive()
##########################

l = DLL()

l.add(1)
l.add(2)
l.add(3)
l.add(4)
l.add(5)

assert(l.as_list() == [1, 2, 3, 4, 5])

# Remove first
l.remove_recursive(1)
assert(l.as_list() == [2, 3, 4, 5])

# Remove middle
l.remove_recursive(4)
assert(l.as_list() == [2, 3, 5])

# Remove last
l.remove_recursive(5)
assert(l.as_list() == [2, 3])


##########################
# Test remove_flat()
##########################

l = DLL()

l.add(1)
l.add(2)
l.add(3)
l.add(4)
l.add(5)

assert(l.as_list() == [1, 2, 3, 4, 5])

# Remove first
l.remove_flat(1)
assert(l.as_list() == [2, 3, 4, 5])

# Remove middle
l.remove_flat(4)
assert(l.as_list() == [2, 3, 5])

# Remove last
l.remove_flat(5)
assert(l.as_list() == [2, 3])

