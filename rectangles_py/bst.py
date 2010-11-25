class Node:
    left = None
    right = None
    parent = None

    def __init__(self, key, value, parent):
        self.key = key
        self.value = value
        self.parent = parent


    def add(self, key, value):
        if key < self.key:
            if self.left == None:
                self.left = Node(key, value, self)
            else:
                self.left.add(key, value)
        else:
            if self.right == None:
                self.right = Node(key, value, self)
            else:
                self.right.add(key, value)


    def get(self, key):
        if key == self.key:
            return self.value
        elif key < self.key:
            return self.left.get(key) if self.left else None
        else:
            return self.right.get(key) if self.right else None


    def get_all(self, key):
        """ Get all values associated with the specified key """
        if key == self.key:
            return [self.value] + (self.right.get_all(key) if self.right else [])
        elif key < self.key:
            return self.left.get_all(key) if self.left else []
        else:
            return self.right.get_all(key) if self.right else []


    def remove(self, key):
        """
        Remove a node from the tree.
        Returns False if the root node does not need modifying,
        otherwise returns the new root node.
        """
        root = False

        if self.key.exactly_equal_to(key):
            # Determine which node to splice out
            if not self.left or not self.right:
                splicee = self
            else:
                splicee = self.get_successor()

            # Find the non-nil child of this node (if any)
            splicer = splicee.left or splicee.right

            # Splice out the node
            if splicer:
                splicer.parent = splicee.parent
            if splicee.parent == None:
                root = splicer
            elif splicee == splicee.parent.left:
                splicee.parent.left = splicer
            else:
                splicee.parent.right = splicer

            # If the successor was spliced out, copy its data to the note to remove
            if splicee != self:
                self.key = splicee.key
                self.value = splicee.value

        else:
            if key < self.key and self.left:
                root = self.left.remove(key)
            elif key > self.key and self.right:
                root = self.right.remove(key)

        return root


    def num_children(self):
        return int(bool(self.left)) + int(bool(self.right))


    def get_successor(self):
        successor = self.right
        while successor and successor.left:
            successor = successor.left
        return successor


    def display(self):
        """ Display the tree using an in-order traversal. """
        if self.left:
            self.left.display()
        print(self.value)
        if self.right:
            self.right.display()


    def vis(self):
        return "[ %s --> %s, %s ]" % (self.value,
                                      self.left.vis() if self.left else "-",
                                      self.right.vis() if self.right else "-")




class BST:
    root = None

    def add(self, key, value):
        if self.root:
            self.root.add(key, value)
        else:
            self.root = Node(key, value, None)

    def get(self, key):
        return self.root.get(key) if self.root else None

    def get_all(self, key):
        return self.root.get_all(key) if self.root else []

    def remove(self, key):
        new_root = self.root.remove(key)
        if new_root != False:
            self.root = new_root

    def vis(self):
        return self.root.vis() if self.root else ""



# Tests
#from range import *
#
#t = BST()
#
#t.add(Range(1, 1), 'one')
#t.add(Range(4, 4), 'four')
#t.add(Range(2, 2), 'two')
#t.add(Range(3, 3), 'three')
#t.add(Range(5, 5), 'five')
#
#t.remove(Range(1, 1))
#
#print(t.vis())
