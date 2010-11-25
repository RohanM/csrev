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


    # Get all values associated with the specified key
    def get_all(self, key):
        if key == self.key:
            return [self.value] + (self.right.get_all(key) if self.right else [])
        elif key < self.key:
            return self.left.get_all(key) if self.left else []
        else:
            return self.right.get_all(key) if self.right else []


    # TODO: Refactor from here

    def remove(self, key):
        if self.key.exactly_equal_to(key):
            if self.num_children() == 0:
                if self.parent.left == self:
                    self.parent.left = None
                else:
                    self.parent.right = None
            elif self.num_children() == 1:
                if self.parent.left == self:
                    self.parent.left = self.left or self.right
                else:
                    self.parent.right = self.left or self.right
            else:
                # Splice out root's successor, then place its data (key, value) into this node
                successor = self.get_successor()
                if successor.parent.left == successor:
                    successor.parent.left = successor.right
                else:
                    successor.parent.right = successor.right
                self.key = successor.key
                self.value = successor.value
        else:
            if key < self.key and self.left:
                self.left.remove(key)
            elif key > self.key and self.right:
                self.right.remove(key)


    def num_children(self):
        return int(bool(self.left)) + int(bool(self.right))


    def get_successor(self):
        successor = self.right
        while successor and successor.left:
            successor = successor.left
        return successor


    # Display, in-order traversal
    def display(self):
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
        if self.root.key.exactly_equal_to(key):
            if self.root.num_children() == 0:
                self.root = None
            elif self.root.num_children() == 1:
                self.root = self.root.left or self.root.right
            else:
                # Splice out root's successor, then place its data (key, value) into root
                successor = self.root.get_successor()
                if successor.parent.left == successor:
                    successor.parent.left = successor.right
                else:
                    successor.parent.right = successor.right
                self.key = successor.key
                self.value = successor.value
        else:
            self.root.remove(key)

    def vis(self):
        return self.root.vis()



# Tests
from range import *

t = BST()

t.add(Range(1, 1), 'one')
t.add(Range(2, 2), 'two')
t.add(Range(3, 3), 'three')
t.add(Range(4, 4), 'four')
t.add(Range(5, 5), 'five')

t.remove(Range(5, 5))

print(t.vis())
