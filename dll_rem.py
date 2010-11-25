class Node:
    prev = next = None
    value = None

    def __init__(self, value):
        self.value = value

class DLL:
    head = None

    def remove_recursive(self, value, node=None):
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
        node = head
        while node and node.value != value:
            node = node.next

        if node:
            if node.prev:
                node.prev.next = node.next
            else:
                self.head = node.next
            if node.next:
                node.next.prev = node.prev

dll = DLL()

