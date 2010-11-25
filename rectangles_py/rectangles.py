import random
from bst import *
from range import *

# Generate some random rectangles
rectangles = []
for i in range(5):
    x = random.randrange(0, 50)
    y = random.randrange(0, 50)
    width = random.randrange(0, 50)
    height = random.randrange(0, 50)

    rectangles.append([[x, y], [x+width, y+height]])


# Extract horizontal edges from these
edges = []
for r in rectangles:
    edges.append({'x': r[0][0], 'isLeft': True,  'y0': r[0][1], 'y1': r[1][1], 'r': r})
    edges.append({'x': r[1][0], 'isLeft': False, 'y0': r[0][1], 'y1': r[1][1], 'r': r})


# Sort the edges from left to right
edges.sort(key=lambda e: e['x'])


# Find overlapping rectangles
t = BST()
for edge in edges:
    if edge['isLeft']:
        # Find any overlapping rectangles
        overlaps = t.get_all(Range(edge['y0'], edge['y1']))
        for overlap in overlaps:
            print("%s <--> %s" % (edge['r'], overlap['r']))

        # Add to BST
        t.add(Range(edge['y0'], edge['y1']), edge)
    else:
        t.remove(Range(edge['y0'], edge['y1']))
