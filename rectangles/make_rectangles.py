import sys
import random

num_rectangles = int(sys.argv[1])

for i in range(num_rectangles):
    x = random.randrange(0, 100)
    y = random.randrange(0, 100)
    width = random.randrange(0, 100)
    height = random.randrange(0, 100)

    print("%d %d %d %d" % (x, y, x+width, y+height))
