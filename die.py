#!/usr/bin/python3

# Simulate a dice roll using coin flips

import random

while True:
    n = 0
    for i in range(3):
        n |= (random.randrange(2) << i)
    if n > 0 and n <= 6:
        break

print(n)
