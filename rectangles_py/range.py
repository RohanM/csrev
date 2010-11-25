class Range:
    def __init__(self, min, max):
        assert(max >= min)
        self.min = min
        self.max = max

    def __eq__(self, other):
        """ Returns equal if ranges overlap. """
        return not (self < other) and not (self > other)
    def __lt__(self, other):
        return self.max < other.min
    def __gt__(self, other):
        return self.min > other.max

    def exactly_equal_to(self, other):
        """ Equal if range is exactly equal to other. """
        return self.min == other.min and self.max == other.max
