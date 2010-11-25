import sys



# Determine whether needle is a substring of haystack
def substr(needle, haystack):
    i = 0

    for i in range(len(haystack)-len(needle)+1):
        if is_substr_at(needle, haystack, i):
            return i

    return False


# Test whether needle is a substring of haystack beginning at haystack[pos]
def is_substr_at(needle, haystack, pos):
    i = 0

    while i < len(needle) and pos+i < len(haystack) and needle[i] == haystack[pos+i]:
        i += 1
    
    return i == len(needle)




if len(sys.argv) == 3:
    print substr(sys.argv[1], sys.argv[2])



# This is O(n^2), or rather, O(len(needle)*len(haystack)). Could I make it O(n)?
# Do we really have to go back to the start of a segment after a is_substr_at() test fails?
