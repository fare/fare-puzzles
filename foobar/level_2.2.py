# This was an easy exercise. I failed to copy the name or requirements.

def answer(intervals):
    total = [0]
    start = -1
    end = -1
    def flush():
        total[0] += end - start
    for [x, y] in sorted(intervals, key = lambda (x,y): x):
        if x > end:
            flush()
            start = x
            end = y
        elif y > end:
            end = y
    flush()
    return total[0]



print answer([[10, 14], [4, 18], [19, 20], [19, 20], [13, 20]])

