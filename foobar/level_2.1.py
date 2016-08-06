# This was an easy exercise. I failed to copy the name or requirements.

def answer(x):
    count = 0
    seen = set([])
    def rev(s):
        return ''.join((s[i] for i in xrange(len(s)-1, -1, -1)))
    for code in x:
        if code not in seen:
            count += 1
            seen.add(code)
            seen.add(rev(code))
    return count

