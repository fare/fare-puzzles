

def answer(chunk, word):
    l = len(chunk) ; assert l
    w = len(word) ; assert w

    def letter_index(l):
        return ord(l)-ord('a')
    letter_positions = [[] for _ in range(0, 26)]
    for i in range(0, w):
        letter = letter_index(word[i])
        assert 0 <= letter < 26
        letter_positions[letter].append(i)

    parse = [None for i in range(0, l)]
    best = ''
    reductions = [0] + [None for i in range(1, w)]

    def is_better_answer(a,b):
        return len(a) < len(b) or (len(a) == len(b) and a < b)
    for i in range(0, l):
        previous_best, previous_reductions = parse[i] = (best, reductions)
        best = previous_best + chunk[i]
        reductions = [None for r in range(0, w)]
        reductions[0] = i + 1
        positions = letter_positions[letter_index(chunk[i])]
        for p in positions:
            reduced = previous_reductions[p]
            if p == w-1:
                if reduced is not None:
                    candidate = parse[reduced][0]
                    if is_better_answer(candidate, best):
                        best = candidate
                        for j in range(0, w):
                            r = parse[reduced][1][j]
                            if r is not None:
                                reductions[j]=r
            else:
                reductions[p+1] = reduced
    return best


def check(args, expected):
    actual = answer(*args)
    if actual == expected:
        print 'correct answer%r got expected %r' % (args, expected)
    else:
        print 'WRONG answer%r expected %r but got %r' % (args, expected, actual)

TEST_CASES = [
    ("olclloo", "lo", "olc"),
    ("lloo", "lo", ""),
    ("aaaaa", "a", ""),
    ("canaan", "a", "cnn"),
    ("lolo", "lo", ""),
    ("llolol", "lol", ""),
    ("goodgooogoogfogoood", "goo", "dogfood"),
    ("lololollol", "lolol", ""),
    ("lololololo", "lol", "looo"),
    ("ololololol", "olo", "llol"),
    ("lololololololol", "lolol", "loloo"),
    ("lllllloololololllollo", "llo", "lol"),
    ("banana", "an", "ba"),
    ("aaa", "a", ""),
    ("aaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaa", "")
]

def tests(test_cases=TEST_CASES):
    for (chunk, word, expected) in test_cases:
        check((chunk, word), expected)

tests()
