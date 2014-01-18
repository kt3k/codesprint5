#! /usr/bin/env python

def main():

    n = input()

    raw = raw_input()

    if DEBUG.is_true:
        print parse(raw)

    start_node = interpret_str(raw)

    result = search_pattern(start_node, '', n)

    if result:
        print result
    else:
        print 'NIL'

class DEBUG: pass

DEBUG.is_true = False

class STATE: pass

STATE.START = 0
STATE.END = 1
STATE.OTHER = 2

class GLOBAL: pass
GLOBAL.counter = 0

class Node:

    def __init__(self, state, char = ''):

        self.state = state
        self.nexts = []
        self.char = char

        GLOBAL.counter += 1

        self.id = GLOBAL.counter

    def appendNext(self, node):

        self.nexts.append(node)

        return self

def parse(chars):

    seq = []
    alt = []

    while chars:

        c, chars = chars[0], chars[1:]

        if c == '(':
            item, chars = parse(chars)
            seq.append(item)

        elif c == ')':

            if alt:
                alt.append(seq)

                return ['|', alt], chars

            return seq, chars

        elif c == '*':
            x = seq.pop()
            seq.append('*')
            seq.append(x)

        elif c == '|':
            # push an alternative
            # and reset current
            alt.append(seq)
            seq = []

        else:
            seq.append(c)

    if alt:
        alt.append(seq)

        return ['|', alt], ''

    return seq, ''

def interpret(seq, node):

    if DEBUG.is_true:
        print seq, node

    while seq:

        s, seq = seq[0], seq[1:]

        if s == '|':

            end_node = Node(STATE.OTHER)

            alts, seq = seq[0], seq[1:]

            for alt_seq in alts:
                alt_node = Node(STATE.OTHER)
                node.appendNext(alt_node)
                interpret(alt_seq, alt_node).appendNext(end_node)

            node = end_node

        elif isinstance(s, list):
            node = interpret(s, node)

        elif s == '*':
            # split t from head
            t, seq = seq[0], seq[1:]

            in_node = Node(STATE.OTHER)
            out_node = Node(STATE.OTHER)

            interpret(t, in_node).appendNext(out_node).appendNext(in_node)

            node.appendNext(out_node)
            node.appendNext(in_node)

            node = out_node

        else: # s is literal

            char_node = Node(STATE.OTHER, s)

            # append char_node
            node.appendNext(char_node)

            node = char_node

    return node

def interpret_str(str):

    start_node = Node(STATE.START)

    end_node = interpret(parse(str), start_node)

    end_node.state = STATE.END

    return start_node

pattern_cache = set()

def search_pattern(node, pattern, size):

    if (node.id, pattern) in pattern_cache:
        return 0

    pattern_cache.add((node.id, pattern))

    if DEBUG.is_true:
        print pattern

    # if reached to char_node then append char to the pattern
    pattern += node.char

    if len(pattern) == size and node.state == STATE.END:
        # found!
        return pattern

    elif len(pattern) > size:
        return 0

    else:
        if len(node.nexts) == 1:
            return search_pattern(node.nexts[0], pattern, size)

        for next in node.nexts:

            result = search_pattern(next, pattern, size)

            if result:
                return result

        return 0


if __name__ == '__main__':
    main()
