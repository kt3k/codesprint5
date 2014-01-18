#! /usr/bin/env python


def main():

    T = input()

    for i in range(T):

        N = input()

        print special_multiple(N)

# generate sequence of positive integers of only 9 and 0 digits
def gen():

    i = 1

    while True:

        # transform binary representation to special 0,9-sequence
        # ex. 99 -> "0b1100011" -> "1100011" -> "9900099" -> 9900099
        yield int(bin(i)[2:].replace('1', '9'))

        i += 1



def special_multiple(m):

    for n in gen():

        if n % m == 0:

            return n

main()
