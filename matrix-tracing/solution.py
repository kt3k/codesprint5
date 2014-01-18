#! /usr/bin/env python

MODULO = 10 ** 9 + 7

def main():

    N = input()

    for i in range(N):

        m, n = map(int, raw_input().split())

        print trace_route(m, n)

def trace_route(m, n):
    m -= 1
    n -= 1

    x = m + n

    # numerator / bumbo
    k = 1

    # denominator / bunshi
    d = 1

    # calculate fact(m + n) / fact(m) / fact(n) efficiently
    for i in range(1, min(m, n) + 1):

        k = k * x % MODULO
        d = d * i % MODULO

        x -= 1

    # because MODULO is a prime, d ** (MODULO - 2) is the inverse of d in MODULO field

    return k * pow(d, (MODULO - 2), MODULO) % MODULO

main()
