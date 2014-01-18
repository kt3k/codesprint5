#! /usr/bin/env python

def main():
    N = input()

    fibo = gen_fibo(10**10)

    for i in range(N):
        T = input()

        if T in fibo:
            print 'IsFibo'
        else:
            print 'IsNotFibo'

def gen_fibo(M):
    n, m, next = 0, 1, 1

    fibo = set()

    while next < M:
        next = n + m
        fibo.add(next)

        n, m = m, next

    return fibo

main()
