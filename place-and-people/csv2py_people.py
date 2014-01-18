#! /usr/bin/env python


names = set()

for line in open('surnames.csv'):
    line = line.split(',')

    a = line[0]

    names.add(a.lower())

print names
