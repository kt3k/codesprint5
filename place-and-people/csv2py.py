#! /usr/bin/env python


names = set()

for line in open('counties.csv'):
    line = line.split(',')

    a = line[1].strip('"')
    b = line[3].strip('"')

    names.add(a)
    names.add(b)

for line in open('places.csv'):
    line = line.split(',')

    a = line[1].strip('"')
    b = line[2].strip('"')

    names.add(a)
    names.add(b)

print names
