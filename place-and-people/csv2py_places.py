#! /usr/bin/env python


names = set()

for line in open('counties.csv'):
    line = line.split(',')

    a = line[1].strip('"')
    b = line[3].strip('"')

    names.add(a.lower())
    names.add(b.lower())

for line in open('places.csv'):
    line = line.split(',')

    a = line[1].strip('"')
    b = line[2].strip('"')

    names.add(a.lower())
    names.add(b.lower())

print names
