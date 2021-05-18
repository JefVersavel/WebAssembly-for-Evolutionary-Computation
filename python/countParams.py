import json
from matplotlib import pyplot as plt
import os
import pathlib
import functools
import operator
import statistics as st


flist = []
dir = "../postGeneralNew"

sum = 0
counter = 0
sums = []
ones = 0
smalls = 0

files = os.listdir(dir)
    
files = sorted(files)


flist = []

for p in pathlib.Path(dir).iterdir():
    if p.is_file():
        flist.append(p)


flist = sorted(flist)
for f in flist:
    print(f)

for path in flist:
    file = open(path)
    name = str(path).split("/")[-1]
    if name != ".DS_Store":
        file = file.read()
        file = json.loads(file)
        counter = counter + 1
    else:
        continue

    zeros = []
    ones = []
    twos = []
    threes = []
    fours = []
    fives = []
    normalData = file.get("nrParams")
    for l in normalData:
        zero = 0
        one = 0
        two = 0
        three = 0
        four = 0
        five = 0
        for n in l:
            if n == 0:
                zero = zero + 1
            elif n == 1:
                one = one + 1
            elif n == 2:
                two = two + 1
            elif n == 3:
                three == three + 1
            elif n == 4: 
                four = four + 1
            elif n== 5:
                five = five + 1
            else:
                continue

        zeros.append(zero)
        ones.append(one)
        twos.append(two)
        threes.append(three)
        fours.append(four)
        fives.append(five)

    plt.ioff()
    fig, ax = plt.subplots()
    ax.set_xlabel("Iterations")
    ax.set_ylabel("#organisms")
    plt.plot(zeros, label="0 parameters")
    plt.plot(ones, label="1 parameter")
    plt.plot(twos, label="2 parameters")
    plt.plot(threes, label="3 parameters")
    plt.plot(fours, label="4 parameters")
    plt.plot(fives, label="5 parameters")
    plt.legend()
    plt.savefig(dir + "/" + name + ".png")
