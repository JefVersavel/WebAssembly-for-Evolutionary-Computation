import json
from matplotlib import pyplot as plt
import os
import pathlib
import functools
import operator
import statistics as st
from operator import add
import functools


flist = []
dir = "../postMutationComparison"

foldl = lambda func, acc, xs: functools.reduce(func, xs, acc)

def makeIncrementList(list, inc=True):
    sum = 0
    newList = []
    for e in list:
        if len(e) > 0:
            sum = foldl(add, 0, e) / len(e) 
        else:
            sum = 0
        newList.append(sum)

    return newList

counter = 0

addedList = [0 for i in range (0,1001)]

dict = {}

files = os.listdir(dir)
    
files = sorted(files)


flist = []

for p in pathlib.Path(dir).iterdir():
    if p.is_file():
        flist.append(p)


flist = sorted(flist)
fig, ax = plt.subplots()
ax.set_xlabel("iterations")
ax.set_ylabel("ancestorDiversity")

for path in flist:
    file = open(path)
    name = str(path).split("/")[-1]
    if name != ".DS_Store":
        file = file.read()
        file = json.loads(file)
        spaces = name.split(" ")
        firstName = spaces[0]
        rate = spaces[1]
        counter = counter + 1
    else:
        continue

    normalData = file.get("ancestor1Diff")
    normalData = makeIncrementList(normalData)

    if counter < 30:
        addedList = [addedList[i] + normalData[i] for i in range(len(addedList))]
    elif counter == 30:
        addedList = [addedList[i] + normalData[i] for i in range(len(addedList))]
        print(firstName)
        print(rate)
        averageList = [addedList[i] / 30 for i in range(len(addedList))]
        label = "rate = 1/" + rate 
        plt.plot(averageList, label=label)
        dict[rate] = averageList
        addedList = [0 for i in range(0,1001)]
        counter = 0

plt.legend()
plt.savefig("mixed 3_4 ancestor")


