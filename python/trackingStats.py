import json
from matplotlib import pyplot as plt
import os
import pathlib
import functools
import operator

foldl = lambda func, acc, xs: functools.reduce(func, xs, acc)


def makeIncrementList(list, inc=True):
    sum = 0
    newList = []
    for e in list:
        try:
            if inc:
                sum += e
            else:
                sum += e
                sum = e
        except:
            if len(e) > 0:
                sum = foldl(operator.add, 0, e) / len(e) 
        newList.append(sum)

    return newList


def saveImages(data, names):
    if len(data) != len(names):
        return

    for i in range(len(data)):
        plt.plot(data[i])
        plt.savefig(names[i])
        plt.close("all")


def makeCharts(dir, inc=True):
    flist = []

    for p in pathlib.Path(dir).iterdir():
        if p.is_file():
            print(p)
            flist.append(p)

    for path in flist:
        plt.ioff()
        file = open(path)
        name = str(path).split("/")[-1]
        if name != ".DS_Store":
            file = file.read()
            print(name)
            file = json.loads(file)
        else:
            continue

        data = []
        names = []
        newDir = dir + "/dir_" + name
        os.makedirs(newDir, exist_ok=True)
        for entry in file.keys():
            normalData = file.get(entry)
            incData = makeIncrementList(normalData, inc)
            data.append(incData)
            newName = newDir + "/" + entry
            names.append(newName)

        saveImages(data, names)


makeCharts("../trackingStats")
makeCharts("../postStats", False)
