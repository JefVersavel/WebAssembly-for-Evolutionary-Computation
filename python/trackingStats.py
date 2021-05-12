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


def saveImages(data, names, entries):
    if len(data) != len(names):
        return

    for i in range(len(data)):
        fig, ax = plt.subplots()
        ax.set_xlabel("iterations")
        ax.set_ylabel(entries[i])
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
        entries = []
        for entry in file.keys():
            normalData = file.get(entry)
            incData = makeIncrementList(normalData, inc)
            data.append(incData)
            newName = newDir + "/" + entry
            if entry == "ancestor1Diff":
                entry = "matching % to ancestor"
            if entry == "ancestor2Divv":
                entry = "matching % to generator"
            if entry == "nrParams":
                entry = "#parameters"
            if entry == "popGrowth":
                entry = "population total"
            if entry == "diversity":
                entry = "matching %"
            if entry == "resourceGrowth":
                entry = "resource total"
            entries.append(entry)
            names.append(newName)

        saveImages(data, names, entries)


def main():
    makeCharts("../trackingGeneralBig")
    makeCharts("../postGeneralBig", False)

if __name__ == "__main__":
    main()
