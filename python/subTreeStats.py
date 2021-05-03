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

def getDepth(name: str):
    splitted = name.split('_')
    identifier = "mutationDepth"
    lngth = len(identifier)
    for splt in splitted:
        print(splt)
        try:
            id = splt[0:lngth]
            print(id)
            if (id == identifier) :
                print("true")
                return splt[lngth + 2:len(splt)]
        except:
            continue
    raise ValueError("could not find a depth")


def makeCharts(dir, inc=True):
    flist = []

    for p in pathlib.Path(dir).iterdir():
        if p.is_file():
            print(p)
            flist.append(p)


    for title in ["depths", "sizes"]:
        fig, ax = plt.subplots()
        ax.set_xlabel("iterations")
        ax.set_ylabel(title)
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

            depth = getDepth(name)
            print(depth)

            newDir = dir + "/dir_" + name
            os.makedirs(newDir, exist_ok=True)
            normalData = file.get(title)
            label = "depth multiplyer= " + depth
            incData = makeIncrementList(normalData, inc)
            plt.plot(incData, label=label)

        plt.legend()
        plt.savefig("subTreeMutationExperiment_" + title)
        print("file created")
        plt.close("all")


makeCharts("../subtreemutationtests/post", False)
