import json
import pathlib
import os

import numpy as np
from matplotlib import pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import seaborn as sns
import pandas as pd

flist = []

for p in pathlib.Path("../jsonSyscall/").iterdir():
    if p.is_file():
        print(p)
        flist.append(p)

objs = []
for fpath in flist:
    f = open(fpath)
    name = str(fpath).split("/")[-1]
    if name != ".DS_Store":
        file = f.read()
        print(name)
        obj = json.loads(file)
        objs.append((obj, name))


def makeResourceArray(generalArray):
    resArray = []
    orgArray = []

    for row in generalArray:
        orgRow = []
        resRos = []
        for item in row:
            org = item[0]
            res = item[1]
            resRos.append(len(res))
            displayString = ""
            if len(org) > 0:
                org = org[1:]
                org = org[0:len(org) - 1]
                orgList = org.split('_')
                displayString = orgList[0] + "\n" + orgList[1] + ", " + orgList[2] + ", " + orgList[3]
            orgRow.append(displayString)
            # if len(org) > 0:
            #     orgRow.append('x')
            # else:
            #     orgRow.append('')
        resArray.append(resRos)
        orgArray.append(orgRow)

    return orgArray, resArray


for (obj, name) in objs:
    print(name)
    largest = 1
    dir = "../figssyscall/" + name
    os.makedirs("../figssyscall/" + name)
    for env in obj:
        _, res = makeResourceArray(env)
        ress = np.flipud(np.array(res))
        m = np.amax(ress)
        if largest < m:
            largest = m
    for i in range(len(obj) - 1):
        org, res = makeResourceArray(obj[i])
        orgs = np.flipud(np.array(org))
        ress = np.flipud(np.array(res))
        sns.set()
        fig, ax = plt.subplots()
        ax = sns.heatmap(ress, vmin=0, vmax=largest, annot=orgs, fmt='', cmap='Blues', linewidths=2)
        plt.title("Iteration " + str(i))
        plt.xlim(0, len(res))
        plt.ylim([0, len(res[0])])
        plt.savefig(dir + "/" + str(i) + ".png")
