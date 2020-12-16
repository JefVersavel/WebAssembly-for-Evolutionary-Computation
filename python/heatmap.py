import json
import pathlib
import os

import numpy as np
from matplotlib import pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import seaborn as sns
import pandas as pd

flist = []

for p in pathlib.Path("../jsonEnv/").iterdir():
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


def refactorArray(generalArray):
    orgArray = []

    for row in generalArray:
        orgRow = []
        for org in row:
            displayString = ""
            if len(org) > 0:
                orgList = org.split('_')
                print(orgList)
                displayString = orgList[0][1:len(orgList[0] )- 1] + "\n" + orgList[1] + ", " + orgList[2]
            orgRow.append(displayString)
        orgArray.append(orgRow)

    return orgArray


for (obj, name) in objs:
    print(name)
    largest = 1
    dir = "../figsenv/" + name
    os.makedirs(dir)
    for i in range(len(obj) - 1):
        # orgs = np.flipud(np.array(org))
        # ress = np.flipud(np.array(res))
        env = obj[i]
        env = refactorArray(env)
        ones = np.ones((len(env), len(env[0])))
        env = np.flipud(env)
        sns.set()
        fig, ax = plt.subplots()
        ax = sns.heatmap(ones, vmin=0, vmax=largest, annot=env, fmt='', cbar=False, cmap='Blues', linewidths=2)
        plt.title("Iteration " + str(i))
        plt.xlim(0, len(ones))
        plt.ylim([0, len(ones[0])])
        plt.savefig(dir + "/" + str(i) + ".png")
