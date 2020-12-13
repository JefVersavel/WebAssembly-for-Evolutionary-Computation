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
    dir = "../figsenv/" + name
    os.makedirs(dir)
    for i in range(len(obj) - 1):
        # orgs = np.flipud(np.array(org))
        # ress = np.flipud(np.array(res))
        env = obj[i]
        ones = np.ones((len(env), len(env[0])))
        env = np.flipud(env)
        sns.set()
        fig, ax = plt.subplots()
        ax = sns.heatmap(ones, vmin=0, vmax=largest, annot=env, fmt='', cbar=False, cmap='Blues', linewidths=2)
        plt.xlim(0, len(ones))
        plt.ylim([0, len(ones[0])])
        plt.savefig(dir + "/" + str(i) + ".png")
