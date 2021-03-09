import json
import pathlib
import os
import shutil

import imageio
import numpy as np
from matplotlib import pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import seaborn as sns
import pandas as pd


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
                org = org[0:len(org) - 1]
                orgList = org.split('_')
                displayString = orgList[0] + "\n" + orgList[1] + ", " + orgList[2] + "\n" + orgList[3]
            orgRow.append(displayString)
            # if len(org) > 0:
            #     orgRow.append('x')
            # else:
            #     orgRow.append('')
        resArray.append(resRos)
        orgArray.append(orgRow)

    return orgArray, resArray


def main():
    flist = []
    plt.ioff()

    for p in pathlib.Path("./jsonSyscall/").iterdir():
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

    for (obj, name) in objs:
        images: list = []
        print(name)
        largest = 1
        dir = "./figssyscall/" + name
        os.makedirs("./figssyscall/" + name, exist_ok=True)
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
            plt.subplots()
            sns.heatmap(ress, vmin=0, vmax=largest, annot=orgs, fmt='', cmap='Blues', linewidths=2)
            plt.title("Iteration " + str(i))
            plt.xlim(0, len(res))
            plt.ylim([0, len(res[0])])
            imName: str = dir + "/" + str(i) + ".png"
            plt.savefig(imName, bbox_inches='tight')
            plt.close('all')
            im = imageio.imread(imName)
            images.append(im)

        shutil.rmtree(dir)

        gifDir = "./gifs"
        os.makedirs(gifDir, exist_ok=True)
        gifName: str = gifDir + "/" + name + ".gif"
        imageio.mimwrite(gifName, images, format='.gif', fps=10)


if __name__ == "__main__":
    main()
