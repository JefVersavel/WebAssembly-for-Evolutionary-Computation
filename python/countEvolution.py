import json
from matplotlib import pyplot as plt
import os
import pathlib
import functools
import operator
import statistics as st


flist = []
dir = "../postStats"

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
        print(p)
        flist.append(p)


flist = sorted(flist)

for path in flist:
    file = open(path)
    name = str(path).split("/")[-1]
    if name != ".DS_Store":
        file = file.read()
        file = json.loads(file)
        counter = counter + 1
    else:
        continue

    normalData = file.get("popGrowth")
    last = normalData[len(normalData) - 1]
    sum = sum + last
    if last == 1:
        ones = ones + 1
    if last <= 3:
        smalls = smalls + 1
    sums.append(last)
    if counter == 50:
        print("\n")
        print(name)
        counter = 0
        print("total= " + str(sum))
        print("median= " + str(st.median(sums)))
        print("ones= " + str(ones))
        print("smalls= " + str(smalls))
        sums = []
        sum = 0
        ones = 0
        smalls = 0

