import json
import pathlib
import matplotlib.pyplot as plt
import os

flist = []
for p in pathlib.Path("../json/").iterdir():
    if p.is_file():
        print(p)
        flist.append(p)


def draw(i, obj, dirname):
    fig, ax = plt.subplots()
    current = obj[i]
    labels = []
    data = []
    for item in current:
        labels.append(item[0])
        data.append(item[1])
    ax.pie(data, labels=labels, autopct='%1.1f%%')
    ax.axis('equal')
    plt.title("iteration " + str(i))
    fig.savefig(dirname + "/" + str(i))


for fName in flist:
    f = open(fName)
    name = str(fName).split('/')[-1]
    if name != ".DS_Store":
        file = f.read()
        obj = json.loads(file)
        print(name)
        dirname = "../figs/" + name
        os.mkdir(dirname)

        for i in range(len(obj)):
            draw(i, obj, dirname)
