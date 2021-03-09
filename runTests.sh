#!/bin/sh

stack build
stack eval "ALife.testCreature"
python3 python/heatmap.py
