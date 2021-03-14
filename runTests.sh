#!/bin/sh

stack build
stack eval "Main.main"
