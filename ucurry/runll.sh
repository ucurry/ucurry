#!/bin/bash

f=$1
echo $f.s

llc -relocation-model=pic -o $f.s $f
cc -o $f.exe $f.s
./$f.exe
