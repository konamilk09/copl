#!/bin/sh

list=($(echo `ls t*.txt`))

for file in ${list[@]}
do
    cat $file | ./ml3.o
done
