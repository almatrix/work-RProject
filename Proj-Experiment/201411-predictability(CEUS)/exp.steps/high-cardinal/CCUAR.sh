#!/bin/bash
# Script name: CCUAR.sh = Create, Copy, Update Assnw or Residual folder
# Purpose: used to create, copy and update the folder ASSNW or RESIDUAL
# 11th, June, 2014, written by Hu Wu
# 14th, July, 2014, modified by Hu Wu
# at IFE, Uni-Hannover

i=2
NJ=6

while [ $i -le ${NJ} ]
do name=$(printf job%01d $i)
    #sleep 1
	pwd
    # copy the targer folder to each data directory
    cp -r job1 ${name}
    i=$(($i+1))
done
