#!/bin/bash
# Script name: DACR.sh = (Display the contents of Assnw folder or Combine the
#			output of Residual folder)
# Purpose: check the contents of ASSNW Folder or combine the output of residual
# 12th, June, 2014, written by Hu Wu, at IFE, Hannover
# 15th, July, 2014, modified by Hu Wu, at IFE, Hannover

# number of jobs
i=1
NJ=5

TargetF='configs.txt'
while [ $i -le ${NJ} ]
    do name=$(printf job%01d $i)
	cd ${name}
	pwd

	echo $((i/2))
	#echo ${i}',120,0.9' > ${TargetF}
#	printf "%4d %4d 5.1f" ${i} 120 0.9

	cd ..
	i=$(($i+1))
    done
