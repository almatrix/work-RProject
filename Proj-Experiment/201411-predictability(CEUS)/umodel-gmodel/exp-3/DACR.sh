#!/bin/bash
# Script name: DACR.sh = (Display the contents of Assnw folder or Combine the
#			output of Residual folder)
# Purpose: check the contents of ASSNW Folder or combine the output of residual
# 12th, June, 2014, written by Hu Wu, at IFE, Hannover
# 15th, July, 2014, modified by Hu Wu, at IFE, Hannover

# number of jobs
i=1
j=1
NI=12
NJ=5

interval_array[1]=377
interval_array[2]=557
interval_array[3]=722
interval_array[4]=821
interval_array[5]=921

ratio_array[1]=0.9
ratio_array[2]=0.8
ratio_array[3]=0.7
ratio_array[4]=0.6
ratio_array[5]=0.5

base=0.001
#base_array[1]=0.001
#base_array[2]=0.005
#base_array[3]=0.01
#base_array[4]=0.05
#base_array[5]=0.1

TargetF='configs.txt'

while [ $j -le ${NJ} ]
do 
	ratio=${ratio_array[j]}
	interval=${interval_array[j]}
	while [ $i -le ${NI} ]
	do
		name=$(printf job%01d $((j*NI-NI+i)))
		cd ${name}
		pwd

		#printf "%1d,%5d," 1 10000 > ${TargetF}
		#echo | awk -v x=$i -v y=$base -f ../program.awk >> ${TargetF}
		#printf ",%.2f\n" 0.02 >> ${TargetF}
		
		echo ${i}','${interval}','${ratio}','${base} > ${TargetF}
	#	printf "%4d %4d 5.1f" ${i} 120 0.9

		cd ..
		i=$(($i+1))
	done
	j=$(($j+1))
	i=1
done
