#!/bin/bash
# Script name: DACR.sh=(Display the contents of Assnw folder or Combine the
#			output of Residual folder)
# Purpose: check the contents of ASSNW Folder or combine the output of residual
# 12th, June, 2014, written by Hu Wu, at IFE, Hannover
# 15th, July, 2014, modified by Hu Wu, at IFE, Hannover

# number of jobs

city_arr=(1 2 3)
k_threshold_arr=(0.8 0.9)
learning_ratio_arr=(0.6 0.7 0.8 0.9 0.8 0.8 0.8)
weight_base_arr=(0.001 0.001 0.001 0.001 0.005 0.01 0.05)
interval_arr=(807 685 548 371 548 548 548 849 709 561 370 561 561 561 2159 1831 1449 1012 1449 1449 1449)

# for each dataset

i=1
j=1
k=1
l=1
NI=3
NJ=2
NK=7
NL=12

TargetF='configs.txt'

while [ $i -le ${NI} ]
do 
	city_index=${city_arr[i-1]}
	while [ $j -le ${NJ} ]
	do 
		k_threshold=${k_threshold_arr[j-1]}
		while [ $k -le ${NK} ]
		do
			learning_ratio=${learning_ratio_arr[k-1]}
			weight_base=${weight_base_arr[k-1]}
			interval=${interval_arr[(i-1)*NK+k-1]}
			while [ $l -le ${NL} ]
			do 

				foldername=$(printf job%01d $(((i-1)*NJ*NK*NL + (j-1)*NK*NL + (k-1)*NL + l )) )
				#echo ${foldername}
				cd ${foldername}
				pwd

				echo ${city_index}','${k_threshold}','${learning_ratio}','${weight_base}','${interval}','${l} > ${TargetF}

				cd ..

				l=$(($l+1))
			done
			k=$(($k+1))
			l=1
		done
		j=$(($j+1))
		k=1
		l=1
	done
	i=$(($i+1))
	j=1
	k=1
	l=1
done

