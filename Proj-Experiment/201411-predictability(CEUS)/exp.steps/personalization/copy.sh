#!/bin/bash
# Script name: copy.sh
# Purpose: copy the run.job files to each sub directory

echo "used to copy the run.job files"
echo 

TargetF='job'

for ((i=1; i<=504; i++))
do FolderName=$(printf job%01d $i)
  echo ${FolderName}
#  sleep 1
  
  cp run.job ${FolderName}

done

#for ((i=16; i<=35; i++))
#do FolderName=$(printf JOB%01d $i)
 # echo ${FolderName} >> output.log
 # cat ${FolderName}/output.log >> output.log
 # echo >> output.log
 # echo >> output.log

  #echo ${FolderName} >> timeinfo
  #echo >> timeinfo
  #cat ${FolderName}/Ming_R_* >> timeinfo
  #echo >> timeinfo
  #echo >> timeinfo
#done
