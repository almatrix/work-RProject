#!/bin/bash
# Script name: QJAR.sh (Qsub Jobs of Assnw or Residual)
# Purpose: used to qsub the jobs

echo "used to qsub the jobs (in total, 10 jobs)"
echo 

TargetF='job'
Job='Ming_data_prep_'

for ((i=1; i<=6; i++));
do FolderName=$(printf job%01d $i)
    cd ${FolderName}
    pwd
    #sleep 1
    
    JobName=$(printf ${Job}%01d $i)
    #echo ${JobName}
    qsub -N ${JobName} *.job;
    
    cd ..;
done
