for ((i = 1; i<=40; i++))
do
FolderName=$(printf JOB%01d $i)
echo ${FolderName}
cp MingRegress.R ${FolderName}
done
