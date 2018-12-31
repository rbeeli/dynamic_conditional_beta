for i in {1..47};
do
	matlab -nosplash -nojvm -nosoftwareopengl -nodesktop -singleCompThread -logfile "logs/main_$i.log" -r "isShellMode=true; shellID=$i; workerId='EULER'; try, main; end, quit" &
done