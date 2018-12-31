for i in {1..10};
do
	matlab -nosplash -nojvm -nosoftwareopengl -nodesktop -singleCompThread -logfile "logs/main_$i.log" -r "isShellMode=true; shellID=$i; workerId='PC'; try, main; end, quit" &
done