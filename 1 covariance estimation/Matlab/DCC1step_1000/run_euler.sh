for i in {1..47};
do
	matlab -nosplash -nojvm -nosoftwareopengl -nodesktop -logfile "logs/main_$i.log" -r "try, main; end, quit" &
done