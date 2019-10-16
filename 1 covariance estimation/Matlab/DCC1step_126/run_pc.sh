for i in {1..10};
do
	matlab -nosplash -nojvm -nosoftwareopengl -nodesktop -logfile "logs/$i" -r "try, main; end, quit" &
done