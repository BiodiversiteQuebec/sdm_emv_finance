
# nohup bash run.sh > nohup.out 2>&1 &

#rm nohup.out
#touch nohup.out
rm verbose.out
nbruns=1
for i in $(seq 1 1 $nbruns)
do
  #echo "Run $i/$nbruns"
  #TZ=":America/New York" date +'%a, %b %d, %Y  %r'
  nohup Rscript run.r --no-save > verbose.out 2>&1 || true & # the || true is to keep going if there is an error
  wait $!
  e=date
  echo "$e -- done"
  #ls -1tlh /data/sdm_rbq/sdms | head -5
  #echo "Run $i/$nbruns done"
done  