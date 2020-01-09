#!/bin/sh

help() {
  echo "$0 [lift|hand|seq]"
  exit 1
}

if [ $# -ne 1 ]; then
  help
fi

kind="$1"
case "$kind" in
  lift ) ;;
  hand ) ;;
  seq  ) ;;
  * ) help ;;
esac

result_file="result-$kind.txt"

rm $result_file
touch $result_file

for i in `seq 100`; do
  ./runtime/runtime -k pro2019-4/filter-gt-0.5-twice-$kind.cl -d runtime/data -p >> $result_file
done

cat $result_file | awk '{m+=$1} END{print m/NR "us"}'

