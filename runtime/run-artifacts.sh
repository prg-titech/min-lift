#!/bin/sh

has_error=0
for path in `ls $1/*.cl`; do
  output=`mktemp`

  input_size=`head -n 1 $path | cut -c 4- | jq .InputSize`
  input_data="min-data"
  for i in `seq 2 $input_size`; do
    input_data="$input_data,min-data"
  done

  if ! ./runtime -q --file $path -d $input_data > $output ; then
    echo "compilation failed at $path"
    echo "---------------------------------"
    has_error=1
    continue
  fi

  if ! diff $output `dirname $path`/`basename -s .cl $path`.output ; then
    echo "incorrect output at $path"
    echo "---------------------------------"
    has_error=1
  fi
done

exit $has_error
