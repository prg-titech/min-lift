#!/bin/sh

has_error=0
for file in `ls $1/*.cl`; do
  output=`mktemp`
  if ! ./runtime -q --file $file -d min-data > $output ; then
    echo "compilation failed at $file"
    echo "---------------------------------"
    has_error=1
    continue
  fi

  if ! diff $output `dirname $file`/`basename -s .cl $file`.output ; then
    echo "incorrect output at $file"
    echo "---------------------------------"
    has_error=1
  fi
done

exit $has_error
