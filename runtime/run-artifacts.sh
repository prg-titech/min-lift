#!/bin/sh

has_error=0
for file in `ls $1/*.cl`; do
  if ! ./runtime --file $file -c -q ; then
    echo "compilation failed at $file"
    has_error=1
  fi
done

exit $has_error
