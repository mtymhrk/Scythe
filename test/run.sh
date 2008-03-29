#!/bin/sh

for test_suite in $@; do
  echo 
  echo "########## Execute $test_suite ##########"
  echo

  ./$test_suite
done
