#!/bin/bash

## Testing Functions

# Diffs files specified by args 1 and 2
diff_output(){
    orig=$1
    out=$2

    diff_out=$(diff $orig $out)
    if [ "$diff_out" -eq "" ]; then
        echo "************************"
        echo "ERROR IN TEST $(echo $test | sed 's/\.fry//')"
        echo "************************"
    fi
}


FRY="${FRY_HOME}/fry"

ulimit -t 30

test_dir="../testing/"

tests=$(ls ${test_dir}/*.fry)

for test in $tests; do
    fry -c < $test > test.java
    javac test.java
    java test > ${test}.tmp
    diff_output ${test}.out ${test}.tmp
done



