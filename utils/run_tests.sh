#!/bin/bash

## Testing Functions

print_usage(){
    echo "Usage: "
    echo "      $0 -s <sandbox_dir> -d <test_dir> [-f <comma separated .fry test in test_dir>] [-h]"
}

# Diffs files specified by args 1 and 2
diff_output(){
    orig=$1
    out=$2

    diff $orig $out > ${log_dir}/${test}_diff.log
    diff_ret=$?
    if [ "$diff_ret" -ne "0" ]; then
        report_error "ERROR EXECUTING DIFF"
        return 1
    fi
    if [ "$(cat ${log_dir}/${test}_diff.log | wc -l)" -ne "0" ]; then
        report_error "DIFFERENCE IN OUTPUT"
        cat ${log_dir}/${test}_diff.log
        return 1
    else
        echo "TEST  $(filename $(echo $test | sed 's/\.fry//')) PASSED"
        return 0
    fi
}

# Optionally takes a message as the first argument
report_error(){
        echo "************************"
        echo "ERROR IN TEST $(filename $(echo $test | sed 's/\.fry//'))"
        echo "$1"
        echo "*************************"
}

# Reads log file passed as argument, checks for any errors and reports them if they exist
check_and_report_errors(){
    log_file=$1

    exceptions=$(grep "exception|Exception|EXCEPTION" ${log_file})
    if [ "$(cat ${log_file} | wc -l)" -ne "0" ]; then
        report_error "ERROR IN LOG FILE $log_file"
        return 1
    fi

    return 0

}

for i in $@; do
    case $i in 
        "-s")
            # Sandbox dir
            sandbox_dir=$2
            shift 2
         ;;
        "-h")
            print_usage
            exit 1
         ;;
        "-f")   
            tests=$( filename $2 | sed 's/,/ /')
            shift 2
         ;;
        "-d")
            test_dir=${2}
            shift 2
        ;;
    esac
done

if [ -z "${test_dir}" ]; then
    echo "Must specify test directory"
    print_usage
    exit 1
fi

if [ -z "${sandbox_dir}" ]; then
    echo "Must specify sandbox directory"
    print_usage
    exit 1
fi

if [ -z "${tests}" ]; then
    tests=$(ls ${test_dir}/*.fry)
    root_output_dir="$(pwd)/fry_tests_$(date +%m%d_%H%M)"
else
    root_output_dir="$(pwd)/"
fi

FRY="${FRY_HOME}/src/fry"

ulimit -t 30

root_log_dir="${root_output_dir}/logs"

mkdir -p ${root_log_dir}

cd ${root_output_dir}

for test_src in $tests; do
    test=$(filename $(echo $test_src | sed 's/\.fry//'))
    
    echo ""
    echo "Running test ${test}..."

    log_dir="${root_log_dir}/${test}/"
    output_dir="${root_output_dir}/${test}/"

    mkdir -p $log_dir
    mkdir -p $output_dir

    # copy over files for test
    cp -f ${test_dir}/${test}.fry $output_dir
    cp -f ${test_dir}/${test}.out $output_dir
    
    error=0
    # Translate to java
    fry -c < ${output_dir}/${test}.fry > test.java 2>${log_dir}/${test}_trans.log
    
    check_and_report_errors ${log_dir}/${test}_trans.log
    error=$?
    if [ "${error}" -ne "0" ]; then
        continue
    fi

    javac test.java 2>${log_dir}/${test}_javac.log
    error=$(cat ${log_dir}/${test}_javac.log | wc -l)
    if [ "${error}" -ne "0" ]; then
        report_error
        continue
    fi

    java test > ${output_dir}/${test}.tmp
    diff_output ${output_dir}/${test}.out ${output_dir}/${test}.tmp
    error=$?
    if [ "${error}" -eq "0" ]; then
        rm -f ${log_dir}/${test}_trans.log
        rm -f ${log_dir}/${test}_javac.log
        rm -f ${log_dir}/${test}_diff.log
    fi
    
    echo "Finished test ${test}..."
    echo ""
done

cd ..

