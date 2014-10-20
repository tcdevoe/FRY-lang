#!/bin/bash

# FRY src dir is in the level above utils
tokens=$(cat ${FRY_ROOT}/src/scanner.mll | grep -oP "{ (.*) }" | sed 's/{//g' | sed 's/}//g')

count=0
out="%token"
for token in $tokens; do
    count=$(($count+1))
    out="$out $token"

    if [ $(($count % 6)) -eq 0 ]; then
        out="$out\n%token"
    fi
done

out_tokens=$(echo -e $out | tail -n +2)


