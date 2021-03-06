#!/bin/bash
# This script automatically takes all of the tokens defined in scanner.mll 
# and populates parser.mly with those tokens

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

echo -e $out | tail -n +2 > tmp_file

lead='^(\* start tokens \*)'
tail='^(\* end tokens \*)'

sed -e "/$lead/,/$tail/{ /$lead/{p; r tmp_file
 }; /$tail/p; d }" $FRY_ROOT/src/parser.mly

rm -f tmp_file
