#!/bin/sh
ret=$(compile_fry.sh $1)
if [ "$(echo $ret | grep Error)" != "" ];then
    echo $ret
    exit 1
fi
java fry
