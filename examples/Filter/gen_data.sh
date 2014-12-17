#!/bin/bash

for i in $(seq 1 100); do 
    date=$(date -d "$((RANDOM%1+2010))-$((RANDOM%12+1))-$((RANDOM%28+1)) $((RANDOM%23+1)):$((RANDOM%59+1)):$((RANDOM%59+1))" '+%d-%m-%Y %H:%M:%S');
    rand1=$(($RANDOM % 4));
    rand2=$(($RANDOM % 4));
    case $rand1 in
        0 ) 
            site="facebook.com"
            ;;
        1 )
            site="google.com"
            ;;
        2 )
            site="linkedin.com"
            ;;
        3 )
            site="twitter.com"
            ;;
    esac 
    case $rand2 in
        0 )
            user="Frodo"
            ;;
        1 )
            user="Sam"
            ;;
        2 )
            user="Merry"
            ;;
        3 )
            user="Pippen"
            ;;
    esac
    echo "$date|$site|$user" >> website_hits.txt
done
