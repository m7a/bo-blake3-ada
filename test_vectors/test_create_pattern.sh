#!/bin/sh -eu
# Script to create test pattern 1.0.0, (c) 2022 Ma_Sys.ma <info@masysma.net>

j=0
while [ $j -le 412 ]; do
	i=0
	while [ $i -le 250 ]; do
		octal="$(printf "%03o" "$i")"
		printf "\\$octal"
		i=$((i + 1))
	done
	j=$((j + 1))
done
