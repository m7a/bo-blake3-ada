#!/bin/sh -eu
# Script to process test vectors 1.0.0, (c) 2022 Ma_Sys.ma <info@masysma.net>
jq -r '.cases | .[] | [.input_len, .hash, .keyed_hash] | @tsv' < \
			z_test_vectors.json | tr '\t' ',' > test_vectors.csv
