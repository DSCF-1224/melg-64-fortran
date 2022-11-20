#!/bin/bash
set -eu pipifail

readonly TEST_TARGET=melg19937-64

readonly FILE_SRC="./${TEST_TARGET}.c"
readonly FILE_EXE="./${TEST_TARGET}.exe"

readonly FILE_RESULT="./${TEST_TARGET}.txt"

gcc $FILE_SRC -o $FILE_EXE -O3 -Wall
$FILE_EXE>$FILE_RESULT
