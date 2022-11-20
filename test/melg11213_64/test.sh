#!/bin/bash
set -eu pipifail



# STEP.00.01
# setting of variables

readonly TEST_TARGET=melg11213-64

readonly FILE_EXE="./${TEST_TARGET}.exe"

readonly FILE_RESULT_REF="../../reference/${TEST_TARGET}/${TEST_TARGET}.txt"
readonly FILE_RESULT_TGT="./${TEST_TARGET}.txt"



# STEP.00.02
# setting of functions

compare_result() {

    echo
    diff -qs $FILE_RESULT_TGT $FILE_RESULT_REF

    if [[ ($? -eq 0) && (-e $FILE_RESULT_TGT ) ]]; then
        rm $FILE_RESULT_TGT
    fi

}



# STEP.01
# execute the test using DEBUG mode
time make debug_mode -B
time $FILE_EXE>$FILE_RESULT_TGT
compare_result



# STEP.02
# execute the test using RELEASE mode
time make release_mode -B
time $FILE_EXE>$FILE_RESULT_TGT
compare_result
