# Test of pykythe through 3 iterations, for the pykythe Python source
# (this is a variation of test3.sh)
#   1st one: clean
#   2nd one: using the "batch" results
#   3rd one: same as 2nd -- shouldn't re-do anything

MAKEFILE_DIR="$(realpath $(dirname $0)/..)"
TEST_DATA_DIR="$(realpath $(dirname $0)/../pykythe)"
SUBSTDIR=/tmp/pykythe_test/SUBST  # Same as SUBSTDIR in Makefile
BATCH_ID=BATCH_test3a # See comment in test3.sh
# The order of the targets is important -- __main__ should
# do all the rest, so that their cached results are used.
TARGET=$(echo /tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST${TEST_DATA_DIR}/{__main__,ast_raw,ast_cooked,ast,bootstrap_builtins,pod,typing_debug}.kythe.verifier)

time make --warn-undefined-variables -C ${MAKEFILE_DIR} BATCH_ID=${BATCH_ID} clean_lite
SRCS=
for i in $(find ${MAKEFILE_DIR}/test_data ${MAKEFILE_DIR}/pykythe -name '*.py'); do
    SRCS="${SRCS} ${SUBSTDIR}${i}"
done

set -e -x
# Run the pre-processor on the source files:
make --warn-undefined-variables -C ${MAKEFILE_DIR} BATCH_ID=${BATCH_ID} ${SRCS}

echo "=== Analyze everything (including builtins) ==="
time make --warn-undefined-variables -C ${MAKEFILE_DIR} BATCH_ID=${BATCH_ID} ${TARGET}
echo "=== (end 1) ==="

rm ${TARGET}
echo "=== Analyze ${TARGET} plus its imports ==="
time make --warn-undefined-variables -C ${MAKEFILE_DIR} BATCH_ID=${BATCH_ID} touch-fixed-src clean-batch ${TARGET}
echo "=== (end 2) ==="

echo "=== Analyze unmodified ${TARGET} ==="
time make --warn-undefined-variables -C ${MAKEFILE_DIR} BATCH_ID=${BATCH_ID} touch-fixed-src ${TARGET}
echo "=== (end 3) ==="
make --warn-undefined-variables -C ${MAKEFILE_DIR} BATCH_ID=${BATCH_ID} json-decoded-all
