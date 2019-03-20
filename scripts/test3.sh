# Test of pykythe through 3 iterations:
#   1st one: clean
#   2nd one: using the "batch" results
#   3rd one: same as 2nd -- shouldn't re-do anything

# Depending on the source file, additional outputs will be created --
# see test3a.sh for an example.

set -e -x
MAKEFILE_DIR="$(realpath $(dirname $0)/..)"
TEST_DATA_DIR="$(realpath $(dirname $0)/../test_data)"
# Override the automatically generated batch_id, so that the batch
# caching can be tested (if this isn't set, each invocation of
# Makefile gets a new batch_id).
BATCH_ID=BATCH_test3

TARGET0=t0
TARGET=/tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST${TEST_DATA_DIR}/${TARGET0}.kythe.verifier

echo "=== Analyze everything (including builtins) ==="
time make -C ${MAKEFILE_DIR} BATCH_ID=${BATCH_ID} clean_lite      ${TARGET}
echo "=== (end 1) ==="

find /tmp/pykythe_test/KYTHE -name ${TARGET0}'*' -delete
echo "=== Analyze ${TARGET} plus its imports ==="
time make -C ${MAKEFILE_DIR} BATCH_ID=${BATCH_ID} touch-fixed-src clean-batch ${TARGET}
echo "=== (end 2) ==="

echo "=== Analyze unmodified ${TARGET} ==="
time make -C ${MAKEFILE_DIR} BATCH_ID=${BATCH_ID} touch-fixed-src ${TARGET}
echo "=== (end 3) ==="
make -C ${MAKEFILE_DIR} BATCH_ID=${BATCH_ID} json-decoded-all
