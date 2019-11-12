# Run a test on a single test_data file
set -x

# F=imports_dir1/i1_sub/i4
# F=imports_dir1/i1
# F=dummy_dir/dummy_file
F=t10  # t9
rm -f /tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/$F.kythe.{entries,json,json-decodedverifier}
time make -k -C /home/peter/src/pykythe BATCH_ID=BBB  \
     /tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/$F.kythe.verifier \
     /tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/$F.kythe.json-decoded
