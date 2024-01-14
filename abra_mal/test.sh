declare -a testFiles=(
#  'step0_repl'
#  'step1_read_print'
#  ^ These 2 tests are not runnable because their assertions are meant to be run against
#    earlier stages of the mal implementation, prior to the implementation of evaluation.
  'step2_eval'
  'step3_env'
  'step4_if_fn_do'
  'step5_tco'
  'step6_file'
  'step7_quote'
  'step8_macros'
  'step9_try'
)

src_dir=$1
echo "Using src dir '$src_dir'"

for f in "${testFiles[@]}"; do
  echo "************************"
  echo "Running $f..."
  echo "************************"

  ./runtest.py --rundir "$src_dir" "../tests/$f.mal" -- ./run
  if [ $? -ne 0 ]; then
    echo "Test $f failed, abort"
    exit 1
  fi

  if [ $f == "step6_file" ]; then
    cd "$src_dir"

    ../tests/run_argv_test.sh ./run
    if [ $? -ne 0 ]; then
      echo "Command run_argv_test failed, abort"
      exit 1
    fi

    cd ..
  fi

done

echo "All tests passed, looking good!"
