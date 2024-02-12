#! /bin/bash

if [[ $# < 3 ]]; then
  echo "$0 [lexer|parser|typechecker] [ok|error] [test_path]"
  exit 0
else
  runner="$1"; shift
  mode="$1"; shift
  test_path="$1"; shift
fi

if [ "$mode" = "error" ]; then
  out_path=`echo "$test_path" | sed 's/\.abra$/\.out/'`
elif [ "$mode" = "ok" ]; then
  out_path=`echo "$test_path" | sed 's/\.abra$/\.out\.json/'`
else
  echo "Illegal mode '$mode', expected 'ok' or 'error'"
fi

../target/debug/abra build -r "./src/$runner.test.abra" -- "$test_path" > "$out_path"
