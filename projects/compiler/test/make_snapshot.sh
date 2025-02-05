#! /bin/bash

if [[ $# < 3 ]]; then
  echo "$0 [lexer|parser|typechecker] [ok|error|error-and-ok] [test_path]"
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
elif [ "$mode" = "error-and-ok" ]; then
  if [ "$runner" = "typechecker" ]; then
    out_path=`echo "$test_path" | sed 's/\.abra$/\.out\.json/'`
    abra "./src/$runner.test.abra" "$test_path" --print-mods-on-err > "$out_path"
    out_path=`echo "$test_path" | sed 's/\.abra$/\.out/'`
  else
    echo "Illegal mode '$mode' for runner '$runner' ('$mode' only available for 'typechecker')"
    exit 1
  fi
else
  echo "Illegal mode '$mode', expected 'ok' or 'error'"
  exit 1
fi

abra "./src/$runner.test.abra" "$test_path" > "$out_path"
