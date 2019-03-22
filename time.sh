#!/usr/bin/env bash

# Usage: ./time.sh my_command
# Outputs time (in milliseconds) of command
# Swallows command outputs

ts=$(gdate +%s%N)
"$@" > /dev/null
tt=$((($(gdate +%s%N) - $ts)/1000000))
echo $tt