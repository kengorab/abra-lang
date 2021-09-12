# Abra MAL

This directory is an implementation of the wonderful [mal](https://github.com/kanaka/mal) language project. The files
in the `src/` directory are the Abra implementation, but the `tests/` directory is copied straight from
[the mal repo](https://github.com/kanaka/mal/tree/master/impls/tests) (along with
[`runtest.py`](https://github.com/kanaka/mal/blob/master/runtest.py)).

This project has 2 purposes:
1. This is a forcing function for Abra feature development. Working on this project has highlighted gaps in the language
and has inspired a lot of recent feature development.
2. This also serves as a large-scale regression test. The `test.sh` file runs all of the `mal` test files against the 
Abra mal implementation.
