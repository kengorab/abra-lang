# Abra
A small programming language, written in Rust

[![Build Status](https://travis-ci.org/kengorab/abra-lang.svg?branch=master)](https://travis-ci.org/kengorab/abra-lang)

This project is very much a work in progress: you can check the [documentation site](https://abra.kenrg.co) for more information

## What's it look like?
It should look familiar, a lot of inspiration was drawn from modern languages like Swift and Kotlin:

```swift
func fib(n: Int): Int {
  if (n == 0) {
    0
  } else if (n == 1) {
    1
  } else {
    fib(n - 2) + fib(n - 1)
  }
}

println(fib(10))
```

You can also see and play with more examples on the [Try It Out](https://abra.kenrg.co/try) page of the language documentation site.
