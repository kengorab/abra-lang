# Abra
A small statically-typed compiled programming language.

Originally written in Rust, but now self-hosted (the compiler is written in itself). The original Rust-based implementation
has been extracted out and can be found [here](https://github.com/kengorab/abra-lang-old) for historical reference.

![Test](https://github.com/kengorab/abra-lang/workflows/Test/badge.svg)
![Release](https://github.com/kengorab/abra-lang/workflows/Release/badge.svg)

This project is very much a work in progress: you can check the [documentation site](https://abra.kenrg.co) for more information

## Getting Started
Download the latest `abra` binary from the [Releases](https://github.com/kengorab/abra-lang/releases/latest) page, and
place it wherever you want on your system (e.g. `~/.abra`). You will then need to export an `$ABRA_HOME` environment
variable which points to the `~/.abra/std/` directory.
You should then be able to run
```swift
// example.abra
printlnv2("Hello world")
```
```sh
$ abra example.abra
Hello world
```

## What's it look like?
It should look familiar, a lot of inspiration was drawn from modern languages like Swift and Kotlin:

```swift
func fib(n: Int): Int {
  if n == 0 {
    0
  } else if n == 1 {
    1
  } else {
    fib(n - 2) + fib(n - 1)
  }
}

printlnv2(fib(10))
```

You can also see and play with more examples on the [Try It Out](https://abra.kengorab.dev/try) page of the language documentation site.
