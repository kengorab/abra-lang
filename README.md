# Abra
A small programming language, written in Rust

[![Build Status](https://travis-ci.org/kengorab/abra.svg?branch=master)](https://travis-ci.org/kengorab/abra)

Right now, it's in its very early stages: code flows all the way from the lexer, to the parser, to the
typechecker, to the bytecode compiler, through the interpreter. However, the only datatypes implemented are
integers, floating point numbers, and strings; the only operations implemented are unary (-) and
binary (+, -, *, /). As each new facet is added, it will be added in vertical slices (from the lexer all the
way through to the interpreter).
