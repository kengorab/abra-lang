# Todos for LLVM Compiler v2 (in no particular order)

- [ ] tuple destructuring
- [ ] array destructuring
  - [ ] including `*spread` syntax
  - [ ] string destructuring
- [x] enums
- [x] match statement/expression
  - [x] `None` case
  - [x] wildcard case
  - [x] constant case
  - [x] type case (incl. enums)
  - [x] destructuring tagged union enum variants
- [ ] match statement on `Any`/generic type
- [x] imports/exports
- [x] I/O and other low-level things
- [x] module alias as a type
- [ ] error on vars used before initialization (Requires Typechecker)
- [ ] initializers/constructors and/or fields w/ default values that can reference `self` (Requires Typechecker)
- [ ] referencing a method as function value
- [ ] traits (Requires Typechecker, Parser, Lexer)
- [ ] `..` range expression (Requires Typechecker, Parser, Lexer)
- [x] unary negate for Option type, returns `true` if value is `None` (Requires Typechecker)
- [x] growing array when capacity reached
- [x] growing map when capacity/loadFactor reached
- [ ] unsigned integers of different sizes (Requires Typechecker)
- [x] `Result<V, E>` enum
- [ ] `try` expressions (leveraging the `Result<V, E>` enum) (Requires Typechecker)
- [ ] more efficient string interpolation (right now it casts everything to `Any` to call `String#concat` which sucks)
- [ ] better `print`/`println` - don't wrap everything in `Any`, special logic which just `toString`s args beforehand?
- [x] `Process` builtin
  - [x] environment variables
  - [x] program arguments
- [x] memory management
  - [x] garbage collection?
  - [x] use GC_malloc_atomic for char*'s
  - ~~[ ] reference counting?~~
- [ ] name collisions
  - [ ] for bound c-functions
  - [ ] for enum tagged union variant constructors
- [ ] exported funcs which capture exports (also if the capture _isn't_ exported)
  - ```
    // example2.abra
    export var B = 4
    export func incrB(by: Int) {
      B += by
    }
    
    // example.abra
    import makeAdder, B, incrB from "./example2"
    println(B)
    incrB(by: 5)
    println(B)
  ```
