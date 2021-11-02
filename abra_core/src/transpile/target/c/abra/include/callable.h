#ifndef __ABRA_CALLABLE_H
#define __ABRA_CALLABLE_H

#include "abra_function.h"

// Define callable context structs and functions which operate on them
// -------------------------------------------------------------------
//
// This abuses the macro system pretty hard, but it helps cut down on
// repetitive noisy code (and reduces errors due to copy/paste). Each
// macro is explained with a comment.
//

// This is the "repeated, comma separated list" macro. Basically, saying
// `_2(foo)` will produce `, foo, foo`. This is useful for producing
// argument types in function signature typedefs.
#define _0(x)
#define _1(x) ,x
#define _2(x) ,x _1(x)
#define _3(x) ,x _2(x)
#define _4(x) ,x _3(x)
#define _5(x) ,x _4(x)
#define _6(x) ,x _5(x)
#define _7(x) ,x _6(x)
#define _8(x) ,x _7(x)
#define _9(x) ,x _8(x)
#define _10(x) ,x _9(x)
#define _11(x) ,x _10(x)
#define _12(x) ,x _11(x)

// This is the "repeated, incrementing, comma separated list" macro. Basically,
// saying `_2(foo)` will produce `, foo1, foo2`. This is useful for producing
// argument lists in function declarations.
#define __0(x)
#define __1(x) ,x##1
#define __2(x) __1(x), x##2
#define __3(x) __2(x), x##3
#define __4(x) __3(x), x##4
#define __5(x) __4(x), x##5
#define __6(x) __5(x), x##6
#define __7(x) __6(x), x##7
#define __8(x) __7(x), x##8
#define __9(x) __8(x), x##9
#define __10(x) __9(x), x##10
#define __11(x) __10(x), x##11
#define __12(x) __11(x), x##12

// This is the macro to define the value of the `callable_ctx` flavor, per
// arity. There's also a need to obtain the "successor" arity's flavor. Sadly
// (or perhaps not sadly, for the sake of sanity), C's macro system does not
// support `CTX_T(arity + 1)`, so we must enumerate all successors manually here.
#define CTX_T(arity) callable_ctx__##arity##_t
#define CTX_SUCC_0_T callable_ctx__1_t
#define CTX_SUCC_1_T callable_ctx__2_t
#define CTX_SUCC_2_T callable_ctx__3_t
#define CTX_SUCC_3_T callable_ctx__4_t
#define CTX_SUCC_4_T callable_ctx__5_t
#define CTX_SUCC_5_T callable_ctx__6_t
#define CTX_SUCC_6_T callable_ctx__7_t
#define CTX_SUCC_7_T callable_ctx__8_t
#define CTX_SUCC_8_T callable_ctx__9_t
#define CTX_SUCC_9_T callable_ctx__10_t
#define CTX_SUCC_10_T callable_ctx__11_t
#define CTX_SUCC_11_T callable_ctx__12_t

// This is the macro to define the value of the `fn_t` flavor, per arity.
// This is very similar to the macro block above.
#define FN_T(arity) fn##arity##_t
#define FN_SUCC_0_T fn1_t
#define FN_SUCC_1_T fn2_t
#define FN_SUCC_2_T fn3_t
#define FN_SUCC_3_T fn4_t
#define FN_SUCC_4_T fn5_t
#define FN_SUCC_5_T fn6_t
#define FN_SUCC_6_T fn7_t
#define FN_SUCC_7_T fn8_t
#define FN_SUCC_8_T fn9_t
#define FN_SUCC_9_T fn10_t
#define FN_SUCC_10_T fn11_t
#define FN_SUCC_11_T fn12_t

// Declare the `fn_t` and `callable_ctx` struct flavors for the given arity.
#define MAKE_CALLABLE_CTX(arity)                                  \
  typedef AbraValue (*FN_T(arity))(void* _##arity(AbraValue));    \
  typedef struct CTX_T(arity) {                                   \
    FN_T(arity) fn;                                               \
    void* env;                                                    \
    bool is_bound;                                                \
    AbraValue self;                                               \
  } CTX_T(arity);                                                 \

// Declare the `call_fn` and `bind_fn` function flavors for the given arity.
// When calling a function, if the function is 'bound', then we must first
// cast the `callable_ctx` to its successor flavor (see the `CTX_T` macro
// definitions above), then invoke the `ctx->fn` field, passing `ctx->self`
// as its first argument.
//
// When binding a function, allocate a `callable_ctx` of the given arity's
// successor, since we need to store the `self` value. As described above,
// this value will be passed to the underlying function as its implicit argument.
#define MAKE_CALL_AND_BIND_FNS(arity)                                                              \
  AbraValue call_fn_##arity(CTX_T(arity)* ctx __##arity(AbraValue arg)) {                          \
    if (ctx->is_bound) {                                                                           \
      return ((CTX_SUCC_##arity##_T*)ctx)->fn(ctx->env, ctx->self __##arity(arg));                 \
    }                                                                                              \
    return ctx->fn(ctx->env __##arity(arg));                                                       \
  }                                                                                                \
  AbraValue bind_fn_##arity(FN_SUCC_##arity##_T fn, char* name, char* c_name, AbraValue self) {    \
    CTX_SUCC_##arity##_T* ctx = GC_MALLOC(sizeof(CTX_SUCC_##arity##_T));                           \
    ctx->fn = fn;                                                                                  \
    ctx->is_bound = true;                                                                          \
    ctx->self = self;                                                                              \
    return alloc_function(name, c_name, (void*) ctx);                                              \
  }

// **********************************************************
// Actual definitions begin here
// **********************************************************
//
// N = 12
//
// Define `callable_ctx`s and `fn_t`s for arities up to N. Then
// define `call_fn`s and `bind_fn`s for arities up to N - 1 . The arity
// here must be 1 less than the number of `callable_ctx`s created above,
// since the implementations require the successor `callable_ctx` to be
// defined.
//
// (Note also that the utilities at the top of this file will need to be
// updated when extending N beyond its current value.)
//

// Define `callable_ctx`s and `fn_t`s
MAKE_CALLABLE_CTX(0)
MAKE_CALLABLE_CTX(1)
MAKE_CALLABLE_CTX(2)
MAKE_CALLABLE_CTX(3)
MAKE_CALLABLE_CTX(4)
MAKE_CALLABLE_CTX(5)
MAKE_CALLABLE_CTX(6)
MAKE_CALLABLE_CTX(7)
MAKE_CALLABLE_CTX(8)
MAKE_CALLABLE_CTX(9)
MAKE_CALLABLE_CTX(10)
MAKE_CALLABLE_CTX(11)
MAKE_CALLABLE_CTX(12)

// Define `call_fn`s and `bind_fn`s
MAKE_CALL_AND_BIND_FNS(0)
MAKE_CALL_AND_BIND_FNS(1)
MAKE_CALL_AND_BIND_FNS(2)
MAKE_CALL_AND_BIND_FNS(3)
MAKE_CALL_AND_BIND_FNS(4)
MAKE_CALL_AND_BIND_FNS(5)
MAKE_CALL_AND_BIND_FNS(6)
MAKE_CALL_AND_BIND_FNS(7)
MAKE_CALL_AND_BIND_FNS(8)
MAKE_CALL_AND_BIND_FNS(9)
MAKE_CALL_AND_BIND_FNS(10)
MAKE_CALL_AND_BIND_FNS(11)

#endif
