# Overview
========
Write high-level control flow => get correct optimized bit-twiddled parallel C code

# Design
======
* Embedded DSL
  * Host language: Scala
  - [ ] Parser
  - [ ] AST
  - [ ] Optimizers (twiddler/parallelizer) => adds annotations to AST
  - [ ] CodeGen module
  - [ ] Verifier module

## DSL Overview
============
* Primitives: integers, bit-vector
* Constructs: coniditionals, for loops, while loops, do loops
* Arithmetics: +, -, *, /, **
* Bit-wise operators: |, &, ~, >>, <<, ^
* Logical operators: &&, ||, ==, <, >, <=, =>, !=
* Optimized builtins: log2, log10, sqrt, ceil, has_zero, byte_{eq,gt,lt,gte,lte}, signof, abs, min, max, count_bits_set, rev_bits, swap_bits, %, is_power_2, next_power_2
* Optimizations on constructs: vectorize/unroll loops, high-level op -> bit-operation, branch -> bit-operations without branches

Example:
```
x = 20
y = 10
if( has_zero(x) )
    swap_bits(y, log10(x))
```

## Outstanding Design Choices
==========================
- [x] Choose host language
  - Scala
- [ ] Should generated code be portable? Compiler intrinsics/ifdefs/etc. or not
- [ ] Should aggressiveness level be adjustable?
- [ ] Choose codegen backend. E.g. LMS? lms-verify?
- [ ] Provide option to verify code? E.g. lms-verify
- [ ] Parallelize C code where bit-hacks were not possible. E.g. OMP, Vectorization or provide pluggable parallelizers
- [ ] Write design document and report

## TODO
====
- [ ] Implement tagless interpreter for core object language ([see language overview](#dsl-overview))
- [ ] Add code generation facilities to core language
- [ ] Add optimization facilities
- [ ] Build out core library
- [ ] Add verifier and extend

# Resources
=========
* https://github.com/namin/lms-verify
* https://www.slideshare.net/krikava/domain-specific-languages-and-scala
* https://stanford-ppl.github.io/Delite/myfirstdsl.html
* https://skillsmatter.com/skillscasts/3289-javascript-embedded-dsl-scala
* https://github.com/TiarkRompf/virtualization-lms-core
* https://github.com/julienrf/lms-tutorial
* https://scala-lms.github.io/tutorials/04_atwork.html#__toc_id:32162
* https://github.com/namin/metaprogramming
* https://github.com/namin/metaprogramming/blob/master/lectures/4a-dsls/syntax.scala
* https://www.cl.cam.ac.uk/~na482/meta/slides-4a.pdf
* https://graphics.stanford.edu/~seander/bithacks.html
* https://www.youtube.com/watch?v=16A1yemmx-w
* https://github.com/scala-lms/tutorials/blob/master/src/test/scala/lms/tutorial/dslapi.scala
