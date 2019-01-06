# Overview
Write high-level control flow => get correct optimized bit-twiddled parallel C code

# Design
* Embedded DSL
  * Host language: Scala
  - [ ] Parser
  - [ ] AST
  - [ ] Optimizers (twiddler/parallelizer) => adds annotations to AST
  - [ ] CodeGen module
  - [ ] Verifier module

## DSL Overview
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
should produce
```
#define SWAP(a, b) (((a) ^= (b)), ((b) ^= (a)), ((a) ^= (b)))
#define haszero(v) (((v) - 0x01010101UL) & ~(v) & 0x80808080UL)
int x = 20;
int y = 10;
if( haszero(x) )
{
  unsigned int v = x;
  int r;
  r = (v >= 1000000000) ? 9 : (v >= 100000000) ? 8 : (v >= 10000000) ? 7 : 
      (v >= 1000000) ? 6 : (v >= 100000) ? 5 : (v >= 10000) ? 4 : 
      (v >= 1000) ? 3 : (v >= 100) ? 2 : (v >= 10) ? 1 : 0;
  SWAP(y, r)
}
```

## Outstanding Design Choices
- [x] Choose host language
  - Scala
- [x] Should generated code be portable? Compiler intrinsics/ifdefs/etc. or not
  - Yes
- [x] Should aggressiveness level be adjustable?
  - Yes. Perhaps different evaluator for aggressive optimizations
- [x] Choose codegen backend. E.g. LMS? lms-verify?
  - No more LMS. Documentation is virtually non-existent. C backend is not consistent with the C language. Maybe in the long term with a few pull requests to LMS.
- [x] Provide option to verify code? E.g. lms-verify
  - Not yet. Perhaps when supporting LMS style codegen
- [x] Parallelize C code where bit-hacks were not possible. E.g. OMP, Vectorization or provide pluggable parallelizers
  - Yes

## Why no LMS?
- Documentation for this use-case is hard to come by
- The C and C++ backends are not well separated and require manual patchwork to be sound
- Control over variable generation in the output code is not flexible (without significant complexity overhead)

## TODO
- [x] Implement tagless interpreter for core object language ([see language overview](#dsl-overview))
- [x] Add code generation facilities to core language
- [ ] Add optimization facilities
- [x] Build out core library
- [ ] OpenMP codegen ~~for LMS~~?
~~- [ ] Add verifier and extend~~
- [x] ~~ScalaTest support~~ Testsuite
- [ ] Extend with LMS

# Instructions
1. Run LMS [installation instructions](https://github.com/TiarkRompf/virtualization-lms-core)
2. sbt
3. run

# Resources
* https://github.com/namin/lms-verify
* https://www.slideshare.net/krikava/domain-specific-languages-and-scala
* https://stanford-ppl.github.io/Delite/myfirstdsl.html
* https://skillsmatter.com/skillscasts/3289-javascript-embedded-dsl-scala
* https://github.com/TiarkRompf/virtualization-lms-core
* https://github.com/julienrf/lms-tutorial
* https://scala-lms.github.io/tutorials/04_atwork.html
* https://github.com/namin/metaprogramming
* https://github.com/namin/metaprogramming/blob/master/lectures/4a-dsls/syntax.scala
* https://www.cl.cam.ac.uk/~na482/meta/slides-4a.pdf
* https://graphics.stanford.edu/~seander/bithacks.html
* https://www.youtube.com/watch?v=16A1yemmx-w
* https://github.com/scala-lms/tutorials/blob/master/src/test/scala/lms/tutorial/dslapi.scala
* https://github.com/TiarkRompf/virtualization-lms-core/blob/v1.0.0/test-src/epfl/test14-scratch/TestCGen.scala
! https://github.com/TiarkRompf/virtualization-lms-core/blob/v1.0.0/src/internal/CCodegen.scala
* https://scala-lms.github.io/tutorials/04_atwork.html
* https://github.com/TiarkRompf/virtualization-lms-core/blob/361a806f674cd12d9d31655bd0f30664a451ad9f/src/internal/CCodegen.scala
* https://github.com/TiarkRompf/virtualization-lms-core/blob/361a806f674cd12d9d31655bd0f30664a451ad9f/src/common/Packages.scala -> CCodeGenPkg
* https://github.com/namin/lms-verify/blob/4e43e5669285c1ae98adf97848da497000b0d182/src/main/scala/lms/verify/Core.scala -> CCodeGenDSL
* https://stanford-ppl.github.io/Delite/faq.html
* https://github.com/scala-lms/tutorials/blob/master/src/test/scala/lms/tutorial/eval.scala
