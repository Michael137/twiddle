Overview
========
Write high-level control flow => get optimized bit-twiddled parallel C code

Design
======
* Embedded DSL
 * Host language: Scala
 - [ ] Parser
 - [ ] AST
 - [ ] Optimizers (twiddler/parallelizer) -> adds annotations to AST
 - [ ] CodeGen module
 - [ ] Verifier module
=======

Outstanding Design Choices
==========================
- [x] Choose host language
 - Scala
- [ ] Should generated code be portable? Compiler intrinsics/ifdefs/etc. or not
- [ ] Should aggressiveness level be adjustable?
- [ ] Choose codegen backend. E.g. LMS? lms-verify?
- [ ] Provide option to verify code? E.g. lms-verify
- [ ] Parallelize C code where bit-hacks were not possible. E.g. OMP, Vectorization or provide pluggable parallelizers
- [ ] Write design document and report