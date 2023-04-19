---
title:   "Short Paper: Weak Runtime-Irrelevant Typing for Security"
authors: Matthías Páll Gissurarson and Agustín Mista
year:    2020
venue:   ACM SIGSAC 15th Workshop on Programming Languages and Analysis for Security
type:    workshop
pdf:     /assets/pdf/plas20.pdf
cite:    https://dl.acm.org/doi/10.1145/3411506.3417595
date:    2020-11-13
---

Types indexed with extra type-level information are a powerful tool for
statically enforcing domain-specific security properties. In many cases, this
extra information is runtime-irrelevant, and so it can be completely erased at
compile-time without degrading the performance of the compiled code. In
practice, however, the added bureaucracy often disrupts the development process,
as programmers must completely adhere to new complex constraints in order to
even compile their code.

In this work we present WRIT, a plugin for the GHC Haskell compiler that relaxes
the type checking process in the presence of runtime-irrelevant constraints. In
particular, WRIT can automatically coerce between runtime equivalent types,
allowing users to run programs even in the presence of some classes of type
errors. This allows us to gradually secure our code while still being able to
compile at each step, separating security concerns from functional correctness.
Moreover, we present a novel way to specify which types should be considered
equivalent for the purpose of allowing the program to run, how ambiguity at the
type level should be resolved and which constraints can be safely ignored and
turned into warnings.
