---
title:   Deriving Compositional Random Generators
authors: Agustín Mista and Alejandro Russo
year:    2019
venue:   31st Symposium on Implementation and Application of Functional Languages
type:    workshop
pdf:     /assets/pdf/ifl19.pdf
cite:    https://dl.acm.org/doi/abs/10.1145/3412932.3412943
date:    2019-09-25
---

Generating good random values described by algebraic data types is often quite
intricate. State-of-the-art tools for synthesizing random generators serve the
valuable purpose of helping with this task, while providing different levels of
invariants imposed over the generated values. However, they are often not built
for composability nor extensibility, a useful feature when the shape of our
random data needs to be adapted while testing different properties or
sub-systems.

In this work, we develop an extensible framework for deriving compositional
generators, which can be easily combined in different ways in order to fit
developers’ demands using a simple type level description language. Our
framework relies on familiar ideas from the à la Carte technique for writing
composable interpreters in Haskell. In particular, we adapt this technique with
the machinery required in the scope of random generation, showing how concepts
like generation frequency or terminal constructions can also be expressed in the
same type-level fashion. We provide an implementation of our ideas, and evaluate
its performance using real world examples.
