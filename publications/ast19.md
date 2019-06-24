---
title:   Generating Random Structurally Rich Algebraic Data Type Values
authors: Agustín Mista and Alejandro Russo
year:    2019
venue:   14th IEEE/ACM International Workshop on Automation of Software Test
type:    workshop
pdf:     /assets/pdf/ast19.pdf
cite:    https://dl.acm.org/citation.cfm?id=3338669
date:    2019-05-27
---

Automatic generation of random values described by algebraic data types (ADTs)
is often a hard task. State-of-the-art random testing tools can automatically
synthesize random data generators based on ADTs definitions. In that manner,
generated values comply with the structure described by ADTs, something that
proves useful when testing software which expects complex inputs. However, it
sometimes becomes necessary to generate structural richer ADTs values in order
to test deeper software layers. In this work we propose to leverage static
information found in the codebase as a manner to improve the generation process.
Namely, our generators are capable of considering how programs branch on input
data as well as how ADTs values are built via interfaces. We implement a tool,
responsible for synthesizing generators for ADTs values while providing
compiletime guarantees about their distributions. Using compile-time
predictions, we provide a heuristic that tries to adjust the distribution of
generators to what developers might want. We report on preliminary experiments
where our approach shows encouraging results.
