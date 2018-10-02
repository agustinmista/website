---
title:   Branching Processes for QuickCheck Generators
authors: Agustín Mista, Alejandro Russo and John Hughes
year:    2018
venue:   ACM SIGPLAN Haskell Symposium
type:    conf
pdf:     /assets/bp.pdf
cite:    https://dl.acm.org/citation.cfm?doid=3242744.3242747
date:    2018-09-28
---

In QuickCheck (or, more generally, random testing), it is challenging to control
random data generators' distributions---specially when it comes to user-defined
algebraic data types (ADT). In this paper, we adapt results from an area of
mathematics known as branching processes, and show how they help to analytically
predict (at compile-time) the expected number of generated constructors, even in
the presence of mutually recursive or composite ADTs. Using our probabilistic
formulas, we design heuristics capable of automatically adjusting probabilities
in order to synthesize generators which distributions are aligned with users'
demands. We provide a Haskell implementation of our mechanism in a tool called
DRaGeN and perform case studies with real-world applications. When generating
random values, our synthesized QuickCheck generators show improvements in code
coverage when compared with those automatically derived by state-of-the-art
tools.
