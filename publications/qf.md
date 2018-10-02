---
title:   QuickFuzz testing for fun and profit
authors: Gustavo Grieco, Martin Ceresa, Agustín Mista and Pablo Buiras
year:    2017
venue:   Journal of Systems and Software
type:    journal
pdf:     /assets/qf.pdf
cite:    https://dl.acm.org/citation.cfm?id=3163968
date:    2017-12-01
---

Fuzzing is a popular technique to find flaws in programs using invalid or
erroneous inputs but not without its drawbacks. At one hand, mutational fuzzers
require a set of valid inputs as a starting point, in which modifications are
then introduced. On the other hand, generational fuzzing allows to synthesize
somehow valid inputs according to a specification. Unfortunately, this requires
to have a deep knowledge of the file formats under test to write specifications
of them to guide the test case generation process.

In this paper we introduce an extended and improved version of QuickFuzz, a tool
written in Haskell designed for testing unexpected inputs of common file formats
on third-party software, taking advantage of off-the-self well known fuzzers.

Unlike other generational fuzzers, QuickFuzz does not require to write
specifications for the file formats in question since it relies on existing
file-format-handling libraries available on the Haskell code repository. It
supports almost 40 different complex file-types including images, documents,
source code and digital certificates.

In particular, we found QuickFuzz useful enough to discover many previously
unknown vulnerabilities on real-world implementations of web browsers and image
processing libraries among others.
