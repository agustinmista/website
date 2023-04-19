---
title:   "BinderAnn: Automated Reification of Source Annotations for Monadic EDSLs"
authors: Agustín Mista and Alejandro Russo
year:    2020
venue:   21st International Symposium on Trends in Functional Programming
type:    symposium
pdf:     /assets/pdf/tfp20.pdf
cite:    https://link.springer.com/chapter/10.1007/978-3-030-57761-2_2
date:    2020-02-13
---

Embedded Domain-Specific Languages (EDSLs) are an alternative to quickly
implement specialized languages without the need to write compilers or
interpreters from scratch. In this territory, Haskell is a prime choice as the
host language. EDSLs in Haskell, however, are often incapable of reifying useful
static information from the source code, namely variable binding names and
source locations. Not having access to variable names directly affects EDSLs
designed to generate low-level code, where the variables names in the generated
code do not match those found in the source code—thus broadening the semantic
gap among source and target code. Similarly, many existing EDSLs produce poor
error messages due to the lack of knowledge of source locations where errors are
generated.

In this work, we propose a simple technique for enhancing monadic EDSLs
expressed using do notation. This technique employs source-to-source plugins, a
relatively new feature of GHC, to annotate every do statement of our EDSLs with
relevant information extracted from the source code at compile time. We show how
these annotations can be incorporated into EDSL designs either directly inside
values or as monadic effects. We provide BinderAnn, a GHC source plugin
implementing our ideas, and evaluate it by enhancing existing real-world EDSLs
with relatively minor modification efforts to contemplate the source-level
static information related to variables names and source locations.
