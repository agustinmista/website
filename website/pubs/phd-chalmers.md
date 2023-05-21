---
title:   "Language-Based Techniques and Stochastic Models for Automated Testing"
authors: Agustín Mista
year:    2023
type:    phd
venue:   Chalmers University of Technology
pdf:     /assets/pdf/phd-chalmers.pdf
comment: Ph.D. thesis
cite:    https://research.chalmers.se/en/publication/535733
date:    2023-06-13
---

As software systems become bigger and scarier, automating their testing is
crucial to ensure that our confidence in them can keep up with their growth. In
this setting, Generational Fuzzing and Random Property-Based Testing are two
sides of the same testing technique that can help us find bugs effectively
without having to spend countless hours writing unit tests by hand. They both
rely on generating large amounts of random (possibly broken) test cases to be
used as inputs to the system. Test cases that trigger issues such as crashes,
memory leaks, or failed assertions are reported back to the developer for
further investigation. Despite being fairly automatable, the Achilles heel of
this technique lies in the quality of the randomly generated test cases, often
requiring substantial manual work to tune the random generation process when the
system under test expects inputs satisfying complex invariants.

This thesis tackles this problem from the Programming Languages perspective,
taking advantage of the richness of functional, statically-typed languages like
Haskell to develop automated techniques for generating good-quality random test
cases, as well as for automatically tuning the testing process in our favor. To
this purpose, we rely on well-established ideas such as coverage-guided fuzzing,
meta-programming, type-level programming, as well as novel interpretations of
centuries-old statistical tools designed to study the evolution of populations
such as branching processes. All these ideas are empirically validated using an
extensive array of case studies and supported by a substantial number of
real-world bugs discovered along the way.

