---
title:   "MUTAGEN: Reliable Coverage-Guided, Property-Based Testing using Exhaustive Mutations"
authors: Agustín Mista and Alejandro Russo
year:    2023
venue:   16th IEEE International Conference on Software Testing, Verification and Validation (ICST) 2023
type:    conference
date:    2023-04-18
---

Automatically-synthesized random data generators are an appealing option when
using property-based testing. There exists a variety of techniques that extract
static information from the codebase to produce random test cases.
Unfortunately, such techniques cannot enforce the complex invariants often
needed to test properties with sparse preconditions.

Coverage-guided, property-based testing (CGPT) tackles this limitation by
enhancing synthesized generators with structure-preserving mutations guided by
execution traces. Albeit effective, CGPT relies largely on randomness and
exhibits poor scheduling, which can prevent bugs from being found.

We present MUTAGEN, a CGPT framework that tackles such limitations by generating
mutants exhaustively. Our tool incorporates heuristics that help to minimize
scalability issues as well as cover the search space in a principled manner. Our
evaluation shows that MUTAGEN not only outperforms existing CGPT tools but also
finds previously unknown bugs in real-world software.
