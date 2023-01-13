---
title:   "Automated Derivation of Random Generators for Algebraic Data Types"
authors: Agustín Mista
year:    2020
type:    licentiate
venue:   Chalmers University of Technology
pdf:     /assets/pdf/licentiate-chalmers.pdf
comment: Licentiate thesis
cite:    https://search.ebscohost.com/login.aspx?direct=true&db=ir01624a&AN=crp.038c338a.f005.419f.9698.b07bf354168e&site=eds-live&scope=site&authtype=guest&custid=s3911979&groupid=main&profile=eds
date:    2020-03-17
---

Many testing techniques such as generational fuzzing or random property-based testing require the existence of some sort of random generation process for the values used as test inputs. Implementing such generators is usually a task left to end-users, who do their best to come up with somewhat sensible implementations after several iterations of trial and error. This necessary effort is of no surprise, implementing good random data generators is a hard task. It requires deep knowledge about both the domain of the data being generated, as well as the behavior of the stochastic process generating such data. In addition, when the data we want to generate has a large number of possible variations, this process is not only intricate, but also very cumbersome.

To mitigate these issues, this thesis explores different ideas for automatically deriving random generators based on existing static information. In this light, we design and implement different derivation algorithms in Haskell for obtaining random generators of values encoded using Algebraic Data Types (ADTs). Although there exists other tools designed directly or indirectly for this very purpose, they are not without disadvantages. In particular, we aim to tackle the lack of flexibility and static guarantees in the distribution induced by derived generators. We show how automatically derived generators for ADTs can be framed using a simple yet powerful stochastic model. These models can be used to obtain analytical guarantees about the distribution of values produced by the derived generators. This, in consequence, can be used to optimize the stochastic generation parameters of the derived generators towards target distributions set by the user, providing more flexible derivation mechanisms.
