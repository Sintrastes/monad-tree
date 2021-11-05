# monad-tree

<p align="center">
  <a href="https://www.haskell.org/">
    <img src="https://img.shields.io/badge/Language-Haskell-blue">
  </a>
  <a href="https://hackage.haskell.org/package/monad-tree-0.1.0">
    <img src="https://img.shields.io/badge/Hackage-0.1.0-green">
  </a>
  <img src="https://img.shields.io/badge/License-MIT-blue">
</p>

This package provides monad instances for a rose-tree-like data structure `Tree`, allowing for the definition of non-deterministic computations that do not depend on a specific search procedure.

When not used as a monad for nondeterministic computation, `Tree` can also be used as a general purpose rose tree data structure.

For more information, see the documentation for the main Control.Monad.Tree module.

# Related papers:

  * [The Algebra of Logic Programming](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.108.6851&rep=rep1&type=pdf), by Silvija Seres
  * [Typed Logical Variables in Haskell](https://gup.ub.gu.se/file/207634), by Koen Claessen and Peter Ljunglöf
  * [Lightweight Functional Logic Meta-Programming](https://link.springer.com/chapter/10.1007/978-3-030-34175-6_12), Nada Amin et. al.
