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
