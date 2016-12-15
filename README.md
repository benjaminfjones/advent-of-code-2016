# My Advent of Code Submissions for 2016

Problem solutions are either in files at the top-level called `pN.hs`, where
`N` is the calendar day, or they are in their own similarly named directory (in
case I wanted a full cabal sandbox to build the solution).

Note: Some solutions require packages to be installed in order to run them.
One solution to this (which I use for small one-off programs like these) is to
create a Cabal sandbox in the repository, install the necessary packages
into it, and then tell `ghci` or `runhaskell` or `ghc` about it:

```
cabal sandbox init
cabal install vector bytestring attoparsec pureMD5
...
alias ghci-sandbox='ghci -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d'

ghci-sandbox
GHCi, version 8.0.1.20161117: http://www.haskell.org/ghc/  :? for help
ghci> :l p9.hs
Ok, modules loaded: P9.
ghci> main
...
```
