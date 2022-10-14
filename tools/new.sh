#!/usr/bin/env bash
touch "./input/$1.txt"
cat ./challenges/DayX.hs | sed "s/0/$1/" > "./challenges/Day$1.hs"
echo "executable day$1
  import:           day
  main-is:          Day10.hs
  hs-source-dirs:   challenges
  build-depends:    containers

" >> adventofcode2021.cabal