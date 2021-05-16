module Main where

import ALife
import AST
import Ancestor

seeds :: [Int]
seeds = [1 .. 10]

ancestorCombos :: [(ASTExpression, Double)]
ancestorCombos = [(ancestor1, start1)]

generators = [(generator1, generatorStart1)]

limitations :: [Int]
limitations = [1 .. 10]

mutationRates :: [Int]
mutationRates = [2 .. 10]

goodCombos =
  [ (3, 2, 10)
  ]

main :: IO [()]
main = do
  sequence
    [ mainCreature seed ancestor1 ancestor6 start1 start6 10000 5 10 5 a
      | seed <- seeds,
        a <- [1 .. 10]
    ]
