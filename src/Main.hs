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
limitations = [10]

mutationRates :: [Int]
mutationRates = [2 .. 10]

main :: IO [()]
main = do
  sequence
    [ mainCreature seed ancestor1 gen start1 st 10000 5 10 5 m a
      | seed <- seeds,
        (gen, st) <- generators,
        m <- [1 .. 3],
        a <- [0 .. 2]
    ]
