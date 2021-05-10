module Main where

import ALife
import AST
import Ancestor

seeds :: [Int]
seeds = [1 .. 5]

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
    [ mainCreature seed ancestor2 generator1 start2 generatorStart1 10000 5 10 5 m a
      | seed <- seeds,
        m <- [2 .. 3],
        a <- [1 .. 2]
    ]
  sequence
    [ mainCreature seed ancestor1 generator2 start1 generatorStart2 10000 5 10 5 m a
      | seed <- seeds,
        m <- [2 .. 3],
        a <- [1 .. 2]
    ]
  sequence
    [ mainCreature seed ancestor2 generator2 start2 generatorStart2 10000 5 10 5 m a
      | seed <- seeds,
        m <- [2 .. 3],
        a <- [1 .. 2]
    ]
