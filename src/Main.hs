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

goodCombos =
  [ (3, 2, 10),
    (1, 2, 3),
    (2, 0, 10),
    (2, 1, 1),
    (2, 1, 9),
    (2, 2, 9),
    (2, 2, 10),
    (3, 0, 10),
    (3, 1, 1),
    (3, 1, 10),
    (3, 2, 1)
  ]

main :: IO [()]
main = do
  sequence
    [ mainCreature seed ancestor1 generator1 start1 generatorStart1 30000 5 10 5 m a
      | (m, a, seed) <- goodCombos
    ]
