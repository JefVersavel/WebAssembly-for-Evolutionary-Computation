module Main where

import ALife
import AST
import Ancestor

seeds :: [Int]
seeds = [1 .. 50]

ancestorCombos :: [(ASTExpression, Double)]
ancestorCombos =
  [ (ancestor1, start1),
    (ancestor2, start2),
    (ancestor3, start3),
    (ancestor4, start4),
    (ancestor6, start6)
  ]

limitations :: [Int]
limitations = [10, 15, 20, 25]

mutationRates :: [Int]
mutationRates = [2 .. 10] ++ [15, 20]

starts :: [Int]
starts = [1 .. 10]

main :: IO [()]
main = do
  sequence
    [ mainCreature seed anc start 100 5 2 5 strt
      | (anc, start) <- ancestorCombos,
        seed <- seeds,
        strt <- starts
    ]
