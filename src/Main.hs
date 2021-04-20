module Main where

import ALife
import AST
import Ancestor

seeds :: [Int]
seeds = [1 .. 10]

ancestorCombos :: [(ASTExpression, Double)]
ancestorCombos = [(ancestor1, start1)]

limitations :: [Int]
limitations = [10, 15, 20, 25]

mutationRates :: [Int]
mutationRates = [2 .. 10] ++ [15, 20]

main :: IO [()]
main = do
  sequence
    [ mainCreature seed anc start 10000 l mut 5
      | (anc, start) <- ancestorCombos,
        l <- limitations,
        seed <- seeds,
        mut <- mutationRates
    ]
