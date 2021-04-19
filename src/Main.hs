module Main where

import ALife
import AST
import Ancestor

seeds :: [Int]
seeds = [1 .. 10]

ancestorCombos :: [(ASTExpression, Double)]
ancestorCombos = [(ancestor6, start6)]

limitations :: [Int]
limitations = [10, 15 .. 25]

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
