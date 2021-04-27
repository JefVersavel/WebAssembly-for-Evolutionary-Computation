module Main where

import ALife
import AST
import Ancestor

seeds :: [Int]
seeds = [1 .. 3]

ancestorCombos :: [(ASTExpression, Double)]
ancestorCombos = [(ancestor1, start1), (ancestor2, start2), (ancestor6, start6)]

limitations :: [Int]
limitations = [10]

mutationRates :: [Int]
mutationRates = [2 .. 10]

main :: IO [()]
main = do
  sequence
    [ mainCreature seed ancestor1 ancestor2 start1 start2 10000 l mut 5
      | l <- limitations,
        seed <- seeds,
        mut <- mutationRates
    ]
