module Main where

import ALife
import AST
import Ancestor

seeds :: [Int]
seeds = [9 .. 30]

ancestorCombos :: [(ASTExpression, Double)]
ancestorCombos = [(ancestor1, start1), (ancestor2, start2), (ancestor6, start6)]

limitations :: [Int]
limitations = [10]

mutationRates :: [Int]
mutationRates = [2, 4 .. 10]

ratios :: [Int]
ratios = [1, 2, 3]

main :: IO [()]
main = do
  sequence
    [ mainCreature seed ancestor1 start1 1000 5 mut 5 ratio
      | seed <- seeds,
        mut <- mutationRates,
        ratio <- ratios
    ]
