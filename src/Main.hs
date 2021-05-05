module Main where

import ALife
import AST
import Ancestor

seeds :: [Int]
seeds = [10]

ancestorCombos :: [(ASTExpression, Double)]
ancestorCombos = [(ancestor1, start1), (ancestor2, start2)]

limitations :: [Int]
limitations = [5]

mutationRates :: [Int]
mutationRates = [2, 3, 4, 5]

subTreeDepths :: [Double]
subTreeDepths = [0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4]

main :: IO [()]
main = do
  sequence
    [ mainCreature seed anc start 10000 l mut 5 depth
      | (anc, start) <- ancestorCombos,
        l <- limitations,
        seed <- seeds,
        mut <- mutationRates,
        depth <- subTreeDepths
    ]
