module Compatibility where

import AST
import Data.Ratio
import Data.TreeDiff
import qualified Generators as G

compTest = do
  trees <- G.randomGenerationTest
  print trees
  let first = head trees
  let second = trees !! 1
  let d = ediff first second
  print $ prettyEditExpr d
  print $ calculateDifference first second
  print (size first + size second)
  print $ differencePercentage first second

calculateDifference :: ASTExpression -> ASTExpression -> Int
calculateDifference l r = fromIntegral (editDiff diffTree) `div` 2
  where
    diffTree = ediff l r

differencePercentage :: ASTExpression -> ASTExpression -> Ratio Int
differencePercentage l r = 1 - (calculateDifference l r % (size l + size r))

editDiff :: Edit EditExpr -> Int
editDiff (Ins e) = editExprDiff e
editDiff (Del e) = editExprDiff e
editDiff (Cpy _) = 0
editDiff (Swp l r) = editExprDiff l + editExprDiff r

diff :: Expr -> Int
diff (App _ l) = 1 + sum (map diff l)
diff (Lst l) = sum $ map diff l

editExprDiff :: EditExpr -> Int
editExprDiff (EditApp _ l) = sum $ map editDiff l
editExprDiff (EditLst l) = sum $ map editDiff l
editExprDiff (EditExp e) = diff e
