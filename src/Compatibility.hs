module Compatibility where

import AST
import Data.Ratio
import Data.TreeDiff
import qualified Generators as G

compTest = do
  let first = BinOp Add (Const 1) (Const 2)
  let second = BinOp Add (Const 1) (Param 3)
  let d = ediff first second
  print (size first + size second)
  print $ matchingPercentage first second
  let diff = getDiff first second * 2
  print diff
  print $ diff % (getNrNodes first + getNrNodes second)

calculateDifference :: ASTExpression -> ASTExpression -> Int
calculateDifference l r = fromIntegral (editDiff diffTree) `div` 2
  where
    diffTree = ediff l r

matchingPercentage :: ASTExpression -> ASTExpression -> Ratio Int
matchingPercentage l r = 1 - ((getDiff l r * 2) % (size l + size r))

editDiff :: Edit EditExpr -> Int
editDiff (Ins e) = editExprDiff e
editDiff (Del e) = editExprDiff e
editDiff (Cpy (EditApp _ l)) = sum $ map editDiff l
editDiff (Cpy (EditLst l)) = sum $ map editDiff l
editDiff (Cpy _) = 0
editDiff (Swp l r) = editExprDiff l + editExprDiff r

diff :: Expr -> Int
diff (App _ l) = 1 + sum (map diff l)
diff (Lst l) = sum $ map diff l

editExprDiff :: EditExpr -> Int
editExprDiff (EditApp _ l) = sum $ map editDiff l
editExprDiff (EditLst l) = sum $ map editDiff l
editExprDiff (EditExp e) = diff e

class Difference a where
  getDiff :: a -> a -> Int

instance Difference ASTExpression where
  getDiff (Const l) (Const r)
    | l == r = 0
    | otherwise = 2
  getDiff (Const _) l = size l
  getDiff r (Const _) = size r
  getDiff (Param l) (Param r)
    | l == r = 0
    | otherwise = 2
  getDiff (Param _) l = size l
  getDiff r (Param _) = size r
  getDiff (BinOp opl ll lr) (BinOp opr rl rr) =
    getDiff opl opr
      + getDiff ll rl
      + getDiff lr rr
  -- + minimum [getDiff ll rl + getDiff lr rr, 1 + getDiff ll rr + getDiff lr rl]
  getDiff (BinOp _ ll lr) (RelOp _ rl rr) =
    1
      + getDiff ll rl
      + getDiff lr rr
  -- + minimum [getDiff ll rl + getDiff lr rr, 1 + getDiff ll rr + getDiff lr rl]
  getDiff (BinOp _ ll lr) (UnOp _ rl) =
    1
      + minimum [getDiff ll rl, getDiff lr rl]
  getDiff (RelOp opl ll lr) (RelOp opr rl rr) =
    getDiff opl opr
      + getDiff ll rl
      + getDiff lr rr
  -- + minimum [getDiff ll rl + getDiff lr rr, 1 + getDiff ll rr + getDiff lr rl]
  getDiff (RelOp opl ll lr) (BinOp opr rl rr) = getDiff (BinOp opr rl rr) (RelOp opl ll lr)
  getDiff (RelOp _ ll lr) (UnOp _ rl) =
    1 + minimum [getDiff ll rl, getDiff lr rl]
  getDiff (UnOp opl l) (UnOp opr r) = getDiff opl opr + getDiff r l
  getDiff (UnOp opl ll) (BinOp opr rl rr) = getDiff (BinOp opr rl rr) (UnOp opl ll)
  getDiff (UnOp opl ll) (RelOp opr rl rr) = getDiff (RelOp opr rl rr) (UnOp opl ll)

instance Difference BinaryOperation where
  getDiff l r
    | l == r = 0
    | otherwise = 1

instance Difference UnaryOperation where
  getDiff l r
    | l == r = 0
    | otherwise = 1

instance Difference RelationalOperation where
  getDiff l r
    | l == r = 0
    | otherwise = 1
