module Compatibility where

import AST
import Data.Ratio
import Data.TreeDiff
import qualified Generators as G
import Options.Applicative.Help.Levenshtein
import StackExpressions

compTest = do
  let first = Const 1
  let second = BinOp Add (Const 1) (Const 5)
  print $ toStack first
  print $ toStack second
  print $ matchingPercentage first second

matchingPercentage :: ASTExpression -> ASTExpression -> Ratio Int
matchingPercentage l r = match % sizes
  where
    sizes = size l + size r
    dst = getEditDistance l r
    match = sizes - dst

differencePercentage :: ASTExpression -> ASTExpression -> Ratio Int
differencePercentage l r = 1 - uncurry (%) (getMatchingRatio l r)

class MatchingRatio a where
  getMatchingRatio :: a -> a -> (Int, Int)

-- this function is monotonically decreasing, meaning that if the one of the matched trees
-- becomes smaller by the smallest possible margin
-- the matching percentage dips
instance MatchingRatio ASTExpression where
  getMatchingRatio (Const l) (Const r)
    | l == r = (4, 4)
    | otherwise = (2, 4)
  getMatchingRatio l (Const _) = (0, size l + 2)
  getMatchingRatio (Const _) r = (0, size r + 2)
  getMatchingRatio (Param l) (Param r)
    | l == r = (4, 4)
    | otherwise = (2, 4)
  getMatchingRatio (Param _) l = (0, size l + 2)
  getMatchingRatio r (Param _) = (0, size r + 2)
  getMatchingRatio GlobalGet GlobalGet = (0, 2)
  getMatchingRatio GlobalGet l = (0, size l + 2)
  getMatchingRatio r GlobalGet = (0, size r + 2)
  getMatchingRatio (BinOp opl ll lr) (BinOp opr rl rr) =
    (num, denom)
    where
      rest = addTuple (getMatchingRatio ll rl) (getMatchingRatio lr rr)
      op = getMatchingRatio opl opr
      num = fst rest + fst op
      denom = snd rest + snd op
  -- + minimum [getDiff ll rl + getDiff lr rr, 1 + getDiff ll rr + getDiff lr rl]
  getMatchingRatio (BinOp _ ll lr) (RelOp _ rl rr) =
    (fst rest, denom)
    where
      rest = addTuple (getMatchingRatio ll rl) (getMatchingRatio lr rr)
      denom = snd rest + 1
  -- + minimum [getDiff ll rl + getDiff lr rr, 1 + getDiff ll rr + getDiff lr rl]
  getMatchingRatio (BinOp _ ll lr) (UnOp _ rl) =
    (fst rest, denom)
    where
      rest = addTuple (getMatchingRatio ll rl) (getMatchingRatio lr rl)
      denom = snd rest + 1
  getMatchingRatio (RelOp opl ll lr) (RelOp opr rl rr) =
    (num, denom)
    where
      rest = addTuple (getMatchingRatio ll rl) (getMatchingRatio lr rr)
      op = getMatchingRatio opl opr
      num = fst rest + fst op
      denom = snd rest + snd op
  -- + minimum [getDiff ll rl + getDiff lr rr, 1 + getDiff ll rr + getDiff lr rl]
  getMatchingRatio (RelOp opl ll lr) (BinOp opr rl rr) = getMatchingRatio (BinOp opr rl rr) (RelOp opl ll lr)
  getMatchingRatio (RelOp _ ll lr) (UnOp _ rl) =
    (fst rest, denom)
    where
      rest = addTuple (getMatchingRatio ll rl) (getMatchingRatio lr rl)
      denom = snd rest + 1
  getMatchingRatio (UnOp opl l) (UnOp opr r) =
    (num, denom)
    where
      rest = getMatchingRatio r l
      op = getMatchingRatio opl opr
      num = fst rest + fst op
      denom = snd rest + snd op
  getMatchingRatio (GlobalTee l) (GlobalTee r) =
    (fst rest + 1, snd rest + 1)
    where
      rest = getMatchingRatio l r
  getMatchingRatio (UnOp opl ll) (BinOp opr rl rr) = getMatchingRatio (BinOp opr rl rr) (UnOp opl ll)
  getMatchingRatio (UnOp opl ll) (RelOp opr rl rr) = getMatchingRatio (RelOp opr rl rr) (UnOp opl ll)

instance MatchingRatio BinaryOperation where
  getMatchingRatio l r
    | l == r = (2, 2)
    | otherwise = (0, 2)

instance MatchingRatio UnaryOperation where
  getMatchingRatio l r
    | l == r = (2, 2)
    | otherwise = (0, 2)

instance MatchingRatio RelationalOperation where
  getMatchingRatio l r
    | l == r = (2, 2)
    | otherwise = (0, 2)

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (ll, lr) (rl, rr) = (ll + rl, lr + rr)
