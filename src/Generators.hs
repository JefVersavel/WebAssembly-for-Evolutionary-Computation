{-# LANGUAGE DataKinds #-}

module Generators where

import AST
import Control.Monad.Reader
import Seeding
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

rBinOp :: Gen BinaryOperation
rBinOp = elements [minBound .. maxBound]

rBinExpr :: Gen ASTExpression -> Gen ASTExpression -> Gen ASTExpression
rBinExpr gExpr1 gExpr2 = do
  op <- rBinOp
  e1 <- gExpr1
  e2 <- gExpr2
  return $ BinOp op e1 e2

rUnOp :: Gen UnaryOperation
rUnOp = elements [minBound .. maxBound]

rUnExpr :: Gen ASTExpression -> Gen ASTExpression
rUnExpr gExpr = do
  op <- rUnOp
  e <- gExpr
  return $ UnOp op e

rRelOp :: Gen RelationalOperation
rRelOp = elements [minBound .. maxBound]

rRelExpr :: Gen ASTExpression -> Gen ASTExpression -> Gen ASTExpression
rRelExpr gExpr1 gExpr2 = do
  op <- rRelOp
  e1 <- gExpr1
  e2 <- gExpr2
  return $ RelOp op e1 e2

rConst :: Gen ASTExpression
rConst = Const <$> arbitrary

rParam :: Int -> Gen ASTExpression
rParam m = Param <$> elements [0, 1 .. (m - 1)]

-- GROWONE generator
growOne :: Int -> Reader (Int, Int) (Gen ASTExpression)
growOne d = do
  (maxD, nrParam) <- ask
  if d < maxD
    then do
      g1 <- growOne $ d + 1
      g2 <- growOne $ d + 1
      return $
        frequency
          [ (1, rConst),
            (1, rParam nrParam),
            (length [minBound :: BinaryOperation ..], rBinExpr g1 g2),
            (length [minBound :: UnaryOperation ..], rUnExpr g1),
            (length [minBound :: RelationalOperation ..], rRelExpr g1 g2)
          ]
    else return $ oneof [rConst, rParam nrParam]

growOneInit :: QCGen -> Reader (Int, Int) (Gen ASTExpression)
growOneInit gen = do
  g <- growOne 0
  let seeded = useSeed gen g
  return seeded

-- FULLONE generator
fullOne :: Int -> Reader (Int, Int) (Gen ASTExpression)
fullOne d = do
  (maxD, nrParam) <- ask
  if d < maxD
    then do
      f1 <- fullOne $ d + 1
      f2 <- fullOne $ d + 1
      return $
        frequency
          [ (length [minBound :: BinaryOperation ..], rBinExpr f1 f2),
            (length [minBound :: UnaryOperation ..], rUnExpr f1),
            (length [minBound :: RelationalOperation ..], rRelExpr f1 f2)
          ]
    else return $ oneof [rConst, rParam nrParam]

fullOneInit :: QCGen -> Reader (Int, Int) (Gen ASTExpression)
fullOneInit gen = do
  g <- fullOne 0
  let seeded = useSeed gen g
  return seeded

-- executes the ramped half and half method for population initialization
rampedHalfNHalf :: QCGen -> Int -> Int -> Double -> Int -> [Gen ASTExpression]
rampedHalfNHalf gen d nrParam ratio n
  | ratio <= 1 && ratio >= 0 = growList ++ fullList
  | otherwise = error "Please provide a ratio between 0 and 1."
  where
    growN = round $ ratio * fromIntegral n
    fullN = round $ (1 - ratio) * fromIntegral n
    (growSeeds, fullSeeds) = grow2SeedLists gen growN fullN
    growList = generateInitList growSeeds growOneInit
    fullList = generateInitList fullSeeds fullOneInit
    generateInitList gens method =
      [runReader (method g) (fst $ randomR (0, d) g, nrParam) | g <- gens]

generateWithSeed :: QCGen -> Gen a -> IO a
generateWithSeed seed (MkGen g) = do
  return (g seed 30)

genASTExpressions :: Int -> Int -> Int -> Double -> Int -> IO [ASTExpression]
genASTExpressions seed d nrParam ratio n = do
  sequence
    [generate g | g <- rampedHalfNHalf (mkQCGen seed) d nrParam ratio n]
