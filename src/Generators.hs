module Generators where

import AST
import Control.Monad.Reader
import ImportedFunction
import Seeding
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Random

-- | Type alias for a seed for a generator.
type Seed = Int

-- | Type alias for the depth of an AST.
type Depth = Int

-- | Type alias for the number of parameters in an AST.
type NrParam = Int

-- | Type alias for the ratio of of grow and full methods used in ramped-half-and-half.
type Ratio = Double

-- | Type alias for the size of the poopulation that is being generated.
type Size = Int

-- | Return a generator for binary operations. This is unifromly distributed amongs the possible values.
rBinOp :: Gen BinaryOperation
rBinOp = elements [minBound .. maxBound]

-- | Returns a generator for a binary expression given two generators for its sub-expressions.
rBinExpr :: Gen ASTExpression -> Gen ASTExpression -> Gen ASTExpression
rBinExpr gExpr1 gExpr2 = do
  op <- rBinOp
  e1 <- gExpr1
  BinOp op e1 <$> gExpr2

-- | Return a generator for unary operations. This is unifromly distributed amongs the possible values.
rUnOp :: Gen UnaryOperation
rUnOp = elements [minBound .. maxBound]

-- | Returns a generator for a unary expression given another generator for its sub-expression.
rUnExpr :: Gen ASTExpression -> Gen ASTExpression
rUnExpr gExpr = do
  op <- rUnOp
  UnOp op <$> gExpr

-- | Return a generator for relationa operations. This is unifromly distributed amongs the possible values.
rRelOp :: Gen RelationalOperation
rRelOp = elements [minBound .. maxBound]

-- | Returns a generator for a relational expression given two generators for its sub-expressions.
rRelExpr :: Gen ASTExpression -> Gen ASTExpression -> Gen ASTExpression
rRelExpr gExpr1 gExpr2 = do
  op <- rRelOp
  e1 <- gExpr1
  RelOp op e1 <$> gExpr2

-- | Returns generator for a constant expression by randomly selecting a value.
rConst :: Gen ASTExpression
rConst = Const <$> arbitrary

-- | Returns a generatro for a parmeter expression by selecting an index that is non-negative and smaller than the given amount of parameters.
rParam :: NrParam -> Gen ASTExpression
rParam m = Param <$> elements [0, 1 .. (m - 1)]

-- | generator for a globaltee expression
rGlobalTee :: Gen ASTExpression -> Gen ASTExpression
rGlobalTee gExpr = GlobalTee <$> gExpr

rGlobalGet :: Gen ASTExpression
rGlobalGet = return GlobalGet

rGlobalSet :: Gen ASTExpression -> Gen ASTExpression -> Gen ASTExpression
rGlobalSet gExpr1 gExpr2 = do
  e1 <- gExpr1
  GlobalSet e1 <$> gExpr2

rIf :: Gen ASTExpression -> Gen ASTExpression -> Gen ASTExpression -> Gen ASTExpression
rIf gC gL gR = do
  c <- gC
  l <- gL
  IfStatement c l <$> gR

rImportedFunction ::
  ImportedFunctions ->
  Algo ->
  Depth ->
  NrParam ->
  Gen ASTExpression
rImportedFunction list algo depth params = do
  (ImportedFunction name nrParam) <- elements list
  ImportedFunctionCall name <$> generateExprList algo depth params list nrParam

type Algo = Depth -> Reader (Depth, NrParam, ImportedFunctions) (Gen ASTExpression)

generateExprList :: Algo -> Depth -> NrParam -> ImportedFunctions -> Int -> Gen [ASTExpression]
generateExprList _ _ _ _ 0 = return []
generateExprList algo depth params functions n = do
  generated <- runReader (algo 0) (depth, params, functions)
  rest <- generateExprList algo depth params functions $ n -1
  return $ generated : rest

-- | GROW algorithm that builds a generator for an ASTExpression.
-- As long as the depth is shorter than the given maximum depth all possible nodes can be chosen.
-- Once the given maximum epth is reached only leaf nodes an be chosen such as a constant or a parameter.
-- The maximum depth and the number of parameters are registered using the Reader monad.
growOne :: Depth -> Reader (Depth, NrParam, ImportedFunctions) (Gen ASTExpression)
growOne d = do
  (maxD, nrParam, functions) <- ask
  let len = length functions
  if d < maxD
    then do
      g1 <- growOne $ d + 1
      g2 <- growOne $ d + 1
      g3 <- growOne $ d + 1
      return $
        frequency
          [ (1, rConst),
            (1, rParam nrParam),
            (length [minBound :: BinaryOperation ..], rBinExpr g1 g2),
            (length [minBound :: UnaryOperation ..], rUnExpr g1),
            (length [minBound :: RelationalOperation ..], rRelExpr g1 g2),
            (1, rGlobalTee g1),
            (1, rGlobalGet),
            (1, rGlobalSet g1 g2),
            (1, rIf g1 g2 g3),
            (len, rImportedFunction functions growOne (d + 1) nrParam)
          ]
    else return $ oneof [rConst, rParam nrParam, rGlobalGet]

-- | Helper function for the GROW algorithm to set the initial QCGen.
growOneInit :: QCGen -> Reader (Depth, NrParam, ImportedFunctions) (Gen ASTExpression)
growOneInit gen = do
  g <- growOne 0
  let seeded = useSeed gen g
  return seeded

-- | FULL algorithm that builds a generator for an ASTExpression.
-- As long as the depth is shorter than the given maximum depth only non-leaf nodes can be selected.
-- Once the given maximum depth is reached only leaf nodes can be chosen such as a constant or a parameter.
-- The maximum depth and the number of parameters are registered using the Reader monad.
fullOne :: Depth -> Reader (Depth, NrParam, ImportedFunctions) (Gen ASTExpression)
fullOne d = do
  (maxD, nrParam, functions) <- ask
  let len = length functions
  if d < maxD
    then do
      f1 <- fullOne $ d + 1
      f2 <- fullOne $ d + 1
      f3 <- fullOne $ d + 1
      return $
        frequency
          [ (length [minBound :: BinaryOperation ..], rBinExpr f1 f2),
            (length [minBound :: UnaryOperation ..], rUnExpr f1),
            (length [minBound :: RelationalOperation ..], rRelExpr f1 f2),
            (10, rGlobalTee f1),
            (1, rGlobalSet f1 f2),
            (1, rIf f1 f2 f3),
            (len, rImportedFunction functions growOne (d + 1) nrParam)
          ]
    else return $ oneof [rConst, rParam nrParam, rGlobalGet]

-- | Helper function for the FULL algorithm to set the initial QCGen.
fullOneInit :: QCGen -> Reader (Depth, NrParam, ImportedFunctions) (Gen ASTExpression)
fullOneInit gen = do
  g <- fullOne 0
  let seeded = useSeed gen g
  return seeded

-- | Returns a list of generators for expressions given an initial QCGen and a ratio of generators
-- that were conceived using the GROW and FULL method.
rampedHalfNHalf :: QCGen -> Depth -> NrParam -> Ratio -> Size -> ImportedFunctions -> [Gen ASTExpression]
rampedHalfNHalf gen d nrParam ratio n functions
  | ratio <= 1 && ratio >= 0 = growList ++ fullList
  | otherwise = error "Please provide a ratio between 0 and 1."
  where
    growN = round $ ratio * fromIntegral n
    fullN = round $ (1 - ratio) * fromIntegral n
    (growSeeds, fullSeeds) = grow2SeedLists gen growN fullN
    growList = generateInitList growSeeds growOneInit
    fullList = generateInitList fullSeeds fullOneInit
    generateInitList gens method =
      [runReader (method g) (fst $ randomR (1, d) g, nrParam, functions) | g <- gens]

-- | Returns a list of generators given the initial seed, maximium depth, number of paramets.
-- ratio of GROW and FULL and the lenght of the list of generators.
-- The list of gnerators are conceived using rampedHalfNHalf.
genASTExpressions :: Seed -> Depth -> NrParam -> Ratio -> Size -> ImportedFunctions -> IO [ASTExpression]
genASTExpressions seed d nrParam ratio n functions = do
  sequence
    [generate g | g <- rampedHalfNHalf (mkQCGen seed) d nrParam ratio n functions]

randomGenerationTest = genASTExpressions 12 6 1 0.5 10 []
