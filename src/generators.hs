import AST
import Test.QuickCheck
import Test.QuickCheck.Random
import Seeding
import Control.Monad.Reader
import Test.QuickCheck.Gen

-- was gonna use chooseEnum instead of elements but it is not available anymore apparantly, weird?
rBinOp :: Gen BinaryOperation
rBinOp = elements [Add, Sub, Mul, Div, Min, Max, Copysign]

rBinExpr :: Gen Expression -> Gen Expression -> Gen Expression
rBinExpr gExpr1 gExpr2 = do
  op <- rBinOp
  e1 <- gExpr1
  e2 <- gExpr2
  return $ BinOp op e1 e2

rUnOp :: Gen UnaryOperation
rUnOp = elements [Abs, Neg, Sqrt, Ceil, Floor, Trunc, Nearest]

rUnExpr :: Gen Expression -> Gen Expression
rUnExpr gExpr = do
  op <- rUnOp
  e <- gExpr
  return $ UnOp op e

rRelOp :: Gen RelationalOperation
rRelOp = elements [Eq, Ne, Lt, Gt, Le, Ge]

rRelExpr :: Gen Expression -> Gen Expression -> Gen Expression
rRelExpr gExpr1 gExpr2 = do
  op <- rRelOp
  e1 <- gExpr1
  e2 <- gExpr2
  return $ RelOp op e1 e2

rConst :: Gen Expression
rConst = Const <$> arbitrary

rParam :: Int -> Gen Expression
rParam m = Param <$> elements [0, 1 .. (m-1)]

-- GROWONE generator
growOne :: Int -> Reader (Int, Int) (Gen Expression)
growOne d = do 
  (maxD, nrParam) <- ask
  if d < maxD
    then 
      do
      g1 <- growOne $ d+1
      g2 <- growOne $ d+1
      return $ frequency [(1, rConst), (1, rParam nrParam), (7, rBinExpr g1 g2), (7, rUnExpr g1), (6, rRelExpr g1 g2)]
  else
    return $ oneof [rConst, (rParam nrParam)]

growOneInit :: QCGen -> Reader (Int, Int) (Gen Expression)
growOneInit gen = do
  g <- growOne 0
  let seeded = useSeed gen g
  return seeded

-- FULLONE generator
fullOne :: Int -> Reader (Int, Int) (Gen Expression)
fullOne d = do
  (maxD, nrParam) <- ask
  if d < maxD
    then
      do
      f1 <- fullOne $ d+1
      f2 <- fullOne $ d+1
      return $ frequency [(7, rBinExpr f1 f2), (7, rUnExpr f1), (6, rRelExpr f1 f2)]
  else
    return $ oneof [rConst, (rParam nrParam)]

fullOneInit :: QCGen -> Reader (Int, Int) (Gen Expression)
fullOneInit gen = do
  g <- fullOne 0
  let seeded = useSeed gen g
  return seeded

-- executes the ramped half and half method for population initialization
rampedHalfNHalf :: QCGen -> Int -> Int -> Double -> [Gen Expression]
rampedHalfNHalf gen d nrParam ratio
  | ratio <=1 && ratio >= 0 = growList ++ fullList
  | otherwise = error "Please provide a ratio between 0 and 1."
  where
    (growSeeds, fullSeeds) = grow2SeedLists gen 5 5
    growList = generateInitList growSeeds growOneInit
    fullList = generateInitList fullSeeds fullOneInit
    generateInitList gens method = [runReader (method g) (d, nrParam) | g <- gens]


generateWithSeed :: QCGen -> Gen a -> IO a
generateWithSeed seed (MkGen g) = do 
  return (g seed 30)


test :: IO [Expression]
test = do 
  sequence [generate g | g <- rampedHalfNHalf (mkQCGen 15) 5 10 0.5]


