{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ALife where

import AST
import qualified ASTRepresentation as Rep
import qualified Data.ByteString as BS
import Data.List
--import Data.Numbers.Primes
import Data.Serialize
import qualified Data.Text as T
import Environment
import ExecuteWasm
import GHC.Generics
import GeneralUtils
import Generators
import GeneticOperations
import GraphicalAnalysis
import Organism
import Run
import SerializeUtils
import System.Random
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import WasmGenerator

data Creature = Creature
  { expression :: ASTExpression,
    bytes :: BS.ByteString,
    register :: Double
  }

instance Organism Creature where
  genotype mvp =
    firstOp
      ++ "_"
      ++ show (size $ expression mvp)
      ++ "_"
      ++ show (getMaxDepth $ expression mvp)
    where
      firstOp = T.unpack $ T.strip $ T.pack $ smallShow $ expression mvp

instance Show Creature where
  show (Creature e _ r) =
    show (Rep.generateRepresentation e) ++ ", register= " ++ show r ++ "\n"

type GenotypePop = [(String, Int)]

orgListToExprs :: [[Creature]] -> [[ASTExpression]]
orgListToExprs = map (map expression)

generateInitPop :: QCGen -> Double -> IO [Creature]
generateInitPop gen start = do
  let s = 10
  let (g1, g2) = split gen
  ex <- sequence [generate g | g <- rampedHalfNHalf g1 5 1 0.5 s]
  let starts = generateStart g2 s start
  serialized <- sequence [serializeExpression e [st] | (e, st) <- zip ex starts]
  return [Creature e ser st | ((e, ser), st) <- zip (zip ex serialized) starts]

generateStart :: QCGen -> Size -> Double -> [Double]
generateStart gen s start = take s $ randomRs (0, start) gen

initEnvironment :: QCGen -> Neighbourhood -> Lim -> Double -> IO (Environment Creature)
initEnvironment gen n l s = do
  let (g1, g2) = split gen
  pop <- generateInitPop g1 s
  initializeEnvironment n g2 pop l

mutateEnvironment :: QCGen -> Environment Creature -> Ratio -> IO (Environment Creature)
mutateEnvironment gen env ratio = do


mutateMVPs :: QCGen -> [MVP] -> Double -> IO [MVP]
mutateMVPs _ orgs 0 = return orgs
mutateMVPs gen orgs ratio = do
  let n = round $ ratio * fromIntegral (length orgs)
  let nextGen = fst $ split gen
  let indexes = take n $ nub $ randomRs (0, length orgs - 1) nextGen
  mutateOrgList gen orgs 0 indexes

mutateOrgList :: QCGen -> [MVP] -> Int -> [Int] -> IO [MVP]
mutateOrgList _ orgs _ [] = return orgs
mutateOrgList _ [] _ _ = return []
mutateOrgList gen (o : os) index (i : is)
  | index == i = do
    let r = register o
    e <- subTreeMutation gen (expression o) 1
    rest <- mutateOrgList (fst $ split gen) os (index + 1) is
    serialized <- serializeExpression e [r]
    return $ MVP e serialized r : rest
  | index > i = do
    rest <- mutateOrgList gen os (index + 1) is
    return $ o : rest
  | otherwise = do
    rest <- mutateOrgList g

executeCreature :: Creature -> IO Creature
executeCreature (Creature e b _) = do
  outcome <- executeModule b
  return $ Creature e b outcome

executeCreatures :: [Creature] -> IO [Creature]
executeCreatures orgs = sequence [executeCreature org | org <- orgs]

reproducable :: Creature -> Bool
reproducable org = mod (round (register org) :: Int) 13 == 0

reproduce :: [Creature] -> [Creature]
reproduce orgs = orgs ++ [org | org <- orgs, reproducable org]

run :: [Creature] -> QCGen -> Ratio -> Int -> IO [[Creature]]
run orgs gen ratio m = run' orgs gen ratio m m

run' :: [Creature] -> QCGen -> Ratio -> Int -> Int -> IO [[Creature]]
run' orgs _ _ _ 0 = do
  print (0 :: Int)
  print orgs
  return [orgs]
run' orgs gen ratio m n = do
  print n
  print orgs
  let (g1, g2) = split gen
  executed <- executeCreatures orgs
  mutated <- mutateCreatures g1 executed ratio
  let (g11, g22) = split g2
  if length mutated > m
    then do
      let killed = reproduce $ killRandom mutated g11
      killedRun <- run' killed g22 ratio m (n - 1)
      return $ orgs : killedRun
    else do
      let nonKilled = reproduce mutated
      nonKilledRun <- run' nonKilled g22 ratio m (n - 1)
      return $ orgs : nonKilledRun

mainCreature :: Seed -> Double -> Double -> IO ()
mainCreature seed mutationRatio start = do
  let (g1, g2) = split $ mkQCGen seed
  orgs <- generateInitPop g1 start
  return ()

testCreature = mainCreature 10 5 0.5 0.5 10 10 10
