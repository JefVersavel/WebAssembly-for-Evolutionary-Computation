{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ALife_1 where

import AST
import qualified ASTRepresentation as Rep
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.List
--import Data.Numbers.Primes
import Data.Serialize
import qualified Data.Text as T
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

data MVP = MVP
  { expression :: ASTExpression,
    bytes :: BS.ByteString,
    register :: Double
  }

data ShortMVP = ShortMVP
  { expr :: ASTExpression,
    reg :: Double
  }
  deriving (Generic)

instance Serialize ShortMVP

instance Organism ShortMVP where
  genotype mvp =
    firstOp
      ++ "_"
      ++ show (size $ expr mvp)
      ++ "_"
      ++ show (getMaxDepth $ expr mvp)
    where
      firstOp = T.unpack $ T.strip $ T.pack $ smallShow $ expr mvp

instance Organism MVP where
  genotype mvp =
    firstOp
      ++ "_"
      ++ show (size $ expression mvp)
      ++ "_"
      ++ show (getMaxDepth $ expression mvp)
    where
      firstOp = T.unpack $ T.strip $ T.pack $ smallShow $ expression mvp

instance Show MVP where
  show (MVP e _ r) =
    show (Rep.generateRepresentation e) ++ ", register= " ++ show r ++ "\n"

instance Show ShortMVP where
  show (ShortMVP e r) =
    show (Rep.generateRepresentation e) ++ ", register= " ++ show r ++ "\n"

type GenotypePop = [(String, Int)]

data MVPRun = MVPRun Seed Depth Ratio Double Size Size Double [[ShortMVP]]
  deriving (Generic, Show)

instance Serialize MVPRun

instance Run MVPRun where
  getName (MVPRun seed d ratio mutationRatio s maxSize start _) =
    "MVP_seed="
      ++ show seed
      ++ "_depth="
      ++ show d
      ++ "_ratio="
      ++ show ratio
      ++ "_size="
      ++ show s
      ++ "_maxSize="
      ++ show maxSize
      ++ "_mutationRatio="
      ++ show mutationRatio
      ++ "_start="
      ++ show start

getOrgList :: MVPRun -> [[ShortMVP]]
getOrgList (MVPRun _ _ _ _ _ _ _ orgs) = orgs

mvpToShort :: MVP -> ShortMVP
mvpToShort (MVP e _ r) = ShortMVP e r

shortenListList :: [[MVP]] -> [[ShortMVP]]
shortenListList mvps = [map mvpToShort list | list <- mvps]

orgListToExprs :: [[MVP]] -> [[ASTExpression]]
orgListToExprs = map (map expression)

generateInitPop :: QCGen -> Depth -> Ratio -> Size -> Double -> IO [MVP]
generateInitPop gen d ratio n start = do
  ex <- sequence [generate g | g <- rampedHalfNHalf gen d 1 ratio n]
  serialized <- sequence [serializeExpression e [start] | e <- ex]
  return [MVP e s start | (e, s) <- zip ex serialized]

executeMVP :: MVP -> IO MVP
executeMVP (MVP e b _) = do
  outcome <- executeModule b
  return $ MVP e b outcome

executeMVPs :: [MVP] -> IO [MVP]
executeMVPs orgs = sequence [executeMVP org | org <- orgs]

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
    rest <- mutateOrgList gen os (index + 1) (i : is)
    return $ o : rest

killRandom :: [MVP] -> QCGen -> [MVP]
killRandom [] _ = []
killRandom [o] _ = [o]
killRandom (o : os) gen
  | kill = killRandom os nextGen
  | otherwise = o : killRandom os nextGen
  where
    (kill :: Bool, nextGen) = random gen

killFirst :: [MVP] -> Double -> [MVP]
killFirst orgs ratio = drop n orgs
  where
    n = floor $ ratio * fromIntegral (length orgs)

reproducable :: MVP -> Bool
reproducable org = mod (round (register org) :: Int) 13 == 0

reproduce :: [MVP] -> [MVP]
reproduce orgs = orgs ++ [org | org <- orgs, reproducable org]

run :: [MVP] -> QCGen -> Ratio -> Int -> IO [[MVP]]
run orgs gen ratio m = run' orgs gen ratio m m

run' :: [MVP] -> QCGen -> Ratio -> Int -> Int -> IO [[MVP]]
run' orgs _ _ _ 0 = do
  print (0 :: Int)
  print orgs
  return [orgs]
run' orgs gen ratio m n = do
  print n
  print orgs
  let (g1, g2) = split gen
  executed <- executeMVPs orgs
  mutated <- mutateMVPs g1 executed ratio
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

mainMVP :: Seed -> Depth -> Ratio -> Double -> Size -> Size -> Double -> IO ()
mainMVP seed d ratio mutationRatio s maxSize start = do
  let (g1, g2) = split $ mkQCGen seed
  orgs <- generateInitPop g1 d ratio s start
  finalOrgs <- run orgs g2 mutationRatio maxSize
  let exprs = orgListToExprs finalOrgs
  let runs = MVPRun seed d ratio mutationRatio s maxSize start $ shortenListList finalOrgs
  let name = getName runs
  A.encodeFile ("json/" ++ show name) $ countGenotypes $ getOrgList runs

testMVP = mainMVP 10 5 0.5 0.5 10 10 10
