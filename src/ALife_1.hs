{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ALife_1 where

import AST
import ASTRepresentation
import qualified Data.ByteString as BS
import Data.List
--import Data.Numbers.Primes
import qualified Data.Map as M
import Data.Serialize
import qualified Data.Text as T
import ExecuteWasm
import GHC.Generics
import Generators
import GeneticOperations
import GraphicalAnalysis
import Organism
import System.Directory
import System.Random
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import WasmGenerator

barChartPath :: String
barChartPath = "./graphics/barcharts/"

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
    show (generateRepresentation e) ++ ", register= " ++ show r ++ "\n"

type GenotypePop = [(String, Int)]

orgListToExprs :: [[MVP]] -> [[ASTExpression]]
orgListToExprs = map (map expression)

generateInitPop :: QCGen -> Int -> Double -> Int -> Double -> IO [MVP]
generateInitPop gen d ratio n start = do
  expr <- sequence [generate g | g <- rampedHalfNHalf gen d 1 ratio n]
  serialized <- sequence [serializeExpression e [start] | e <- expr]
  return [MVP e s start | (e, s) <- zip expr serialized]

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
    let reg = register o
    expr <- subTreeMutation gen (expression o) 1
    rest <- mutateOrgList (fst $ split gen) os (index + 1) is
    serialized <- serializeExpression expr [reg]
    return $ MVP expr serialized reg : rest
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

run :: [MVP] -> QCGen -> Double -> Int -> Int -> IO [[MVP]]
run orgs _ _ _ 0 = do
  print (0 :: Int)
  print orgs
  return [orgs]
run orgs gen ratio m n = do
  print n
  print orgs
  let (g1, g2) = split gen
  executed <- executeMVPs orgs
  mutated <- mutateMVPs g1 executed ratio
  let (g11, g22) = split g2
  if length mutated > m
    then do
      let killed = reproduce $ killRandom mutated g11
      killedRun <- run killed g22 ratio m (n - 1)
      return $ orgs : killedRun
    else do
      let nonKilled = reproduce mutated
      nonKilledRun <- run nonKilled g22 ratio m (n - 1)
      return $ orgs : nonKilledRun

fancyShowList :: Show a => [[a]] -> String
fancyShowList [] = ""
fancyShowList (x : xs) = show x ++ "\n" ++ fancyShowList xs

countGeno :: Organism a => [a] -> M.Map String Int
countGeno =
  foldr
    (\o -> M.unionWith (+) (M.fromList [(genotype o, 1)]))
    M.empty

countGenotypes :: Organism a => [[a]] -> [[(String, Int)]]
countGenotypes = map (M.toList . countGeno)

createPieCharts :: [[MVP]] -> String -> IO ()
createPieCharts orgs name = do
  let path = barChartPath ++ name ++ "Dir/"
  createDirectoryIfMissing True path
  makePieCharts genos $ path ++ name
  where
    genos = countGenotypes orgs

main :: String -> IO ()
main name = do
  orgs <- generateInitPop (mkQCGen 10) 5 0.5 10 4
  finalOrgs <- run orgs (mkQCGen 10) 0.5 10 10
  let exprs = orgListToExprs finalOrgs
  createPieCharts finalOrgs name
  mainchart exprs
  let path = "./src/tests/" ++ name
  writeFile path (fancyShowList finalOrgs)
