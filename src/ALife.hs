{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ALife where

import AST
import qualified ASTRepresentation as Rep
import qualified Data.ByteString as BS
import Data.Numbers.Primes
import qualified Data.Text as T
import Environment
import ExecuteWasm
import Generators
import GeneticOperations
import Organism
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

generateInitPop :: QCGen -> Double -> Lim -> IO [Creature]
generateInitPop gen start lim = do
  let s = uncurry (*) lim `div` 10 + 1
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
  pop <- generateInitPop g1 s l
  initializeEnvironment n g2 pop l

mutateEnvironment :: QCGen -> Environment Creature -> Ratio -> IO (Environment Creature)
mutateEnvironment gen env ratio = do
  let (g1, g2) = split gen
  let amount = round (ratio * fromIntegral (Environment.getSize env) :: Double)
  let positions = generateRandomPositions g1 env amount
  print "random positions"
  let creatures = getOrgsAt env positions
  print "creatures"
  let orgs = map snd creatures
  let pos = map fst creatures
  print $ length orgs
  mutated <- mutateCreatures g2 orgs
  print "mutated"
  return $ fillInOrgs env (zip pos mutated)

mutateCreatures :: QCGen -> [Creature] -> IO [Creature]
mutateCreatures _ [] = print "last" >> return []
mutateCreatures gen (o : os) = do
  print o
  let (g1, g2) = split gen
  let r = register o
  e <- subTreeMutation g1 (expression o) 1
  print "subtree"
  rest <- mutateCreatures g2 os
  serialized <- serializeExpression e [r]
  return $ Creature e serialized r : rest

executeCreature :: Creature -> IO Creature
executeCreature (Creature e b _) = do
  outcome <- executeModule b
  return $ Creature e b outcome

executeCreatures :: [Creature] -> IO [Creature]
executeCreatures orgs = sequence [executeCreature org | org <- orgs]

reproducable :: Creature -> Bool
reproducable org = isPrime (round (register org) :: Int)

killRandom :: QCGen -> Environment Creature -> Environment Creature
killRandom gen env = nillify env positions
  where
    positions =
      generateRandomPositions gen env (round (0.5 * fromIntegral (Environment.getSize env) :: Double))

reproduce :: QCGen -> Environment Creature -> IO (Environment Creature)
reproduce gen env = reproduceList gen env $ getOrgsPos env

reproduceList :: QCGen -> Environment Creature -> [(Pos, Creature)] -> IO (Environment Creature)
reproduceList _ env [] = return env
reproduceList gen env (o : os) = do
  let (g1, g2) = split gen
  rest <- reproduceList g1 env os
  let pos = fst o
  let creature = snd o
  let nils = getNilNeighbours env pos
  let childPos =
        if null nils
          then selectPosition g2 $ getNeighbours env pos
          else selectPosition g2 nils
  insertOrganismAt rest creature <$> childPos

run :: Environment Creature -> QCGen -> Ratio -> Int -> IO [Environment Creature]
run env _ _ 0 = do
  print (0 :: Int)
  print env
  return [env]
run env gen ratio n = do
  print n
  print env
  let (g1, g2) = split gen
  let posOrg = getOrgsPos env
  let orgs = map snd posOrg
  print orgs
  let positions = map fst posOrg
  executed <- executeCreatures orgs
  print "executed"
  print executed
  let executedEnv = fillInOrgs env $ zip positions executed
  print executedEnv
  mutatedEnv <- mutateEnvironment g1 executedEnv ratio
  print mutatedEnv
  let (g11, g22) = split g2
  if length orgs > floor (0.8 * fromIntegral (Environment.getSize env) :: Double)
    then do
      let killedEnv = killRandom g11 mutatedEnv
      print killedEnv
      killedRun <- run killedEnv g22 ratio (n - 1)
      return $ killedEnv : killedRun
    else do
      reproducedEnv <- reproduce g11 mutatedEnv
      print reproducedEnv
      reproduceRun <- run reproducedEnv g22 ratio (n - 1)
      return $ reproducedEnv : reproduceRun

mainCreature :: Seed -> Double -> Double -> IO ()
mainCreature seed mutationRatio start = do
  let (g1, g2) = split $ mkQCGen seed
  let lim = (5, 5)
  orgs <- generateInitPop g1 start lim
  print orgs
  let (g11, g22) = split g2
  env <- initializeEnvironment Moore g11 orgs lim
  run env g22 mutationRatio 10
  return ()

testCreature = mainCreature 10 0.5 0.5
