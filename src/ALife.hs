{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ALife where

import AST
import qualified ASTRepresentation as Rep
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Numbers.Primes
import qualified Data.Text as T
import Environment
import ExecuteWasm
import GHC.Generics
import Generators
import GeneticOperations
import Organism
import Resource
import System.Random
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import WasmGenerator

-- | Data type that represents a creature in the environment.
data Creature = Creature
  { expression :: ASTExpression, -- The expression that represents the program of the creature.
    bytes :: BS.ByteString, -- The bytestring that is serialized from the expression.
    register :: Double -- Stores the value of the register that contains the state of the creature.
  }
  deriving (Eq, Generic)

data SmallCreature = SmallCreature ASTExpression Double
  deriving (Eq, Generic)

instance ToJSON SmallCreature

instance Organism Creature where
  genotype creature =
    show $
      firstOp
        ++ "_"
        ++ show (size $ expression creature)
        ++ "_"
        ++ show (getMaxDepth $ expression creature)
    where
      firstOp = T.unpack $ T.strip $ T.pack $ smallShow $ expression creature

instance Show Creature where
  show (Creature e _ r) =
    show (Rep.generateRepresentation e) ++ ", register= " ++ show r ++ "\n"

type GenotypePop = [(String, Int)]

changeRegister :: Creature -> Double -> Creature
changeRegister (Creature e b _) = Creature e b

-- | Returns a random register for the creature in the environment at the given position.
-- The random register is uniformly selected from the register of the creature and the resources,
-- if there are any, at the position.
getRandomRegister :: QCGen -> Environment Creature -> Pos -> Creature -> Double
getRandomRegister gen env pos crea =
  case org of
    Nothing -> error "whoops you tried to acces a register at a position where there is no creature"
    Just creature ->
      if creature /= crea
        then error "mismatched given creature and creature at given position."
        else case resources of
          Nothing -> register creature
          Just resList ->
            let totalList = register creature : resList
                randomNb = fst $ randomR (0, length totalList - 1) gen
             in totalList !! randomNb
  where
    org = getOrgAt env pos
    resources = getResourcesAt env pos

-- | Generates an initial population of creatures with and a bound for the initial value of the registers of the creatures and the limits of the environment.
-- The amount of creatures that need to be generated is equal to at least a tenth of the total cells in the environment.
generateInitPop :: QCGen -> Double -> Lim -> IO [Creature]
generateInitPop gen start lim = do
  let s = uncurry (*) lim `div` 10 + 1
  let (g1, g2) = split gen
  ex <- sequence [generate g | g <- rampedHalfNHalf g1 5 1 0.5 s]
  let starts = generateStart g2 s start
  serialized <- sequence [serializeExpression e [st] | (e, st) <- zip ex starts]
  return [Creature e ser st | ((e, ser), st) <- zip (zip ex serialized) starts]

-- | Generates a list of random values for the registers of the creatures with a given bound and size of that list.
generateStart :: QCGen -> Size -> Double -> [Double]
generateStart gen s start = take s $ randomRs (0, start) gen

-- | Initializes a new environment with a given neighbourhood and limits.
-- It also generates a population of creatures and distributes them in the environment.
initEnvironment :: QCGen -> Neighbourhood -> Lim -> Double -> IO (Environment Creature)
initEnvironment gen n l s = do
  let (g1, g2) = split gen
  pop <- generateInitPop g1 s l
  initializeEnvironment n g2 pop l

-- | Mutates the the creatures in the environment.
-- It does not mutate all the creatures. I randomly chooses a number of positions from the environment.
-- When there is a creature at these positions that creature is mutated. It is kind of like a cosmic ray.
mutateEnvironment :: QCGen -> Environment Creature -> Ratio -> IO (Environment Creature)
mutateEnvironment gen env ratio = do
  let (g1, g2) = split gen
  let amount = round (ratio * fromIntegral (Environment.getSize env) :: Double)
  let positions = generateRandomPositions g1 env amount
  let creatures = getOrgsAt env positions
  let orgs = map snd creatures
  let pos = map fst creatures
  mutated <- mutateCreatures g2 orgs
  return $ fillInOrgs env (zip pos mutated)

-- | Performs sun-tree mutation of the given list of creatures.
mutateCreatures :: QCGen -> [Creature] -> IO [Creature]
mutateCreatures _ [] = return []
mutateCreatures gen (o : os) = do
  let (g1, g2) = split gen
  let r = register o
  e <- subTreeMutation g1 (expression o) 1
  rest <- mutateCreatures g2 os
  serialized <- serializeExpression e [r]
  return $ Creature e serialized r : rest

-- | Executes the given list of creatures by updating its register with the outcome of the execution.
executeCreature :: Creature -> IO Creature
executeCreature (Creature e b _) = do
  outcome <- executeModule b
  return $ Creature e b outcome

-- | Executes a list of creaturs.
executeCreatures :: [Creature] -> IO [Creature]
executeCreatures orgs = sequence [executeCreature org | org <- orgs]

-- | Returms True if the given creature is able to reproduce.
reproducable :: Creature -> [Resource] -> Bool
reproducable org res = case getMatch res $ register org of
  Nothing -> False
  Just _ -> True

-- | Randomly kills creatures to control the population.
-- This is achieved by first randomly selecting the half of the position in the environment.
-- When there is a creature at the position the creatures id killed.
killRandom :: QCGen -> Environment Creature -> Environment Creature
killRandom gen env = nillify env positions
  where
    positions =
      generateRandomPositions gen env (round (0.5 * fromIntegral (Environment.getSize env) :: Double))

-- | Reproduces the creaturs in the environment if the are able to reproduce.
reproduce :: QCGen -> Environment Creature -> IO (Environment Creature)
reproduce gen env = do
  let orgs = getOrgsPos env
  reproduceList gen env orgs

-- | Takes a list of creatures with their position and reproduces them if they are able to.
-- The newly produces creature is inserted either on an empty neighbouring position or
-- if there are none then a random neighbouring creature is replaced by the new creature.
reproduceList :: QCGen -> Environment Creature -> [(Pos, Creature)] -> IO (Environment Creature)
reproduceList _ env [] = return env
reproduceList gen env (o : os) = do
  let (g1, g2) = split gen
  rest <- reproduceList g1 env os
  let pos = fst o
  let resources = unsafeGetResources env pos
  let creature = snd o
  let nils = getNilNeighbours env pos
  let childPos =
        if null nils
          then selectPosition g2 $ getNeighbours env pos
          else selectPosition g2 nils
  if reproducable creature resources
    then insertOrganismAt rest creature <$> childPos
    else return rest

-- | randomly switches the register of the creature at .e given position
-- with one of the resources at that position or its own register.
-- When there is no creature or there are no resources at the position the the original environment is returned.
switchRegister :: QCGen -> Environment Creature -> Pos -> Environment Creature
switchRegister gen env pos =
  case org of
    Nothing -> env
    Just creature ->
      case resources of
        Nothing -> env
        Just resList ->
          let totalList = register creature : resList
              randomNb = fst $ randomR (0, length totalList - 1) gen
              newCreature = changeRegister creature $ totalList !! randomNb
           in insertOrganismAt env newCreature pos
  where
    org = getOrgAt env pos
    resources = getResourcesAt env pos

-- | Performs a run.
-- First the Reaper is invoked and randomly kills creature if the environment is more than 80% full.
-- Secondly the creatures in the environment are executed and their register is updated.
-- Thirdly the environment is mutated.
-- Lastly the reproducable creatures are reproduced in the environment.
run :: Environment Creature -> QCGen -> Ratio -> Int -> IO [Environment Creature]
run env _ _ 0 = do
  print "End"
  print env
  return [env]
run env gen ratio n = do
  print n
  let (g1, g2) = split gen
  let posOrg = getOrgsPos env
  let orgs = map snd posOrg
  let positions = map fst posOrg
  let modulo = mod n 4
  let nxt = n -1
  case modulo of
    0 ->
      print "Reaper"
        >> if length orgs
          > floor (0.8 * fromIntegral (Environment.getSize env) :: Double)
          then do
            let killedEnv = killRandom g1 env
            print killedEnv
            killedRun <- run killedEnv g2 ratio nxt
            return $ killedEnv : killedRun
          else do
            print env
            rest <- run env g2 ratio nxt
            return $ env : rest
    3 -> do
      print "Execution"
      executed <- executeCreatures orgs
      let newEnv = fillInOrgs env $ zip positions executed
      print newEnv
      rest <- run newEnv g2 ratio nxt
      return $ newEnv : rest
    2 -> do
      print "Mutation"
      newEnv <- mutateEnvironment g1 env ratio
      print newEnv
      rest <- run newEnv g2 ratio nxt
      return $ newEnv : rest
    _ -> do
      print "reproduction"
      newEnv <- reproduce g1 env
      print newEnv
      rest <- run newEnv g2 ratio nxt
      return $ newEnv : rest

-- | The main function.
mainCreature :: Seed -> Double -> Double -> Int -> IO ()
mainCreature seed mutationRatio start iterations = do
  let (g1, g2) = split $ mkQCGen seed
  let lim = (5, 5)
  env <- initEnvironment g1 Moore lim start
  print "Init"
  print env
  run env g2 mutationRatio (iterations * 4)
  return ()

testCreature = mainCreature 10 0.5 0.5 50
