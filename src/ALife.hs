{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ALife where

import AST
import qualified ASTRepresentation as Rep
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Numbers.Primes
import qualified Data.Text as T
import Debug.Trace
import Environment
import ExecuteWasm
import GHC.Generics
import Generators
import GeneticOperations
import Organism
import System.Random
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import WasmGenerator

-- | Data type that represents a creature in the environment.
data Creature = Creature
  { expression :: ASTExpression, -- The expression that represents the program of the creature.
    register :: Double -- Stores the value of the register that contains the state of the creature.
  }
  deriving (Eq, Generic, Ord)

instance ToJSON Creature

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
  show (Creature e r) =
    show (Rep.generateRepresentation e) ++ ", register= " ++ show r

type GenotypePop = [(String, Int)]

type Bytes = M.Map Creature BS.ByteString

data Run = Run {environments :: [Environment Creature]}
  deriving (Generic)

instance ToJSON Run

changeRegister :: Creature -> Double -> Creature
changeRegister (Creature e _) = Creature e

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
  return [Creature e st | (e, st) <- zip ex starts]

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
mutateEnvironment :: QCGen -> Environment Creature -> Ratio -> StateT Bytes IO (Environment Creature)
mutateEnvironment gen env ratio = do
  let (g1, g2) = split gen
  let amount = round (ratio * fromIntegral (Environment.getSize env) :: Double)
  let positions = generateRandomPositions g1 env amount
  let creatures = getOrgsAt env positions
  let orgs = map snd creatures
  let pos = map fst creatures
  mutated <- liftIO $ mutateCreatures g2 orgs
  bytes <- get
  let afterDeletion = deleteOrganisms bytes orgs
  afterExtraction <- liftIO $ insertBytes mutated afterDeletion
  put afterExtraction
  return $ fillInOrgs env (zip pos mutated)

-- | Performs sub-tree mutation of the given list of creatures.
mutateCreatures :: QCGen -> [Creature] -> IO [Creature]
mutateCreatures _ [] = liftIO $ return []
mutateCreatures gen (o : os) = do
  let (g1, g2) = split gen
  let r = register o
  e <- subTreeMutation g1 (expression o) 1
  rest <- mutateCreatures g2 os
  return $ Creature e r : rest

-- | Serializes the creatures in the given environment into a Map.
serializeEnvironment :: Environment Creature -> IO Bytes
serializeEnvironment env = insertBytes creatures M.empty
  where
    creatures = getAllOrgs env

-- | Serializes the given list of creatures and puts the resul into the given map.
insertBytes :: [Creature] -> Bytes -> IO Bytes
insertBytes [] bytes = return bytes
insertBytes (c : cs) bytes = do
  serialized <- serializeExpression (expression c) [register c]
  insertBytes cs $ M.insert c serialized bytes

-- | Executes the given list of creatures by updating its register with the outcome of the execution.
executeCreature :: Creature -> BS.ByteString -> IO Creature
executeCreature creature bstring = do
  outcome <- executeModule bstring
  return $ Creature (expression creature) outcome

-- | Executes a list of creaturs.
executeCreatures :: Environment Creature -> StateT Bytes IO (Environment Creature)
executeCreatures env = do
  let posOrg = getOrgsPos env
  let orgs = map snd posOrg
  let positions = map fst posOrg
  bytes <- get
  liftIO $ print $ M.size bytes
  executed <- liftIO $ sequence [executeCreature org $ extractByte org bytes | org <- orgs]
  let newEnv = fillInOrgs env $ zip positions executed
  serialized <- liftIO $ serializeEnvironment newEnv
  put serialized
  return newEnv

extractByte :: Creature -> Bytes -> BS.ByteString
extractByte creature bytes =
  case M.lookup creature bytes of
    Nothing -> trace ("error " ++ show creature) $ error "Found no matching bytestring for given creature"
    Just bstring -> bstring

-- | Randomly kills creatures to control the population.
-- This is achieved by first randomly selecting the half of the position in the environment.
-- When there is a creature at the position the creatures id killed.
killRandom :: QCGen -> Environment Creature -> StateT Bytes IO (Environment Creature)
killRandom gen env = do
  bytes <- get
  put $ deleteOrganisms bytes orgs
  return $ nillify env positions
  where
    positions =
      generateRandomPositions gen env (round (0.5 * fromIntegral (Environment.getSize env) :: Double))
    orgs = getOrganisms env positions

deleteOrganisms :: Bytes -> [Creature] -> Bytes
deleteOrganisms = foldl (flip M.delete)

-- | Returms True if the given creature is able to reproduce.
reproducable :: Creature -> Bool
reproducable org = True -- isPrime (round (register org) :: Int)

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
  let creature = snd o
  let nils = getNilNeighbours env pos
  let childPos =
        if null nils
          then selectPosition g2 $ getNeighbours env pos
          else selectPosition g2 nils
  if reproducable creature
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
run :: Environment Creature -> QCGen -> Ratio -> Int -> StateT Bytes IO [Environment Creature]
run env _ _ 0 = do
  liftIO $ print "The end"
  liftIO $ print env
  return [env]
run env gen ratio n = do
  liftIO $ print n
  let (g1, g2) = split gen
  let posOrg = getOrgsPos env
  let orgs = map snd posOrg
  let modulo = mod n 4
  let nxt = n -1
  case modulo of
    0 ->
      if length orgs
        > floor (0.8 * fromIntegral (Environment.getSize env) :: Double)
        then do
          liftIO $ print "The Reaper"
          killedEnv <- killRandom g1 env
          liftIO $ print killedEnv
          killedRun <- run killedEnv g2 ratio nxt
          return $ killedEnv : killedRun
        else do
          liftIO $ print "The Reaper"
          liftIO $ print env
          rest <- run env g2 ratio nxt
          return $ env : rest
    3 -> do
      liftIO $ print "Execution"
      liftIO $ print $ length orgs
      bytes <- get
      liftIO $ print $ M.keys bytes
      newEnv <- executeCreatures env
      liftIO $ print newEnv
      rest <- run newEnv g2 ratio nxt
      return $ newEnv : rest
    2 -> do
      liftIO $ print "Mutation"
      newEnv <- mutateEnvironment g1 env ratio
      liftIO $ print newEnv
      rest <- run newEnv g2 ratio nxt
      return $ newEnv : rest
    _ -> do
      liftIO $ print "Reproduction"
      newEnv <- liftIO $ reproduce g1 env
      liftIO $ print newEnv
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
  initMap <- serializeEnvironment env
  runStateT (run env g2 mutationRatio (iterations * 4)) initMap
  print "the end"
  return ()

testCreature = mainCreature 10 0.5 0.5 10
