{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ALife where

import AST
import qualified ASTRepresentation as Rep
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Numbers.Primes
import qualified Data.Text as T
import Environment
import ExecuteWasm
import GHC.Generics
import Generators
import GeneticOperations
import Organism
import Resource
import Seeding
import SysCall
import System.Random
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import WasmGenerator

-- | Data type that represents a creature in the environment.
data Creature = Creature
  { expression :: ASTExpression, -- The expression that represents the program of the creature.
    register :: Double, -- Stores the value of the register that contains the state of the creature.
    bytestring :: BS.ByteString,
    age :: Int
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
  executable creature = age creature > 1

instance Show Creature where
  show (Creature e r _ a) =
    show (Rep.generateRepresentation e) ++ ", register= " ++ show r ++ ", age= " ++ show a ++ "\n"

type GenotypePop = [(String, Int)]

-- | Ages the creature by one.
grow :: Creature -> Creature
grow (Creature e r b a) = Creature e r b $ a + 1

-- | Returns the same creature but with age 0
reborn :: Creature -> Creature
reborn (Creature e r b _) = Creature e r b 0

-- | Changes the register to the given value.
changeRegister :: Creature -> Double -> Creature
changeRegister (Creature e _ b a) r = Creature e r b a

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
  serializeds <- serializeExpressions ex 1
  return [Creature e st b 0 | ((e, st), b) <- zip (zip ex starts) (map snd serializeds)]

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

-- | Performs sun-tree mutation of the given list of creatures.
mutateCreature :: QCGen -> Creature -> IO Creature
mutateCreature gen creature = do
  let r = register creature
  e <- subTreeMutation gen (expression creature) 1
  serialized <- serializeExpression e 1
  return $ Creature e r serialized (age creature)

-- | Executes the given list of creatures by updating its register with the outcome of the execution.
executeCreature :: Creature -> Resource -> IO Creature
executeCreature creature res = do
  let serialized = bytestring creature
  outcome <- executeModule serialized res
  return $ changeRegister creature outcome

mutationChance :: Int
mutationChance = 4

-- | Reproduces the creaturs in the environment if the are able to reproduce.
-- It also is poxxible by chance that the reproduced creature is mutated.
reproduce :: QCGen -> Environment Creature -> Creature -> Pos -> IO (Environment Creature)
reproduce gen env creature pos = do
  let nils = getNilNeighbours env pos
  let newCreature = reborn creature
  let (g1, g2) = split gen
  let (n, g3) = randomR (1, mutationChance) g1
  childPos <-
    if null nils
      then selectPosition g2 $ getNeighbours env pos
      else selectPosition g2 nils
  print "test"
  print n
  if n == 1
    then do
      print "mutation"
      mutated <- mutateCreature g3 newCreature
      return $ insertOrganismAt env mutated childPos
    else do
      print "test test"
      return $ insertOrganismAt env newCreature childPos

-- | Performs an action on the environment based on the outcome of the execution of the creature.
perfromAction :: QCGen -> Environment Creature -> Creature -> Pos -> Double -> IO (Environment Creature)
perfromAction gen env creature pos outcome = case toSysCall outcome of
  Reproduction -> print "reproducing" >> reproduce gen env creature pos
  None -> return env

-- | Returns a list of tuples with the age of the creature at that position and a generator of the position.
getAgePos :: Environment Creature -> [(Int, Gen Pos)]
getAgePos env = [(age creature, return pos) | (pos, creature) <- getOrgsPos env]

-- | Generates an infiniteList with positions of killable organisms.
generateInfiniteKillList :: Environment Creature -> IO [Pos]
generateInfiniteKillList env = do
  pos <- generate $ frequency $ getAgePos env
  rest <- generateInfiniteKillList env
  return $ pos : rest

-- | Kills a given amount of organisms in the environment.
kill :: Environment Creature -> Int -> IO (Environment Creature)
kill env amount = do
  positions <- generateInfiniteKillList env
  let killPositions = take amount $ List.nub positions
  return $ nillify env killPositions

-- | returns true if for the given total and current amount of organisms in the evironment, there is overpopulation.
killable :: Int -> Int -> Bool
killable total amount = ((fromIntegral amount :: Double) / fromIntegral total) > 0.9

-- | Kills returns if their is overpopulation in the environment.
killableEnv :: Environment Creature -> Bool
killableEnv env = killable (Environment.getSize env) (length $ getAllOrgs env)

-- | Performs a run.
-- First the Reaper is invoked and randomly kills creature if the environment is more than 80% full.
-- Secondly the creatures in the environment are executed and their register is updated.
-- Thirdly the environment is mutated.
-- Lastly the reproducable creatures are reproduced in the environment.
run :: Environment Creature -> QCGen -> Pos -> Int -> IO [Environment Creature]
run env _ _ 0 = do
  print "End"
  print env
  return [env]
run env gen pos n = do
  let nextPos = Environment.next env pos
  if hasOrg env pos
    then do
      let creature = unsafeGetOrgAt env pos
      let agedCreature = grow creature
      if hasResources env pos && executable agedCreature
        then do
          print n
          print pos
          let (g1, g2) = split gen
          let (g21, g22) = split g2
          print $ expression agedCreature
          resource <- unsafeGetRandomResource env g1 pos
          print resource
          executedCreature <- executeCreature agedCreature resource
          let outcome = register executedCreature
          envAfterAction <- perfromAction g2 env executedCreature pos outcome
          let envWithoutRes = deleteSubResources envAfterAction pos resource
          print $ "outcome: " ++ show outcome
          distributedEnv <- insertResourceAtNeighbour g21 envWithoutRes outcome pos
          let envAfterInsertion = insertOrganismAt distributedEnv executedCreature pos
          let total = Environment.getSize envAfterInsertion
          let current = length $ getAllOrgs envAfterInsertion
          if killable total current
            then do
              envAfterKilled <- kill envAfterInsertion $ div current 2
              print envAfterKilled
              rest <- run envAfterKilled g22 nextPos $ n - 1
              return $ envAfterKilled : rest
            else do
              print envAfterInsertion
              rest <- run envAfterInsertion g22 nextPos $ n - 1
              return $ envAfterInsertion : rest
        else run (insertOrganismAt env agedCreature pos) gen nextPos $ n - 1
    else run env gen nextPos $ n - 1

-- | The main function.
mainCreature :: Seed -> Double -> Double -> Int -> IO ()
mainCreature seed mutationRatio start iterations = do
  let (g1, g2) = split $ mkQCGen seed
  let lim = (5, 5)
  env <- initEnvironment g1 Moore lim start
  print "Init"
  print env
  let first = firstPos
  run env g2 first (iterations * Environment.getSize env)
  return ()

testCreature = mainCreature 10 0.5 0.5 15
