{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ALife where

import AST
import qualified ASTRepresentation as Rep
import Compatibility
import Data.Aeson
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Matrix
import Data.Ratio
import qualified Data.Text as T
import Environment
import ExecuteWasm
import GHC.Generics
import Generators
import GeneticOperations
import Interaction
import Movement
import Organism
import Running
import Seeding
import SysCall
import System.Directory
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

data SmallCreature = SmallCreature ASTExpression Double Int
  deriving (Eq, Generic)

instance ToJSON SmallCreature

instance Organism Creature where
  genotype creature = newStr
    where
      firstOp = T.unpack $ T.strip $ T.pack $ smallShow $ expression creature
      str =
        firstOp
          ++ "_"
          ++ show (size $ expression creature)
          ++ "_"
          ++ show (getMaxDepth $ expression creature)
          ++ "_"
          ++ show (age creature)
      extra = 16 - length str
      newStr = str ++ replicate extra ' '
  executable creature = age creature > 1
  compatible l r = double >= compatibility
    where
      match = matchingPercentage (expression l) (expression r)
      double = (fromIntegral (numerator match) :: Double) / (fromIntegral (denominator match) :: Double)

instance Show Creature where
  show (Creature e r _ a) =
    show (Rep.generateRepresentation e) ++ ", register= " ++ show r ++ ", age= " ++ show a ++ "\n"

type GenotypePop = [(String, Int)]

-- | Serializes the given list of environments into a jsonfile with the given name.
-- The json file does not consist of the whole organisms but only their genotypes.
serialize :: [Environment Creature] -> String -> IO ()
serialize envs name = do
  let giantList = map envToLists envs
      directory = "./jsonSysCall/"
  createDirectoryIfMissing True directory
  encodeFile (directory ++ name) $ toJSON giantList

genotype' :: Organism a => Life a -> String
genotype' Nil = ""
genotype' (Org o) = genotype o

envToLists :: Environment Creature -> [[(String, [Resource])]]
envToLists env = map (map (Bi.first genotype')) (toLists $ grid env)

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

-- | Generates an initial population of creatures with and a bound for the initial i
-- value of the registers of the creatures and the limits of the environment.
-- The amount of creatures that need to be generated is equal to at least a
-- tenth of the total cells in the environment.
generateInitPop :: QCGen -> Double -> Depth -> Lim -> Int -> IO [Creature]
generateInitPop gen start depth lim divider = do
  let s = uncurry (*) lim `div` divider + 1
  let (g1, g2) = split gen
  ex <- sequence [generate g | g <- rampedHalfNHalf g1 depth 1 0.5 s]
  let starts = generateStart g2 s start
  serializeds <- serializeExpressions ex 1
  return [Creature e st b 0 | ((e, st), b) <- zip (zip ex starts) (map snd serializeds)]

-- | Generates a list of random values for the registers of the creatures
-- with a given bound and size of that list.
generateStart :: QCGen -> Size -> Double -> [Double]
generateStart gen s start = take s $ randomRs (0, start) gen

-- | Initializes a new environment with a given neighbourhood and limits.
-- It also generates a population of creatures and distributes them in the environment.
initEnvironment :: QCGen -> Neighbourhood -> Lim -> Double -> Depth -> Int -> IO (Environment Creature)
initEnvironment gen n l s depth divider = do
  let (g1, g2) = split gen
  pop <- generateInitPop g1 s depth l divider
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
  output <- executeModule serialized [res] 0
  case output of
    Just out -> do
      putStr $ "executed with outcome " ++ show (outcome out)
      return $ changeRegister creature $ outcome out
    Nothing -> error "execution has failed"

mutationChance :: Int
mutationChance = 4

-- | Reproduces the creaturs in the environment if the are able to reproduce.
-- It also is possible by chance that the reproduced creature is mutated.
reproduce ::
  QCGen ->
  Environment Creature ->
  [Runnable Creature] ->
  Int ->
  IO ([Runnable Creature], Environment Creature)
reproduce _ env [] _ = return ([], env)
reproduce gen env ((Runnable creature pos _ _) : rest) mutationRate = do
  let nils = getNilNeighbours env pos
      newCreature = reborn creature
      (g1, g2) = split gen
      (n, g3) = randomR (1, mutationRate) g1
  childPos <-
    if null nils
      then selectPosition g2 $ getNeighbours env pos
      else selectPosition g2 nils
  print childPos
  let newRest = filter (\(Runnable _ p _ _) -> p /= childPos) rest
  if n == 1
    then do
      print "mutate"
      mutated <- mutateCreature g3 newCreature
      print "this is the child"
      putStr $ show mutated
      return
        ( newRest
            ++ [ Runnable creature pos ResourceAquirement Empty,
                 Runnable mutated childPos ResourceAquirement Empty
               ],
          insertOrganismAt env mutated childPos
        )
    else do
      print "don't mutate"
      print "this is the child"
      putStr $ show newCreature
      return
        ( newRest
            ++ [ Runnable creature pos ResourceAquirement Empty,
                 Runnable newCreature childPos ResourceAquirement Empty
               ],
          insertOrganismAt env newCreature childPos
        )

-- | Performs an action on the environment based on the outcome of the execution of the creature.
executeAction ::
  QCGen ->
  Environment Creature ->
  [Runnable Creature] ->
  Double ->
  Int ->
  IO ([Runnable Creature], Environment Creature)
executeAction _ env [] _ _ = return ([], env)
executeAction gen env runnables@(x : xs) outcome mutationRate = do
  print outcome
  case toSysCall outcome of
    Reproduction -> do
      print "reproducing"
      reproduce gen env runnables mutationRate
    Up -> do
      print "moving up"
      tryMovement gen runnables U env
    Down -> do
      print "moving down"
      tryMovement gen runnables D env
    Rght -> do
      print "moving right"
      tryMovement gen runnables R env
    Lft -> do
      print "moving left"
      tryMovement gen runnables L env
    None -> print "do nothing" >> return (xs ++ [x], env)

tryMovement ::
  QCGen ->
  [Runnable Creature] ->
  Move ->
  Environment Creature ->
  IO ([Runnable Creature], Environment Creature)
tryMovement _ [] _ env = return ([], env)
tryMovement gen (Runnable creature pos _ _ : rest) mov env =
  case moveOrg env pos mov of
    Left (newPos, newEnv) -> do
      print newPos
      return (rest ++ [Runnable creature newPos ResourceAquirement Empty], newEnv)
    Right (crossoverOrg, newPos) -> do
      putStr $ "the matchin percentage:" ++ show (matchingPercentage (expression crossoverOrg) (expression creature))
      if compatible creature crossoverOrg
        then do
          print "crossing over"
          print pos
          print newPos
          print creature
          print $ expression creature
          print crossoverOrg
          print $ expression crossoverOrg
          let leftExpr = expression creature
              rightExpr = expression crossoverOrg
              (leftChild, rightChild, crossgen) = crossover gen leftExpr rightExpr
          putStr "leftchild:"
          putStr $ show leftChild
          putStr "rightchild:"
          putStr $ show rightChild
          leftSer <- serializeExpression leftChild 1
          rightSer <- serializeExpression rightChild 1
          (lPos, rPos) <- getChildPositions crossgen env pos newPos
          let lft = Creature leftChild (register creature) leftSer 0
              rght = Creature rightChild (register crossoverOrg) rightSer 0
              newEnv = insertOrganismAt (insertOrganismAt env lft lPos) rght rPos
              leftRunnable = Runnable lft lPos ResourceAquirement Empty
              rightRunnable = Runnable lft rPos ResourceAquirement Empty
              newQueue =
                filter
                  (\(Runnable _ p _ _) -> not (p == lPos || p == rPos))
                  rest
                  ++ [Runnable creature pos ResourceAquirement Empty, leftRunnable, rightRunnable]
          return (newQueue, newEnv)
        else return (rest ++ [Runnable creature pos ResourceAquirement Empty], env)

-- | Returns a list of tuples with the age of the creature at that position and a generator of the position.
getAgePos :: Environment Creature -> [(Int, Gen Pos)]
getAgePos env = [(age creature, return pos) | (pos, creature) <- getOrgsPos env]

-- | Generates an infiniteList with positions of killable organisms.
generateInfiniteKillList :: Environment Creature -> IO [Pos]
generateInfiniteKillList env = do
  print "killPostions 1"
  -- add seed below
  pos <- generate $ frequency $ getAgePos env
  print "killPostions 2"
  rest <- generateInfiniteKillList env
  print "killPostions 3"
  return $ pos : rest

-- | Kills a given amount of organisms in the environment.
kill :: Environment Creature -> Int -> IO (Environment Creature)
kill env amount = do
  positions <- generateInfiniteKillList env
  let killPos = take amount $ List.nub positions
  return $ nillify env killPos

adjustedPosByAge :: [(Pos, Creature)] -> [(Int, Gen Pos)]
adjustedPosByAge = map (\(p, c) -> (age c, return p))

takeN :: QCGen -> Int -> [(Pos, Creature)] -> IO [Pos]
takeN _ 0 _ = return []
takeN _ _ [] = return []
takeN gen n list = do
  let (g', g'') = split gen
  let adjusted = adjustedPosByAge list
  let nonzeros = filter (\(a, _) -> a /= 0) adjusted
  if not (null nonzeros)
    then do
      pos <- generate $ useSeed g' $ frequency adjusted
      let newList = filter (\(p, _) -> p /= pos) list
      rest <- takeN g'' (n - 1) newList
      return $ pos : rest
    else do
      let poss = map fst list
      if length list >= n
        then return $ take n poss
        else return poss

-- | Returns a given number of positions in the envirionment on which the organims can be killed
killPositions :: QCGen -> Environment Creature -> Int -> IO [Pos]
killPositions gen env amount = do
  takeN gen amount $ getOrgsPos env

-- | returns true if for the given total and current amount of organisms in the evironment, there is overpopulation.
killable :: Int -> Int -> Bool
killable total amount = ((fromIntegral amount :: Double) / fromIntegral total) > 0.9

-- | Kills returns if their is overpopulation in the environment.
killableEnv :: Environment Creature -> Bool
killableEnv env = killable (Environment.getSize env) (length $ getAllOrgs env)

run :: Environment Creature -> RunState Creature -> IO [Environment Creature]
run env (RunState _ [] _ _) = do
  print "no organisms found"
  return [env]
run env (RunState iteration runningQueue gen mutationRate) = do
  print ""
  print iteration
  print runningQueue
  if iteration == 0
    then return [env]
    else do
      let (leftGen, rightGen) = split gen
          currentRunnable = head runningQueue
      print "organism:"
      print $ organism currentRunnable
      putStr $ show (expression $ organism currentRunnable)
      (newRunnables, newEnv) <- performAction leftGen env runningQueue mutationRate
      let total = Environment.getSize newEnv
          current = length newRunnables
          newIteration = iteration - 1
      if killable total current
        then do
          let (g', g'') = split rightGen
          print "killing things"
          kills <- killPositions g' newEnv $ div current 2
          let runnablesAfterKilled =
                filter
                  (\(Runnable _ pos _ _) -> pos `notElem` kills)
                  newRunnables
              newState = RunState newIteration runnablesAfterKilled g'' mutationRate
              envAfterKilled = nillify newEnv kills
          print envAfterKilled
          restRun <- run envAfterKilled newState
          return $ env : restRun
        else do
          print "not killing things"
          let newState = RunState newIteration newRunnables rightGen mutationRate
          print newEnv
          restRun <- run newEnv newState
          return $ env : restRun

performAction ::
  QCGen ->
  Environment Creature ->
  [Runnable Creature] ->
  Int ->
  IO ([Runnable Creature], Environment Creature)
performAction _ env [] _ = return ([], env)
performAction gen env (Runnable org pos ResourceAquirement res : rest) _ = do
  print "trying to aquire resources"
  if hasResources env pos
    then do
      newRes <- unsafeGetRandomResource env gen pos
      print "resource:"
      print newRes
      let deletedEnv = deleteSubResources env pos newRes
      return
        (rest ++ [Runnable org pos (nextAction ResourceAquirement) (Res newRes)], deletedEnv)
    else do
      print "no resource found"
      return (rest ++ [Runnable org pos ResourceAquirement res], env)
performAction _ env (Runnable org pos Execution Empty : rest) _ = do
  print "execution without resources is not possible"
  return (rest ++ [Runnable org pos ResourceAquirement Empty], env)
performAction gen env (Runnable org pos Execution (Res res) : rest) _ = do
  print "execution"
  print $ expression org
  executedCreature <- executeCreature org res
  let outcome = register executedCreature
      newCreature = grow executedCreature
  if isNaN outcome
    then do
      -- remove creature if outcome is NaN
      let removedEnv = nillify env [pos]
      return (rest, removedEnv)
    else do
      addedEnv <- addResourceToNeighbours gen env pos outcome
      let addedEnv' = insertOrganismAt addedEnv newCreature pos
      return (rest ++ [Runnable newCreature pos SystemCall Empty], addedEnv')
performAction gen env runnables@(Runnable org _ SystemCall _ : _) mutationRate = do
  print "doing a system call"
  executeAction gen env runnables (register org) mutationRate

-- | Inserts a new organism in the running queue
insertNewOrg :: [Runnable Creature] -> Creature -> Pos -> [Runnable Creature]
insertNewOrg runnables new pos = runnables ++ [Runnable new pos ResourceAquirement Empty]

-- | Prepares the initial state for a simulation.
makeState :: Environment Creature -> Int -> QCGen -> Int -> RunState Creature
makeState env iterations =
  RunState
    iterations
    ( map
        (\(pos, org) -> Runnable org pos ResourceAquirement Empty)
        (List.sortOn (\(_, org :: Creature) -> age org) $ getOrgsPos env)
    )

-- | The main function.
mainCreature :: Seed -> Double -> Int -> Int -> Depth -> Int -> Int -> IO ()
mainCreature seed start iterations l depth mutationRate divider = do
  let name =
        "seed= " ++ show seed
          ++ "_iterations= "
          ++ show iterations
          ++ "_limit= "
          ++ show l
          ++ "_maxDepth= "
          ++ show depth
          ++ "_mutationRate= "
          ++ show mutationRate
          ++ "_divider= "
          ++ show divider
      (g1, g2) = split $ mkQCGen seed
      lim = (l, l)
  putStr "\n\n"
  print name
  env <- initEnvironment g1 Moore lim start depth divider
  print "Init"
  print env
  let firstState = makeState env iterations g2 mutationRate
  envList <- run env firstState
  serialize (env : envList) name

testCreature :: IO ()
testCreature = do
  mainCreature 2 10 1000 5 6 4 10
