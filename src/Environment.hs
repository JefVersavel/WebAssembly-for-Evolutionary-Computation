{-# LANGUAGE GADTs #-}

module Environment where

import qualified Data.List as List
import Data.Matrix
import Data.Maybe
import GeneralUtils
import Organism
import Seeding
import qualified System.Random as R
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Random

type Resource = Double

type Pos = (Int, Int)

type Lim = Pos

-- | Reprents a lifeform in the environment,
-- it can either be Nil which means that there is no life form in the cell or it can be an Organism.
data Life a = Organism a => Nil | Org a

-- | Represents a cell in the environemnt with its lifeform and a list of resources that can be found in that cell.
type Cell a = (Life a, [Resource])

instance (Show a, Organism a) => Show (Life a) where
  show Nil = "Nil"
  show (Org org) = genotype org

-- | Represents the environment with cells and the neighbourhood and limits of the environment.
data Environment a = Organism a =>
  Env
  { grid :: Matrix (Cell a),
    neighbourhood :: Neighbourhood,
    limits :: Lim,
    low :: Int
  }

instance (Show a, Organism a) => Show (Environment a) where
  show env = showCells $ toLists $ grid env

-- | Returns the x coordinate of the given position.
getX :: Pos -> Int
getX (x, _) = x

-- | Returns the y coordinate of the given position.
getY :: Pos -> Int
getY (_, y) = y

-- | Returns a textual representation of a list of rows from the grid.
showCells :: Organism a => [[Cell a]] -> String
showCells [] = ""
showCells (row : rest) = showRow row ++ "\n" ++ showCells rest

-- | Returns a textual representation of a row on the grid.
showRow :: Organism a => [Cell a] -> String
showRow [] = ""
showRow [c] = show c
showRow (c : cs) = show c ++ ", " ++ showRow cs

-- | The neighbourhood of an environment which affects wich cells are neighbours of eachother.
-- https://en.wikipedia.org/wiki/Moore_neighborhood
-- https://en.wikipedia.org/wiki/Von_Neumann_neighborhood
data Neighbourhood = Moore | VonNeumann
  deriving (Show)

-- functions to work with the limits of hte environmnet

-- | Returns the limits of the environment
getLim :: Environment a -> Lim
getLim = limits

-- | Returns the limit of the 1st dimension.
getXLim :: Environment a -> Int
getXLim env = fst $ limits env

-- | Returns the limit of the 2nd dimentsion.
getYLim :: Environment a -> Int
getYLim env = snd $ limits env

-- | Returns the amount of cells in the environment.
getSize :: Environment a -> Int
getSize env = x * y
  where
    (x, y) = limits env

-- | Returns True if a given position is within the limits of the environment.
legalPos :: Pos -> Lim -> Bool
legalPos (x, y) (mx, my) = x >= 1 && x <= mx && y >= 1 && y <= my

-- | Returns True if the given limit is non-negative in both dimensions.
legalLimits :: Lim -> Bool
legalLimits (x, y) = x >= 0 && y >= 0

-- | Returns all the positions that make up and environment given its limit.
getAllPos :: Lim -> [Pos]
getAllPos (mx, my) = [(x, y) | x <- [1 .. mx], y <- [1 .. my]]

-- | Returns all the positions in the environment in a random order.
getPermutedPos :: QCGen -> Environment a -> IO [Pos]
getPermutedPos gen env = QC.generate $ useSeed gen $ QC.shuffle $ getAllPos $ limits env

-- | Returns the first position of the matrix in the environment.
firstPos :: Pos
firstPos = (1, 1)

-- | Returns the last postition of the matrix in the environment.
lastPos :: Environment a -> Pos
lastPos env = (getXLim env, getYLim env)

-- | Returns the next position relative to the given postion,
next :: Environment a -> Pos -> Pos
next env pos
  | pos == lastPos env = firstPos
  | otherwise = add
  where
    add
      | y < ylim = (x, y + 1)
      | otherwise = (x + 1, 1)
    x = getX pos
    y = getY pos
    ylim = getYLim env

-- | Returns the next position that has and organism and resources.
nextOrgRes :: Organism a => Environment a -> Pos -> Pos
nextOrgRes env pos
  | hasOrg env nextPos && hasResources env nextPos = nextPos
  | otherwise = nextOrgRes env nextPos
  where
    nextPos = next env pos

-- | Returns the adjacent positions of a given position.
-- The adjacent positions are are the positions above, below and next to the given position.
adjacentPos :: Pos -> [Pos]
adjacentPos (x, y) = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

-- | Returns the diagonal positions relative to the given position.
diagonalPos :: Pos -> [Pos]
diagonalPos (x, y) = [(x -1, y -1), (x + 1, y + 1), (x + 1, y -1), (x -1, y + 1)]

-- | Returns the neighbours of a position in an environment.
-- This takes into account the environments neighbourhood.
getNeighbours :: Organism a => Environment a -> Pos -> [Pos]
getNeighbours (Env _ Moore lim _) p =
  [neighbour | neighbour <- adjacentPos p ++ diagonalPos p, legalPos neighbour lim]
getNeighbours (Env _ VonNeumann lim _) p =
  [neighbour | neighbour <- adjacentPos p, legalPos neighbour lim]

-- | Returns the cell for the given position if the given position is legal.
getCellAt :: Environment a -> Pos -> Maybe (Cell a)
getCellAt env p
  | legalPos p $ limits env = Just $ uncurry unsafeGet p $ grid env
  | otherwise = Nothing

-- | Returns The organism at the given cell if it contains an organism.
-- It returns Nothing if there was no organism.
getOrg :: Organism a => Cell a -> Maybe a
getOrg (Org o, _) = Just o
getOrg _ = Nothing

-- | Returns the resources in a given cell.
getResources :: Cell a -> [Resource]
getResources (_, r) = r

-- | Returns True if the given cell has a non-zero amount of resources.
containsResources :: Cell a -> Bool
containsResources (_, r)
  | List.null r = False
  | otherwise = True

-- | Returns the resources at a given position.
getResourcesAt :: Environment a -> Pos -> Maybe [Resource]
getResourcesAt env pos = do
  cell <- getCellAt env pos
  return $ getResources cell

-- | Returns true if the postion in the enbironment contains resources
-- otherwise returns false.
hasResources :: Environment a -> Pos -> Bool
hasResources env pos = case getResourcesAt env pos of
  Nothing -> False
  Just r -> not $ null r

-- | Also returns the resources at the given position but returns an error when the position is not valid in the environemnt.
unsafeGetResources :: Environment a -> Pos -> [Resource]
unsafeGetResources env pos = case getResourcesAt env pos of
  Nothing -> error "The given position is not valid in the environment"
  Just r -> r

-- | Returns a random resource at the given position.
unsafeGetRandomResource :: Environment a -> QCGen -> Pos -> IO Resource
unsafeGetRandomResource env gen pos = QC.generate $ useSeed gen $ QC.elements $ unsafeGetResources env pos

-- | Inserts the given resources the given position in the environmnet.
insertResourcesAt :: Organism a => Environment a -> [Resource] -> Pos -> Environment a
insertResourcesAt env res p
  | legalPos p lim = Env (unsafeSet (fst cell, res) p grd) hood lim (low env)
  | otherwise = error "given position is not inbounds"
  where
    cell = fromJust $ getCellAt env p
    grd = grid env
    lim = limits env
    hood = neighbourhood env

-- | Insert the given resource to one of the neighbouring cells.
insertResourceAtNeighbour :: Organism a => QCGen -> Environment a -> Resource -> Pos -> IO (Environment a)
insertResourceAtNeighbour gen env res p = do
  position <- QC.generate $ useSeed gen $ QC.elements $ getNeighbours env p
  print position
  return $ addResource env position res

-- | Deletes the resource at the given index
deleteResourceIndex :: Organism a => Environment a -> Pos -> Int -> Environment a
deleteResourceIndex env pos index =
  case res of
    Nothing -> env
    Just r -> insertResourcesAt env (deleteIndex r index) pos
  where
    res = getResourcesAt env pos

-- | Adds the given resource to the resources at the given position.
addResource :: Organism a => Environment a -> Pos -> Resource -> Environment a
addResource env pos d =
  case res of
    Nothing -> env
    Just r -> insertResourcesAt env (d : r) pos
  where
    res = getResourcesAt env pos

-- | Delete the given resource from the list of resources on the given position.
deleteSubResources :: Organism a => Environment a -> Pos -> Resource -> Environment a
deleteSubResources env pos d =
  case res of
    Nothing -> env
    Just r -> insertResourcesAt env (List.delete d r) pos
  where
    res = getResourcesAt env pos

-- | Fills in the given list of tuples with positions and resources into the environmnet.
fillInResources :: Organism a => Environment a -> [(Pos, [Resource])] -> Environment a
fillInResources env [] = env
fillInResources env ((pos, res) : rst) = insertResourcesAt env' res pos
  where
    env' = fillInResources env rst

-- | Adds the given resource to one of the neighbours of the given position.
addResourceToNeighbours :: Organism a => QCGen -> Environment a -> Pos -> Resource -> IO (Environment a)
addResourceToNeighbours gen env pos d = do
  neighbour <- QC.generate $ useSeed gen $ QC.elements $ getNeighbours env pos
  let res = getResourcesAt env neighbour
  case res of
    Nothing -> return $ insertResourcesAt env [d] neighbour
    Just r -> return $ insertResourcesAt env (d : r) neighbour

-- | Returns True if the given cell contains an organism.
isOrg :: Organism a => Cell a -> Bool
isOrg (Org _, _) = True
isOrg _ = False

-- | Returns the organism at the given position in the environment if it contains one, otherwise return Noting.
getOrgAt :: Organism a => Environment a -> Pos -> Maybe a
getOrgAt env pos = do
  cell <- getCellAt env pos
  getOrg cell

-- | Returns true if the position in the environment contains an organims
-- returns false otherwise.
hasOrg :: Organism a => Environment a -> Pos -> Bool
hasOrg env pos = case getOrgAt env pos of
  Nothing -> False
  Just _ -> True

-- | Returns the organism at the given position but returns and error when there is no organism at that position.
unsafeGetOrgAt :: Organism a => Environment a -> Pos -> a
unsafeGetOrgAt env pos = case getOrgAt env pos of
  Nothing -> error "There was no org at the given position."
  Just o -> o

-- | Returns a list of tuples with positions with is corresponding organism for the given list of positions
-- in the given environment.
getOrgsAt :: Organism a => Environment a -> [Pos] -> [(Pos, a)]
getOrgsAt _ [] = []
getOrgsAt env (x : xs) = case getCellAt env x of
  Just c ->
    if isOrg c
      then (x, fromJust (getOrg c)) : getOrgsAt env xs
      else getOrgsAt env xs
  Nothing -> getOrgsAt env xs

-- | Returns True if there is no Organism at the given position in environment.
isNil :: Organism a => Environment a -> Pos -> Bool
isNil env p = case getCellAt env p of
  Nothing -> False
  Just (life, _) -> case life of
    Nil -> True
    _ -> False

-- | Returns a cell with without organism and resources for the given position.
-- This is used to initiate an empty environment.
nilGenerator :: Organism a => Pos -> Cell a
nilGenerator _ = (Nil, [])

-- | Deletes the organisms at the fiven list of positions.
nillify :: Organism a => Environment a -> [Pos] -> Environment a
nillify = foldl deleteOrganismAt

-- | Returns the neighbours of the given position in the environment that do not contaim an organism.
getNilNeighbours :: Organism a => Environment a -> Pos -> [Pos]
getNilNeighbours env p = filter (isNil env) (getNeighbours env p)

getCrossoverNeighbours :: Organism a => Environment a -> Pos -> Pos -> [Pos]
getCrossoverNeighbours env lp rp =
  List.nub $
    filter
      (\p -> p /= lp && p /= rp)
      (getNeighbours env lp ++ getNeighbours env rp)

getNilCrossoverNeighbours :: Organism a => Environment a -> Pos -> Pos -> [Pos]
getNilCrossoverNeighbours env lp rp =
  List.nub $
    filter (isNil env) $ getCrossoverNeighbours env lp rp

-- | Insterts a given organism at a given position in the environment.
insertOrganismAt :: Organism a => Environment a -> a -> Pos -> Environment a
insertOrganismAt env org p
  | legalPos p lim = Env (unsafeSet (Org org, res) p grd) hood lim (low env)
  | otherwise = error "given position is not inbounds"
  where
    res = fromJust $ getResourcesAt env p
    grd = grid env
    lim = limits env
    hood = neighbourhood env

-- | Returns a list of all the organims in the environment.
getAllOrgs :: Organism a => Environment a -> [a]
getAllOrgs (Env m _ _ _) = map (fromJust . getOrg) $ filter isOrg $ toList m

-- | Returns a list of tuples with all all the organisms in the environment with its accompanying position.
getOrgsPos :: Organism a => Environment a -> [(Pos, a)]
getOrgsPos env = getOrgsAt env $ getAllPos $ getLim env

-- | Returns a list of all the organisms with their position in random order.
getPermutedOrgsPos :: Organism a => QCGen -> Environment a -> IO [(Pos, a)]
getPermutedOrgsPos gen env = QC.generate $ useSeed gen $ QC.shuffle $ getOrgsPos env

-- | Deletes organism at the given position.
deleteOrganismAt :: Organism a => Environment a -> Pos -> Environment a
deleteOrganismAt env p
  | legalPos p lim = Env (unsafeSet (Nil, resources) p grd) hood lim (low env)
  | otherwise = error "given position is not inbounds"
  where
    resources = fromJust $ getResourcesAt env p
    grd = grid env
    lim = limits env
    hood = neighbourhood env

-- | Takes a list of tuples with positions and organisms
-- and insterts the organisms at their accompanying positions.
fillInOrgs :: Organism a => Environment a -> [(Pos, a)] -> Environment a
fillInOrgs env [] = env
fillInOrgs env ((pos, org) : rst) = insertOrganismAt env' org pos
  where
    env' = fillInOrgs env rst

-- | Returns an empty environment, with Nill and no resources at every environment,
-- with the given limits and neighbourhood.
empty :: Organism a => Lim -> Neighbourhood -> Environment a
empty (mx, my) n
  | legalLimits (mx, my) = Env (matrix mx my nilGenerator) n (mx, my) 1
  | otherwise = error "the given limits must be positive and nonzero"

-- | Distributes the given list of items, could be organisms or resources,
-- among positions within the given limits.
distribute :: QCGen -> [a] -> Lim -> IO [(Pos, a)]
distribute gen orgs lim = distribute' gen orgs $ getAllPos lim

-- | Distributes the given list of items along the given list of items.
distribute' :: QCGen -> [a] -> [Pos] -> IO [(Pos, a)]
distribute' _ [] _ = return []
distribute' gen (o : os) list = do
  let (g1, g2) = R.split gen
  p <- QC.generate $ useSeed g1 $ QC.elements list
  let l = List.delete p list
  rest <- distribute' g2 os l
  return $ (p, o) : rest

-- | Initializes a new environment with the given limits, neighbourhood and organisms.
-- Random resources are generated and added to the environment at random positions.
initializeEnvironment ::
  Organism a =>
  Neighbourhood ->
  QCGen ->
  [a] ->
  Lim ->
  IO (Environment a)
initializeEnvironment n gen orgList lim = do
  let (g1, g2) = R.split gen
  posOrgs <- distribute gen orgList lim
  let posList = map fst posOrgs
  let env = fillInOrgs (empty lim n) posOrgs
  let amount = floor (fromIntegral (getSize env) :: Double)
  let resources = generateResources g1 amount 1
  let (g21, g22) = R.split g2
  let extraRes = generateResourcesFixed g21 (length posList) 10
  posRes <- distribute g22 resources lim
  let newEnv = fillInResources env posRes
  let extraResEnv = fillInResources newEnv $ zip posList extraRes
  return extraResEnv

-- generateResourcesPos :: QCGen -> [Pos] -> Int -> [(Pos, [Resource])]
-- generateResourcesPos gen [] _ []
-- generateResourcesPos gen (p:ps) n =

-- | Generate a random position from a list of positions.
selectPosition :: QCGen -> [Pos] -> IO Pos
selectPosition gen positions = QC.generate $ useSeed gen $ QC.elements positions

-- | Generate a list of unique random positions of a given length within the limits of the environment.
generateRandomPositions :: Organism a => QCGen -> Environment a -> Int -> [Pos]
generateRandomPositions gen env n = take n $ List.nub $ genRanPos gen env

-- | Generates an infinite list of random positions of the environment.
genRanPos :: Organism a => QCGen -> Environment a -> [Pos]
genRanPos gen env = (x, y) : genRanPos g2 env
  where
    mx = getXLim env
    my = getYLim env
    (x, g1) = R.randomR (1, mx) gen
    (y, g2) = R.randomR (1, my) g1

bottom :: Double
bottom = - 999

top :: Double
top = 999

-- | Generates a list of a given length of lists of randomly generated resources
-- with a random length no greater than a given maximum size.
generateResources :: QCGen -> Int -> Int -> [[Resource]]
generateResources _ 0 _ = []
generateResources gen amount m = take randomSize infinteList : generateResources g3 (amount - 1) m
  where
    (g1, g2) = R.split gen
    (randomSize, g3) = R.randomR (1, m) g1
    infinteList = R.randomRs (bottom, top) g2

-- | Generates a list of a given length of lists of randomly generated resources
-- with a fixed length.
generateResourcesFixed :: QCGen -> Int -> Int -> [[Resource]]
generateResourcesFixed _ 0 _ = []
generateResourcesFixed gen amount m = take m infinteList : generateResources g1 (amount - 1) m
  where
    (g1, g2) = R.split gen
    infinteList = R.randomRs (bottom, top) g2
