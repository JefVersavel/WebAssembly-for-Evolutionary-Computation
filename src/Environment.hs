{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Environment where

import Data.Aeson
import qualified Data.List as List
import Data.Matrix
import Data.Maybe
import GHC.Generics
import Organism
import Seeding
import System.Random
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Random

type Resource = Double

type Pos = (Int, Int)

type Lim = Pos

-- | Reprents a lifeform in the environment,
-- it can either be Nil which means that there is no life form in the cell or it can be an Organism.
data Life a = Nil | Org a
  deriving (Generic)

instance ToJSON a => ToJSON (Life a)

-- | Represents a cell in the environemnt with its lifeform and a list of resources that can be found in that cell.
type Cell a = (Life a, [Resource])

instance (Show a, Organism a) => Show (Life a) where
  show Nil = "Nil"
  show (Org org) = genotype org

-- | Represents the environment with cells and the neighbourhood and limits of the environment.
data Environment a = Env
  { grid :: Matrix (Cell a),
    neighbourhood :: Neighbourhood,
    limits :: Lim
  }
  deriving (Generic)

instance ToJSON a => ToJSON (Environment a)

instance ToJSON a => ToJSON (Matrix (Cell a))

instance (Show a, Organism a) => Show (Environment a) where
  show env = showCells $ toLists $ grid env

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
  deriving (Show, Generic)

instance ToJSON Neighbourhood

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
getNeighbours (Env _ Moore lim) p =
  [neighbour | neighbour <- adjacentPos p ++ diagonalPos p, legalPos neighbour lim]
getNeighbours (Env _ VonNeumann lim) p =
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
getResources :: Cell a -> Maybe [Resource]
getResources (_, r) = Just r
getResources _ = Nothing

-- | Returns True if the given cell has a non-zero amount of resources.
containsResources :: Cell a -> Bool
containsResources (_, r)
  | List.null r = False
  | otherwise = True
containsResources _ = False

-- | Returns the resources at a given position.
getResourcesAt :: Environment a -> Pos -> Maybe [Resource]
getResourcesAt env pos = do
  cell <- getCellAt env pos
  getResources cell

-- | Inserts the given resources the given position in the environmnet.
insertResourcesAt :: Organism a => Environment a -> [Resource] -> Pos -> Environment a
insertResourcesAt env res p
  | legalPos p lim = Env (unsafeSet (fst cell, res) p grd) hood lim
  | otherwise = error "given position is not inbounds"
  where
    cell = fromJust $ getCellAt env p
    grd = grid env
    lim = limits env
    hood = neighbourhood env

-- | Fills in the given list of tuples with positions and resources into the environmnet.
fillInResources :: Organism a => Environment a -> [(Pos, [Resource])] -> Environment a
fillInResources env [] = env
fillInResources env ((pos, res) : rst) = insertResourcesAt env' res pos
  where
    env' = fillInResources env rst

-- | Returns True if the given cell contains an organism.
isOrg :: Organism a => Cell a -> Bool
isOrg (Org _, _) = True
isOrg _ = False

-- | Returns the organism at the given position in the environment if it contains one, otherwise return Noting.
getOrgAt :: Organism a => Environment a -> Pos -> Maybe a
getOrgAt env pos = do
  cell <- getCellAt env pos
  getOrg cell

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

-- | Returns a list of organisms for the given list of positions
-- in the given environment.
getOrganisms :: Organism a => Environment a -> [Pos] -> [a]
getOrganisms _ [] = []
getOrganisms env (x : xs) = case getCellAt env x of
  Just c ->
    if isOrg c
      then fromJust (getOrg c) : getOrganisms env xs
      else getOrganisms env xs
  Nothing -> getOrganisms env xs

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

-- | Insterts a given organism at a given position in the environment.
insertOrganismAt :: Organism a => Environment a -> a -> Pos -> Environment a
insertOrganismAt env org p
  | legalPos p lim = Env (unsafeSet (Org org, resources) p grd) hood lim
  | otherwise = error "given position is not inbounds"
  where
    resources = fromJust $ getResourcesAt env p
    grd = grid env
    lim = limits env
    hood = neighbourhood env

-- | Returns a list of all the organims in the environment.
getAllOrgs :: Organism a => Environment a -> [a]
getAllOrgs (Env m _ _) = map (fromJust . getOrg) $ filter isOrg $ toList m

-- | Returns a list of tuples with all all the organisms in the environment with its accompanying position.
getOrgsPos :: Organism a => Environment a -> [(Pos, a)]
getOrgsPos env = getOrgsAt env $ getAllPos $ getLim env

-- | Deletes organism at the given position.
deleteOrganismAt :: Organism a => Environment a -> Pos -> Environment a
deleteOrganismAt env p
  | legalPos p lim = Env (unsafeSet (Nil, resources) p grd) hood lim
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
  | legalLimits (mx, my) = Env (matrix mx my nilGenerator) n (mx, my)
  | otherwise = error "the given limits must be positive and nonzero"

-- | Distributes the given list of items, could be organisms or resources,
-- among positions within the given limits.
distribute :: QCGen -> [a] -> Lim -> IO [(Pos, a)]
distribute gen orgs lim = distribute' gen orgs $ getAllPos lim

-- | Distributes the given list of items along the given list of items.
distribute' :: QCGen -> [a] -> [Pos] -> IO [(Pos, a)]
distribute' _ [] _ = return []
distribute' gen (o : os) list = do
  let (g1, g2) = split gen
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
  let (g1, g2) = split gen
  posOrgs <- distribute gen orgList lim
  let env = fillInOrgs (empty lim n) posOrgs
  let amount = floor $ (fromIntegral (getSize env) :: Double) * 0.1
  let resources = generateResources g1 amount 3
  posRes <- distribute g2 resources lim
  let newEnv = fillInResources env posRes
  return newEnv

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
    (x, g1) = randomR (1, mx) gen
    (y, g2) = randomR (1, my) g1

-- | Generates a list of a givne length of lists of randomly generated resources
-- with a random length no greater than a given maximum size.
generateResources :: QCGen -> Int -> Int -> [[Resource]]
generateResources _ 0 _ = []
generateResources gen amount m = take randomSize infinteList : generateResources g3 (amount - 1) m
  where
    (g1, g2) = split gen
    (randomSize, g3) = randomR (1, m) g1
    infinteList = randoms g2
