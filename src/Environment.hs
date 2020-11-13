{-# LANGUAGE GADTs #-}

module Environment where

import qualified Data.List as List
import Data.Matrix
import Data.Maybe
import Organism
import Seeding
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Random

type Resource = Double

type Pos = (Int, Int)

type Lim = Pos

data Place a = Organism a => Nil | Org a | Res Resource

instance Show a => Show (Place a) where
  show Nil = "Nil"
  show (Org org) = show org
  show (Res r) = "Res: " ++ show r

data Environment a = Organism a => Env (Matrix (Place a)) Neighbourhood Lim

instance Show a => Show (Environment a) where
  show (Env m _ _) = show m

-- https://en.wikipedia.org/wiki/Moore_neighborhood
-- https://en.wikipedia.org/wiki/Von_Neumann_neighborhood
data Neighbourhood = Moore | VonNeumann
  deriving (Show)

legalPos :: Pos -> Lim -> Bool
legalPos (x, y) (mx, my) = x >= 1 && x <= mx && y >= 1 && y <= my

legalLimits :: Lim -> Bool
legalLimits (x, y) = x >= 0 && y >= 0

getAllPos :: Lim -> [Pos]
getAllPos (mx, my) = [(x, y) | x <- [1 .. mx], y <- [1 .. my]]

getOrg :: Organism a => Place a -> Maybe a
getOrg (Org o) = Just o
getOrg _ = Nothing

isOrg :: Organism a => Place a -> Bool
isOrg (Org _) = True
isOrg _ = False

isNil :: Organism a => Place a -> Bool
isNil Nil = True
isNil _ = False

getOrgsAt :: Organism a => Environment a -> [Pos] -> a

getOrgsAt env

getplaceAt :: Organism a => Environment a -> Pos -> Maybe (Place a)
getplaceAt (Env m _ maxPos) p
  | legalPos p maxPos = Just $ uncurry unsafeGet p m
  | otherwise = Nothing

nilGenerator :: Organism a => Pos -> Place a
nilGenerator _ = Nil

insertOrganismAt :: Organism a => Environment a -> a -> Pos -> Environment a
insertOrganismAt (Env m n lim) org p
  | legalPos p lim = Env (unsafeSet (Org org) p m) n lim
  | otherwise = error "given position is not inbounds"

deleteOrganismAt :: Organism a => Environment a -> Pos -> Environment a
deleteOrganismAt (Env m n lim) p
  | legalPos p lim = Env (unsafeSet Nil p m) n lim
  | otherwise = error "given position is not inbounds"

adjacentPos :: Pos -> [Pos]
adjacentPos (x, y) = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

diagonalPos :: Pos -> [Pos]
diagonalPos (x, y) = [(x -1, y -1), (x + 1, y + 1), (x + 1, y -1), (x -1, y + 1)]

getNeighbours :: Organism a => Environment a -> Pos -> [Pos]
getNeighbours (Env _ Moore lim) p =
  [neighbour | neighbour <- adjacentPos p ++ diagonalPos p, legalPos neighbour lim]
getNeighbours (Env _ VonNeumann lim) p =
  [neighbour | neighbour <- adjacentPos p, legalPos neighbour lim]

getAllOrgs :: Organism a => Environment a -> [a]
getAllOrgs (Env m _ _) = map (fromJust . getOrg) $ filter isOrg $ toList m

empty :: Organism a => Lim -> Neighbourhood -> Environment a
empty (mx, my) n
  | legalLimits (mx, my) = Env (matrix mx my nilGenerator) n (mx, my)
  | otherwise = error "the given limits must be positive and nonzero"

distributeOrgs :: Organism a => QCGen -> [a] -> Lim -> IO [(Pos, a)]
distributeOrgs gen orgs lim = distributeOrgs' gen orgs $ getAllPos lim

distributeOrgs' :: Organism a => QCGen -> [a] -> [Pos] -> IO [(Pos, a)]
distributeOrgs' _ [] _ = return []
distributeOrgs' gen (o : os) list = do
  let (g1, g2) = split gen
  p <- generate $ useSeed g1 $ elements list
  let l = List.delete p list
  rest <- distributeOrgs' g2 os l
  return $ (p, o) : rest

fillInOrgs :: Organism a => Environment a -> [(Pos, a)] -> Environment a
fillInOrgs env [] = env
fillInOrgs env ((pos, org) : rst) = insertOrganismAt env' org pos
  where
    env' = fillInOrgs env rst

initializeEnvironment :: Organism a => Neighbourhood -> QCGen -> [a] -> Lim -> IO (Environment a)
initializeEnvironment n gen orgList lim = do
  posOrgs <- distributeOrgs gen orgList lim
  return $ fillInOrgs (empty lim n) posOrgs

generateRandomPositions :: QCGen -> Lim -> Int -> [Pos]
generateRandomPositions gen l n = genRanPos gen l n n

genRanPos :: QCGen -> Lim -> Int -> Int -> [Pos]
genRanPos gen (mx, my) n m = (x, y) : genRanPos g2 (mx, my) n (m -1)
  where
    (x, g1) = randomR (0, mx) gen
    (y, g2) = randomR (0, my) g1
