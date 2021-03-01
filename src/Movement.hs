module Movement where

import Data.Maybe
import Environment
import Organism

-- | data type that consists of the different moves that are possible for the organisms
data Move = U | D | L | R

-- | Moves the position of up in the environment
up :: Environment a -> Pos -> Pos
up env (x, y)
  | x > low env = (x - 1, y)
  | otherwise = (x, y)

-- | Moves the position down in the environment
down :: Environment a -> Pos -> Pos
down env (x, y)
  | x < getX (limits env) = (x + 1, y)
  | otherwise = (x, y)

-- | Moves the position left in the enviroment
left :: Environment a -> Pos -> Pos
left env (x, y)
  | y > low env = (x, y - 1)
  | otherwise = (x, y)

-- | Moves the position right in the environment
right :: Environment a -> Pos -> Pos
right env (x, y)
  | y < getY (limits env) = (x, y + 1)
  | otherwise = (x, y)

-- | Moves the position in the given direction
movePos :: Environment a -> Pos -> Move -> Pos
movePos env pos U = up env pos
movePos env pos D = down env pos
movePos env pos L = left env pos
movePos env pos R = right env pos

-- | If there is an organism at the given position in the environment, then that organism is moved based on the
-- given direction.
-- when there is an organims in the new position than the environment remains unchanged.
moveOrg :: Organism a => Environment a -> Pos -> Move -> Either (Pos, Environment a) (a, Pos)
moveOrg env pos move
  | hasOrg env newPos = Right (fromJust $ getOrgAt env newPos, newPos)
  | otherwise = case org of
    Just o ->
      if legalPos newPos $ limits env
        then Left (newPos, deleteOrganismAt (insertOrganismAt env o newPos) pos)
        else Left (pos, env)
    Nothing -> Left (pos, env)
  where
    newPos = movePos env pos move
    org = getOrgAt env pos
