{-# LANGUAGE GADTs #-}

module Running where

import Environment
import Organism
import Test.QuickCheck.Random

-- | Data type for the different actions that an organism can or has to do.
data Action = ResourceAquirement | Execution | SystemCall
  deriving (Show)

data Storage = Res Resource | Empty
  deriving (Show)

-- | Representation of an organims that is currently part of the running queue
data Runnable a = Organism a =>
  Runnable
  { organism :: a,
    position :: Pos,
    action :: Action,
    resource :: Storage
  }

instance Show (Runnable a) where
  show (Runnable org pos act stor) = genotype org ++ " " ++ show pos ++ " " ++ show act ++ " " ++ show stor ++ "\n"

-- | Represents that state that is kept during the running of a simulation
data RunState a = Organism a => RunState Int [Runnable a] QCGen

-- | Sets the action of the runnable to the given action.
setAction :: Runnable a -> Action -> Runnable a
setAction (Runnable org pos _ stor) act = Runnable org pos act stor

-- | Sets the position of the runnable to the given position.
setPosition :: Runnable a -> Pos -> Runnable a
setPosition (Runnable org _ act stor) pos = Runnable org pos act stor

-- | Adds a resource to the runnable.
-- If there was already a resource in the runnable that resource is overwritten.
addResource :: Runnable a -> Resource -> Runnable a
addResource (Runnable org pos act _) res = Runnable org pos act $ Res res

-- | Empties the storage of the runnable.
emptyStorage :: Runnable a -> Runnable a
emptyStorage (Runnable org pos act _) = Runnable org pos act Empty

-- | Returns the next action based on a boolean.
nextAction :: Action -> Action
nextAction ResourceAquirement = Execution
nextAction Execution = SystemCall
nextAction SystemCall = ResourceAquirement

-- | Advances the action of the runnable to the next action.
next :: Runnable a -> Runnable a
next runnable = setAction runnable $ nextAction (action runnable)
