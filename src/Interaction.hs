module Interaction where

import Organism

data Interaction = Mate | Ignore

compatibility :: Double
compatibility = 0.5

interact :: Organism a => a -> a -> Interaction
interact l r
  | compatible l r = Mate
  | otherwise = Ignore
