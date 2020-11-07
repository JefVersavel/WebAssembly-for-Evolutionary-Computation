module Run where

import Data.Serialize

class Serialize a => Run a where
  getName :: a -> String


