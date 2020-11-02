module Environment where

import qualified Data.Map as M

type Resource = Double

type Pos = (Int, Int)

data Place a = Nil | Org a | Res Resource

type Env a = M.Map Int (M.Map Int (Place a))

getAtPos :: Pos -> Env a -> Maybe (Place a)
getAtPos (x, y) e1 = do
  e2 <- M.lookup x e1
  M.lookup y e2

insertAtPos :: Place a -> Pos -> Env a -> Maybe (Env a)
insertAtPos p (x, y) e1 = do
  e2 <- M.lookup x e1
  let e2' = M.insert y p e2
  return $ M.insert x e2' e1

