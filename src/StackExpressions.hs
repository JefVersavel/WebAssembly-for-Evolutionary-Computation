module StackExpressions where

import AST
import Generators

data StackInstruction
  = B BinaryOperation
  | U UnaryOperation
  | R RelationalOperation
  | C Double
  | Cnst
  | P Int
  | Prm
  | GSet
  | GGet
  deriving (Eq)

instance Show StackInstruction where
  show (B b) = show b
  show (U u) = show u
  show (R r) = show r
  show (C d) = " " ++ show d
  show (P p) = " " ++ show p
  show Cnst = " Const"
  show Prm = " Param"
  show GSet = " global.set"
  show GGet = " global.get"

class ToStack a where
  toStack :: a -> InstructionSequence

newtype InstructionSequence = Seq [StackInstruction]

(+++) :: InstructionSequence -> InstructionSequence -> InstructionSequence
(Seq l) +++ (Seq r) = Seq $ l ++ r

instance ToStack ASTExpression where
  toStack (Const c) = Seq [C c, Cnst]
  toStack (Param p) = Seq [P p, Prm]
  toStack (BinOp b l r) = toStack r +++ toStack l +++ Seq [B b]
  toStack (UnOp u e) = toStack e +++ Seq [U u]
  toStack (RelOp b r l) = toStack r +++ toStack l +++ Seq [R b]
  toStack (GlobalTee l) = toStack l +++ Seq [GSet, GGet]
  toStack (GlobalSet l r) = toStack l +++ Seq [GSet] +++ toStack r
  toStack GlobalGet = Seq [GGet]

instance Show InstructionSequence where
  show (Seq []) = ""
  show (Seq (x : xs)) = show x ++ "\n" ++ show (Seq xs)

stacktest = do
  expr <- randomGenerationTest
  print $ expr !! 7
  let first = expr !! 7
  putStr $ show $ toStack first

-- https://wiki.haskell.org/Edit_distance
dist :: Eq a => [a] -> [a] -> Int
dist a b =
  last
    ( if lab == 0
        then mainDiag
        else
          if lab > 0
            then lowers !! (lab - 1)
            else {- < 0 -} uppers !! (-1 - lab)
    )
  where
    mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
    uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
    lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
    eachDiag a [] diags = []
    eachDiag a (bch : bs) (lastDiag : diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
      where
        nextDiag = head (tail diags)
    oneDiag a b diagAbove diagBelow = thisdiag
      where
        doDiag [] b nw n w = []
        doDiag a [] nw n w = []
        doDiag (ach : as) (bch : bs) nw n w = me : doDiag as bs me (tail n) (tail w)
          where
            me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
        firstelt = 1 + head diagBelow
        thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
    lab = length a - length b
    min3 x y z = if x < y then x else min y z
