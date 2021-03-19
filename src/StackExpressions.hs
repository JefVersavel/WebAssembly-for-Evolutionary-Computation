module StackExpressions where

import AST
import Generators
import Options.Applicative.Help.Levenshtein

data StackInstruction
  = B BinaryOperation
  | U UnaryOperation
  | R RelationalOperation
  | C Double
  | P Int
  | GSet
  | GGet
  | GTee
  | If
  | Then
  | Else
  deriving (Eq)

instance Show StackInstruction where
  show (B b) = show b
  show (U u) = show u
  show (R r) = show r
  show (C d) = " const " ++ show d
  show (P p) = " param " ++ show p
  show GSet = " global.set"
  show GGet = " global.get"
  show GTee = " global.tee"
  show If = " if "
  show Then = "then"
  show Else = "else"

class ToStack a where
  toStack :: a -> InstructionSequence

newtype InstructionSequence = Seq [StackInstruction]

(+++) :: InstructionSequence -> InstructionSequence -> InstructionSequence
(Seq l) +++ (Seq r) = Seq $ l ++ r

instance ToStack ASTExpression where
  toStack (Const c) = Seq [C c]
  toStack (Param p) = Seq [P p]
  toStack (BinOp b l r) = toStack r +++ toStack l +++ Seq [B b]
  toStack (UnOp u e) = toStack e +++ Seq [U u]
  toStack (RelOp b r l) = toStack r +++ toStack l +++ Seq [R b]
  toStack (GlobalTee l) = toStack l +++ Seq [GTee]
  toStack (GlobalSet l r) = toStack l +++ Seq [GSet] +++ toStack r
  toStack GlobalGet = Seq [GGet]
  toStack (IfStatement c l r) = toStack c +++ Seq [If, Then] +++ toStack l +++ Seq [Else] +++ toStack r

instance Show InstructionSequence where
  show (Seq []) = ""
  show (Seq (x : xs)) = show x ++ "\n" ++ show (Seq xs)

-- | https://www.academia.edu/26551805/Lazy_dynamic_programming_can_be_eager
getEditDistance :: ASTExpression -> ASTExpression -> Int
getEditDistance l r = editDistance first second
  where
    (Seq first) = toStack l
    (Seq second) = toStack r

stacktest = do
  expr <- randomGenerationTest
  let first = expr !! 7
  print first
  let (Seq fStack) = toStack first
  putStr $ show fStack
  let second = expr !! 5
  print second
  let (Seq sStack) = toStack second
  putStr $ show sStack
  print $ editDistance fStack sStack
