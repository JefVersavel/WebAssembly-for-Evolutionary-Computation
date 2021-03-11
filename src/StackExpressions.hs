module StackExpressions where

import AST
import Generators

data StackInstruction = B BinaryOperation | U UnaryOperation | R RelationalOperation | C Double | P Int

instance Show StackInstruction where
  show (B b) = show b
  show (U u) = show u
  show (R r) = show r
  show (C d) = "Const " ++ show d
  show (P p) = "Get $" ++ show p

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

instance Show InstructionSequence where
  show (Seq []) = ""
  show (Seq (x : xs)) = show x ++ "\n" ++ show (Seq xs)

stacktest = do
  expr <- randomGenerationTest
  print $ head expr
  let first = head expr
  putStr $ show $ toStack first
