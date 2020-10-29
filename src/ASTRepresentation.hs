module ASTRepresentation where

import           AST

type Depth = Int

type Size = Int

data Representation = Rep String Size Depth

instance Show Representation where
  show (Rep n s d) =
    "AST: "
      ++ "first node= "
      ++ show n
      ++ ", size= "
      ++ show s
      ++ ", depth= "
      ++ show d

generateRepresentation :: ASTExpression -> Representation
generateRepresentation expr =
  Rep (smallShow expr) (size expr) (getMaxDepth expr)
