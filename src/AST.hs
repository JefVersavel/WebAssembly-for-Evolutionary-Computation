{-# LANGUAGE DeriveGeneric #-}

module AST where

import Binaryen.Op
import BinaryenTranslation
import Data.Aeson
import Data.Serialize
import Data.TreeDiff
import GHC.Generics

--- | Internal representation of an expression.
-- Expressions are modelled as an abstract syntax tree.
-- These expressions can be converted to wasm modules.
data ASTExpression
  = -- | Leaf of an AST that is just a constant value.
    Const Double
  | -- | Leaf of an AST that is a parameter that is configured later.
    -- The integer represents the index the array of possible parameters.
    Param Int
  | -- | Non-leaf node in an AST that represents a binary operation and thus has two sub-expressions.
    BinOp BinaryOperation ASTExpression ASTExpression
  | -- | Non-leaf node in an AST that represents a unary operation and thus has one sub-expression.
    UnOp UnaryOperation ASTExpression
  | -- | Non-leaf node in an AST that represents a relational operator and thus has two sub-expressions.
    RelOp RelationalOperation ASTExpression ASTExpression
  deriving (Generic)

instance ToExpr ASTExpression

-- | ASTExpression is an instance of Serialize so that it can ve serialized to analyse later.
instance Serialize ASTExpression

instance ToJSON ASTExpression

-- | Internal representation of a binary operation.
data BinaryOperation = Add | Sub | Mul | Div | Min | Max | Copysign
  deriving (Enum, Eq, Bounded, Generic)

instance Serialize BinaryOperation

instance ToJSON BinaryOperation

instance ToExpr BinaryOperation

-- | Internal representation of a unary operation.
data UnaryOperation = Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest
  deriving (Enum, Eq, Bounded, Generic)

instance Serialize UnaryOperation

instance ToJSON UnaryOperation

instance ToExpr UnaryOperation

-- | Internal representation of a relational operation.
data RelationalOperation = Eq | Ne | Lt | Gt | Le | Ge
  deriving (Enum, Eq, Bounded, Generic)

instance Serialize RelationalOperation

instance ToJSON RelationalOperation

instance ToExpr RelationalOperation

-- | Returns a string of the root of the AST, usefull for when only a short representation of an AST is needed.
smallShow :: ASTExpression -> String
smallShow (Const c) = "Const " ++ show c
smallShow (Param d) = "Param" ++ show d
smallShow (BinOp b _ _) = show b
smallShow (UnOp u _) = show u
smallShow (RelOp r _ _) = show r

instance Show BinaryOperation where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "
  show Min = " min "
  show Max = " max "
  show Copysign = " copysign "

instance OperationTranslation BinaryOperation where
  translateOp Add = addFloat64
  translateOp Sub = subFloat64
  translateOp Mul = mulFloat64
  translateOp Div = divFloat64
  translateOp Min = minFloat64
  translateOp Max = maxFloat64
  translateOp Copysign = copySignFloat64

instance Show UnaryOperation where
  show Abs = " abs "
  show Neg = " neg "
  show Sqrt = " sqrt "
  show Ceil = " ceil "
  show Floor = " floor "
  show Trunc = " trunc "
  show Nearest = " nearest "

instance OperationTranslation UnaryOperation where
  translateOp Abs = absFloat64
  translateOp Neg = negFloat64
  translateOp Sqrt = sqrtFloat64
  translateOp Ceil = ceilFloat64
  translateOp Floor = floorFloat64
  translateOp Trunc = truncFloat64
  translateOp Nearest = nearestFloat64

instance Show RelationalOperation where
  show Eq = " == "
  show Ne = " =/= "
  show Lt = " < "
  show Gt = " > "
  show Le = " <= "
  show Ge = " >= "

instance OperationTranslation RelationalOperation where
  translateOp Eq = eqFloat64
  translateOp Ne = neFloat64
  translateOp Lt = ltFloat64
  translateOp Gt = gtFloat64
  translateOp Le = leFloat64
  translateOp Ge = geFloat64

instance Eq ASTExpression where
  (Const d1) == (Const d2) = d1 == d2
  (Param i1) == (Param i2) = i1 == i2
  (BinOp b1 e11 e12) == (BinOp b2 e21 e22) =
    b1 == b2 && e11 == e21 && e12 == e22
  (RelOp r1 e11 e12) == (RelOp r2 e21 e22) =
    r1 == r2 && e11 == e21 && e12 == e22
  (UnOp u1 e1) == (UnOp u2 e2) = u1 == u2 && e1 == e2
  _ == _ = False

instance Show ASTExpression where
  show e = show' e 0

-- | Helper function for the Show instance of ASTExpression.
-- Uses an extra argument to better use indentation to make the AST more readable.
show' :: ASTExpression -> Int -> String
show' _ d | d < 0 = error "The depth cannot be less than 0"
show' (Const f) _ = " " ++ show f
show' (Param i) _ = " param[" ++ show i ++ "]"
show' (BinOp b e1 e2) d =
  "\n"
    ++ concat (replicate d "\t")
    ++ "("
    ++ show b
    ++ show' e1 (d + 1)
    ++ show' e2 (d + 1)
    ++ ")"
show' (UnOp u e1) d =
  "\n" ++ concat (replicate d "\t") ++ "(" ++ show u ++ show' e1 (d + 1) ++ ")"
show' (RelOp r e1 e2) d =
  "\n"
    ++ concat (replicate d "\t")
    ++ "("
    ++ show r
    ++ show' e1 (d + 1)
    ++ show' e2 (d + 1)
    ++ ")"

-- | Returns the maximum depth of an AST.
getMaxDepth :: ASTExpression -> Int
getMaxDepth (Const _) = 0
getMaxDepth (Param _) = 0
getMaxDepth (UnOp _ e) = 1 + getMaxDepth e
getMaxDepth (BinOp _ e1 e2) = 1 + max (getMaxDepth e1) (getMaxDepth e2)
getMaxDepth (RelOp _ e1 e2) = 1 + max (getMaxDepth e1) (getMaxDepth e2)

-- | Returns the total number of nodes in a nAST.
getNrNodes :: ASTExpression -> Int
getNrNodes (Const _) = 2
getNrNodes (Param _) = 2
getNrNodes (UnOp _ e) = 1 + getNrNodes e
getNrNodes (BinOp _ e1 e2) = 1 + getNrNodes e1 + getNrNodes e2
getNrNodes (RelOp _ e1 e2) = 1 + getNrNodes e1 + getNrNodes e2

-- | Returns the total number of nodes in a nAST.
getNrNodes' :: ASTExpression -> Int
getNrNodes' (Const _) = 2
getNrNodes' (Param _) = 2
getNrNodes' (UnOp _ e) = 1 + getNrNodes e
getNrNodes' (BinOp _ e1 e2) = 2 + getNrNodes e1 + getNrNodes e2
getNrNodes' (RelOp _ e1 e2) = 2 + getNrNodes e1 + getNrNodes e2

-- | Returns the size of an AST. Has the same implementation as getNrNodes.
size :: ASTExpression -> Int
size = getNrNodes

-- | Returns the average size of all the ASTExpressions in a given list.
averageSize :: [ASTExpression] -> Double
averageSize exprs = fromIntegral (sum (map size exprs)) / fromIntegral (length exprs)
