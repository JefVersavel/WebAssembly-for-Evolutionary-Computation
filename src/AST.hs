module AST where

import           BinaryenTranslation
import           Binaryen.Op


data ASTExpression =
    Const Double |
    -- The integer represents the index of the parameter, will need to be generated based on the length of the list of parameters
    Param Int |
    BinOp BinaryOperation ASTExpression ASTExpression |
    UnOp UnaryOperation ASTExpression |
    RelOp RelationalOperation ASTExpression ASTExpression

data BinaryOperation = Add | Sub | Mul | Div | Min | Max | Copysign
    deriving (Enum, Eq, Bounded)

data UnaryOperation = Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest
    deriving (Enum, Eq, Bounded)

data RelationalOperation = Eq | Ne | Lt | Gt | Le | Ge
    deriving (Enum, Eq, Bounded)

getConstVal :: ASTExpression -> Maybe Double
getConstVal (Const i) = Just i
getConstVal _         = Nothing

getIndex :: ASTExpression -> Maybe Int
getIndex (Param i) = Just i
getIndex _         = Nothing

getFirstASTExpression :: ASTExpression -> Maybe ASTExpression
getFirstASTExpression (BinOp _ e _) = Just e
getFirstASTExpression (UnOp _ e   ) = Just e
getFirstASTExpression (RelOp _ e _) = Just e
getFirstASTExpression _             = Nothing

getSecondASTExpression :: ASTExpression -> Maybe ASTExpression
getSecondASTExpression (BinOp _ _ e) = Just e
getSecondASTExpression (RelOp _ _ e) = Just e
getSecondASTExpression _             = Nothing

instance Show BinaryOperation where
  show Add      = " + "
  show Sub      = " - "
  show Mul      = " * "
  show Div      = " / "
  show Min      = " min "
  show Max      = " max "
  show Copysign = " copysign "

instance OperationTranslation BinaryOperation where
  translateOp Add      = addFloat64
  translateOp Sub      = subFloat64
  translateOp Mul      = mulFloat64
  translateOp Div      = divFloat64
  translateOp Min      = minFloat64
  translateOp Max      = maxFloat64
  translateOp Copysign = copySignFloat64

instance Show UnaryOperation where
  show Abs     = " abs "
  show Neg     = " neg "
  show Sqrt    = " sqrt "
  show Ceil    = " ceil "
  show Floor   = " floor "
  show Trunc   = " trunc "
  show Nearest = " nearest "


instance OperationTranslation UnaryOperation where
  translateOp Abs     = absFloat64
  translateOp Neg     = negFloat64
  translateOp Sqrt    = sqrtFloat64
  translateOp Ceil    = ceilFloat64
  translateOp Floor   = floorFloat64
  translateOp Trunc   = truncFloat64
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

instance Show ASTExpression where
  show e = show' e 0

instance Eq ASTExpression where
  (Const d1) == (Const d2) = d1 == d2
  (Param i1) == (Param i2) = i1 == i2
  (BinOp b1 e11 e12) == (BinOp b2 e21 e22) =
    b1 == b2 && e11 == e21 && e12 == e22
  (RelOp r1 e11 e12) == (RelOp r2 e21 e22) =
    r1 == r2 && e11 == e21 && e12 == e22
  (UnOp u1 e1) == (UnOp u2 e2) = u1 == u2 && e1 == e2
  _            == _            = False


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


getMaxDepth :: ASTExpression -> Int
getMaxDepth (Const _      ) = 0
getMaxDepth (Param _      ) = 0
getMaxDepth (UnOp _ e     ) = 1 + getMaxDepth e
getMaxDepth (BinOp _ e1 e2) = 1 + max (getMaxDepth e1) (getMaxDepth e2)
getMaxDepth (RelOp _ e1 e2) = 1 + max (getMaxDepth e1) (getMaxDepth e2)

getNrNodes :: ASTExpression -> Int
getNrNodes (Const _      ) = 1
getNrNodes (Param _      ) = 1
getNrNodes (UnOp _ e     ) = 1 + getNrNodes e
getNrNodes (BinOp _ e1 e2) = 1 + getNrNodes e1 + getNrNodes e2
getNrNodes (RelOp _ e1 e2) = 1 + getNrNodes e1 + getNrNodes e2

size :: ASTExpression -> Int
size = getNrNodes
