module AST where

data Expression =
  Const Double |
  -- The integer represents the index of the parameter, will need to be generated based on the length of the list of parameters
  Param Int |
  BinOp BinaryOperation Expression Expression |
  UnOp UnaryOperation Expression |
  RelOp RelationalOperation Expression Expression

data BinaryOperation = Add | Sub | Mul | Div | Min | Max | Copysign
  deriving (Enum)

data UnaryOperation = Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest
  deriving (Enum)

data RelationalOperation = Eq | Ne | Lt | Gt | Le | Ge
  deriving (Enum)

getConstVal :: Expression -> Maybe Double
getConstVal (Const i) = Just i
getConstVal _ = Nothing


getIndex :: Expression -> Maybe Int
getIndex (Param i) = Just i
getIndex _ = Nothing

getFirstExpression :: Expression -> Maybe Expression
getFirstExpression (BinOp _ e _) = Just e
getFirstExpression (UnOp _ e) = Just e
getFirstExpression (RelOp _ e _) = Just e
getFirstExpression _ = Nothing

getSecondExpression :: Expression -> Maybe Expression
getSecondExpression (BinOp _ _ e) = Just e
getSecondExpression (RelOp _ _ e) = Just e
getSecondExpression _ = Nothing

instance Show BinaryOperation where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "
  show Min = " min "
  show Max = " max "
  show Copysign = " copysign "

instance Show UnaryOperation where
  show Abs = " abs "
  show Neg = " - "
  show Sqrt = " sqrt "
  show Ceil = " ceil "
  show Floor = " floor "
  show Trunc = " trunc "
  show Nearest = " nearest "

instance Show RelationalOperation where
  show Eq = " == "
  show Ne = " =/= "
  show Lt = " < "
  show Gt = " > "
  show Le = " <= "
  show Ge = " >= "

instance Show Expression where
  show (Const f) = show f ++ " "
  show (Param i) = "param[" ++ show i ++ "] "
  show (BinOp b e1 e2) = "(" ++ show b ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (UnOp u e1) = "(" ++ show u ++ " " ++ show e1 ++ ")"
  show (RelOp r e1 e2) = "(" ++ show r ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"

getMaxDepth :: Expression -> Int
getMaxDepth (Const _) = 0
getMaxDepth (Param _) = 0
getMaxDepth (UnOp _ e) = 1 + (getMaxDepth e)
getMaxDepth (BinOp _ e1 e2) = 1 + (max (getMaxDepth e1) (getMaxDepth e2))
getMaxDepth (RelOp _ e1 e2) = 1 + (max (getMaxDepth e1) (getMaxDepth e2))
