module AST where

data Expression =
  Const Double |
  -- The integer represents the index of the parameter, will need to be generated based on the length of the list of parameters
  Param Int |
  BinOp BinaryOperation Expression Expression |
  UnOp UnaryOperation Expression |
  RelOp RelationalOperation Expression Expression

data BinaryOperation = Add | Sub | Mul | Div | Min | Max | Copysign
  deriving (Enum, Eq, Bounded)

data UnaryOperation = Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest
  deriving (Enum, Eq, Bounded)

data RelationalOperation = Eq | Ne | Lt | Gt | Le | Ge
  deriving (Enum, Eq, Bounded)

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
  show Neg = " neg "
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
  show e = show' e 0

instance Eq Expression where
  (Const d1) == (Const d2) = d1 == d2
  (Param i1) == (Param i2) = i1 == i2
  (BinOp b1 e11 e12) == (BinOp b2 e21 e22) = b1 == b2 && e11 == e21 && e12 == e22
  (RelOp r1 e11 e12) == (RelOp r2 e21 e22) = r1 == r2 && e11 == e21 && e12 == e22
  (UnOp u1 e1) == (UnOp u2 e2) = u1 == u2 && e1 == e2
  _ == _ = False


show' :: Expression -> Int -> String
show' _ d 
  | d < 0 = error "The depth cannot be less than 0"
show' (Const f) _ = show f ++ " "
show' (Param i) _ = "param[" ++ show i ++ "] "
show' (BinOp b e1 e2) d = "\n" ++ (concat $ replicate d "\t") ++ "(" ++ show b ++ show' e1 (d+1) ++ show' e2 (d+1) ++ ")"
show' (UnOp u e1) d = "\n" ++ (concat $ replicate d "\t") ++ "(" ++ show u ++ show' e1 (d+1) ++ ")"
show' (RelOp r e1 e2) d = "\n" ++ (concat $ replicate d "\t") ++ "(" ++ show r ++ show' e1 (d+1) ++ show' e2 (d+1) ++ ")"


getMaxDepth :: Expression -> Int
getMaxDepth (Const _) = 0
getMaxDepth (Param _) = 0
getMaxDepth (UnOp _ e) = 1 + (getMaxDepth e)
getMaxDepth (BinOp _ e1 e2) = 1 + (max (getMaxDepth e1) (getMaxDepth e2))
getMaxDepth (RelOp _ e1 e2) = 1 + (max (getMaxDepth e1) (getMaxDepth e2))