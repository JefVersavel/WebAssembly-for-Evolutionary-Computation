import GHC.Generics (Generic)
import Test.QuickCheck

import Generic.Random

data Expression =
  Const Float |
  BinOp BinaryOperation Expression Expression |
  UnOp UnaryOperation Expression

data BinaryOperation = Add | Sub | Mul | Div | Min | Max | Copysign | Eq | Ne | Lt | Gt | Le | Ge

data UnaryOperation = Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest

instance Arbitrary Binaryoperation where
  arbitrary = oneof
    [Add , Sub , Mul]


instance Show BinaryOperation where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "
  show Min = " min "
  show Max = " max "
  show Copysign = " copysign "
  show Eq = " == "
  show Ne = " =/= "
  show Lt = " < "
  show Gt = " > "
  show Le = " <= "
  show Ge = " >= "

instance Show UnaryOperation where
  show Abs = " abs "
  show Neg = " - "
  show Sqrt = " sqrt "
  show Ceil = " ceil "
  show Floor = " floor "
  show Trunc = " trunc "
  show Nearest = " nearest "


instance Show Expression where
  show (Const f) = show f ++ " "
  show (BinOp b e1 e2) = "(" ++ show b ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (UnOp u e1) = "(" ++ show u ++ " " ++ show e1 ++ ")"

m = show (BinOp Add (UnOp Abs (Const (-2.4))) (Const 6.5))
