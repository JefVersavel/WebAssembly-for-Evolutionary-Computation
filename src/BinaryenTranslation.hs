module BinaryenTranslation where

import Binaryen.Op

-- | Class that translates a datatype to a binaryen operation.
class OperationTranslation a where
  translateOp :: a -> Op
