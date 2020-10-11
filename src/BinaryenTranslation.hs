module BinaryenTranslation where

import           Binaryen.Op

class OperationTranslation a where
    translateOp :: a -> Op
