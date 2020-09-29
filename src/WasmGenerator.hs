module WasmGenerator where

import Generators ( genASTExpressions )
import Binaryen.Module ( Module, create )
import Binaryen.Expression
    ( Expression, binary, constFloat64, unary )
import AST ( ASTExpression(..) )
import BinaryenTranslation ( OperationTranslation(translateOp) )

generateExpression :: Module -> [Double] -> ASTExpression -> IO Expression
generateExpression m _ (Const d) = constFloat64 m d
generateExpression m params (Param i) = constFloat64 m (params !! i)
generateExpression m params (BinOp op e1 e2) = do 
    ge1 <- generateExpression m params e1
    ge2 <- generateExpression m params e2
    binary m (translateOp op) ge1 ge2
generateExpression m params (UnOp op e) = do
    ge <- generateExpression m params e
    unary m (translateOp op) ge
generateExpression m params (RelOp op e1 e2) = do
    ge1 <- generateExpression m params e1
    ge2 <- generateExpression m params e2
    binary m (translateOp op) ge1 ge2

generateExpressions :: [Module] -> [Double] -> IO [Expression]
generateExpressions mods params = do
    exprs <- genASTExpressions 10 5 (length params) 0.5 10
    sequence [generateExpression m params e | e <- exprs, m <- mods]


test = do 
    mods <- sequence [create, create, create, create]
    generateExpressions mods [0, 1, 2, 3]