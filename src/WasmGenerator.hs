module WasmGenerator where

import Generators ( genASTExpressions )
import Binaryen.Module
import Binaryen.Expression
import AST ( ASTExpression(..) )
import BinaryenTranslation ( OperationTranslation(translateOp) )
import Foreign
import Foreign.C
import Binaryen.Type (Type, none, float64)
import Binaryen.Index (Index(Index))
import Binaryen.Function (Function)


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
-- generateExpression m params (RelOp op e1 e2) = do
--     ge1 <- generateExpression m params e1
--     ge2 <- generateExpression m params e2
--     binary m (translateOp op) ge1 ge2

generateExpressions :: [Module] -> [Double] -> IO [Expression]
generateExpressions mods params = do
    exprs <- genASTExpressions 10 5 (length params) 0.5 4
    sequence [generateExpression m params e | (e, m) <- zip exprs mods]

-- generateFunction mod expr = do
--     name <- newCString "firstFunction"
--     addFunction name none float64 (Ptr none) (Index (w32# 1)) expr

generateFunction :: Module -> [Double] -> ASTExpression -> IO Function
generateFunction m params e = do
    pool <- newPool
    namePtr <- pooledNew pool (castCharToCChar 'f')
    typePtr <- pooledNew pool none
    expr <- generateExpression m params e
    addFunction m namePtr none float64 typePtr (Index 0) expr

test = do 
    mods <- sequence [create, create, create, create]
    let params = [0,1,2,3]
    exprs <- genASTExpressions 10 5 (length params) 0.5 4
    functions <- sequence [ generateFunction m params e | (e,m) <- zip exprs mods]
    setStart (mods !! 0 ) $ functions !! 0
    pool <- newPool
    ptr <- pooledNew pool (CChar 0)
    Binaryen.Module.write (mods !! 0) ptr (CSize 9999)
