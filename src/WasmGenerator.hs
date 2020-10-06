{-# LANGUAGE OverloadedStrings #-}

module WasmGenerator where

import Generators ( genASTExpressions )
import Binaryen.Module (addFunction, addFunctionExport, create, Module)
import Binaryen.Expression (binary, unary, Expression, constFloat64)
import AST ( ASTExpression(..) )
import BinaryenTranslation ( OperationTranslation(translateOp) )
import Foreign
import Binaryen.Type (none, float64)
import Binaryen.Index (Index(Index))
import Binaryen.Function (Function, getName)
import Binaryen.Op
import BinaryenUtils
import Data.ByteString as BS (writeFile, ByteString)
import System.Directory
import Pool

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
    be <- binary m (translateOp op) ge1 ge2
    unary m convertUInt32ToFloat64 be

generateExpressions :: [Module] -> [Double] -> IO [Expression]
generateExpressions mods params = do
    exprs <- genASTExpressions 10 5 (length params) 0.5 4
    sequence [generateExpression m params e | (e, m) <- zip exprs mods]

generateFunction :: Module -> [Double] -> ASTExpression -> IO Function
generateFunction m params e = do
    pool <- newPool
    namePtr <- pooledNewByteString0 pool "main"
    typePtr <- pooledNew pool none
    expr <- generateExpression m params e
    addFunction m namePtr none float64 typePtr (Index 0) expr

createModule :: ASTExpression -> [Double] -> IO Module
createModule e params = do 
    m <- create 
    function <- generateFunction m params e
    functionName <- getName function
    _ <- addFunctionExport m functionName functionName
    return m

createWasmFiles :: [ASTExpression] -> [Double] -> IO [(ASTExpression, String)]
createWasmFiles exprs params = do
    let dir = "./src/wasm/"
    mods <- sequence [createModule e params | e <- exprs]
    let fileNames = [dir ++ "p" ++ show i ++ ".wasm" | i <- [0 .. (length mods -1)] ]
    serializedMods <- sequence [serializeModule m | m <- mods]
    oldFiles <- listDirectory dir
    let oldFileNames = [dir ++ f | f <- oldFiles]
    putStrLn "removing files:"
    mapM_ putStrLn ["\t" ++ n | n <- oldFileNames]
    removeAllFiles oldFileNames
    writeFiles fileNames serializedMods
    putStrLn "Creating files:"
    mapM_ putStrLn ["\t" ++ n | n <- fileNames]
    return $ zip exprs fileNames

serializeExpressions :: [ASTExpression] -> [Double] -> IO [(ASTExpression, BS.ByteString)]
serializeExpressions exprs params = do
    mods <- sequence [createModule e params | e <- exprs]
    serializedMods <- sequence [serializeModule m | m <- mods]
    return $ zip exprs serializedMods

writeFiles :: [String] -> [BS.ByteString] -> IO ()
writeFiles [] [] = return ()
writeFiles (n:ns) (b:bs) = BS.writeFile n b >> writeFiles ns bs
writeFiles _ _ = error "the length of the filenames don't match the length of the serialized modules"

removeAllFiles :: [FilePath] -> IO ()
removeAllFiles [] = return ()
removeAllFiles (f:fs) = removeFile f >> removeAllFiles fs

test = do 
    let params = [0.913487512345, 234.345, 34.12,973456.2,-78764.2]
    exprs <- genASTExpressions 10 5 (length params) 0.5 6
    serializeExpressions exprs params
