{-# LANGUAGE OverloadedStrings #-}

module WasmGenerator where

import AST
import Binaryen.Expression (Expression, binary, constFloat64, globalGet, unary)
import Binaryen.Function (Function, getName)
import Binaryen.Index (Index (Index))
import Binaryen.Module
  ( Module,
    addFunction,
    addFunctionExport,
    addGlobal,
    addGlobalImport,
    create,
  )
import Binaryen.Op
import Binaryen.Type (float64, none)
import BinaryenTranslation
import BinaryenUtils
import Data.ByteString as BS (ByteString, writeFile)
import Data.Serialize
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Generators
import Pool
import System.Directory

-- | Translates and ASTExpression to a binaryen expression and adds it to the given module.
-- The given list of doubles is used to initialize the parameters in the ASTExpression.
generateExpression :: Module -> [CString] -> ASTExpression -> IO Expression
generateExpression m _ (Const d) = constFloat64 m d
generateExpression m names (Param i) = globalGet m (names !! i) float64
generateExpression m names (BinOp op e1 e2) = do
  ge1 <- generateExpression m names e1
  ge2 <- generateExpression m names e2
  binary m (translateOp op) ge1 ge2
generateExpression m names (UnOp op e) = do
  ge <- generateExpression m names e
  unary m (translateOp op) ge
generateExpression m names (RelOp op e1 e2) = do
  ge1 <- generateExpression m names e1
  ge2 <- generateExpression m names e2
  be <- binary m (translateOp op) ge1 ge2
  unary m convertUInt32ToFloat64 be

-- | Translates a list of ASTExpressions to binaryen expressions and adds them to the corresponding module of al ist of modules.
generateExpressions :: [Module] -> [CString] -> IO [Expression]
generateExpressions mods names = do
  exprs <- genASTExpressions 10 5 (length names) 0.5 4
  sequence [generateExpression m names e | (e, m) <- zip exprs mods]

-- | Translates an ASTExpression to a binaryen function and adds that function the given module.
generateFunction :: Module -> [CString] -> ASTExpression -> IO Function
generateFunction m params e = do
  pool <- newPool
  namePtr <- pooledNewByteString0 pool "main"
  typePtr <- pooledNew pool none
  expr <- generateExpression m params e
  addFunction m namePtr none float64 typePtr (Index 0) expr

-- | Generates a binaryen module from an ASTExpression by generating a binaryen function wiht that ASTExpression
-- and adding that function to the default binaryen module,
createModule :: ASTExpression -> [Double] -> IO Module
createModule e params = do
  (first, second, third) <- generateGlobalNames $ length params
  m <- create
  function <- generateFunction m [third] e
  functionName <- getName function
  _ <- addFunctionExport m functionName functionName
  print m
  addGlobalImport m first second third float64 0
  print m
  return m

-- | Generates the names for the global variables.
generateGlobalNames :: Int -> IO (CString, CString, CString)
generateGlobalNames n = do
  pool <- newPool
  first <- pooledNewByteString0 pool "first"
  second <- pooledNewByteString0 pool "second"
  third <- pooledNewByteString0 pool "third"
  return (first, second, third)

-- | Generates wasm files from the given list of ASTExpression by making modules of them and printing the bytestrings to files.
createWasmFiles :: [ASTExpression] -> [Double] -> IO [(ASTExpression, String)]
createWasmFiles exprs params = do
  let dir = "./src/wasm/"
  mods <- sequence [createModule e params | e <- exprs]
  let fileNames =
        [dir ++ "p" ++ show i ++ ".wasm" | i <- [0 .. (length mods - 1)]]
  print fileNames
  serializedMods <- sequence [serializeModule m | m <- mods]
  print serializedMods
  oldFiles <- listDirectory dir
  let oldFileNames = [dir ++ f | f <- oldFiles]
  putStrLn "removing files:"
  mapM_ putStrLn ["\t" ++ n | n <- oldFileNames]
  removeAllFiles oldFileNames
  writeFiles fileNames serializedMods
  putStrLn "Creating files:"
  mapM_ putStrLn ["\t" ++ n | n <- fileNames]
  return $ zip exprs fileNames

-- | Generates tuples of the givne ASTExpressions and its bytestring.
-- The bytestring is generated by making a module of an ASTExpression and serializing it to wasm.
serializeExpressions ::
  [ASTExpression] -> [Double] -> IO [(ASTExpression, BS.ByteString)]
serializeExpressions exprs params = do
  mods <- sequence [createModule e params | e <- exprs]
  serializedMods <- sequence [serializeModule m | m <- mods]
  return $ zip exprs serializedMods

-- | Returns the bytestring by generating a module of the given ASTExpression and serializing it to wasm.
serializeExpression :: ASTExpression -> [Double] -> IO ByteString
serializeExpression expr params = do
  m <- createModule expr params
  serializeModule m

-- | Writes a list of bytestrings to files at the given list of filepaths.
writeFiles :: [FilePath] -> [BS.ByteString] -> IO ()
writeFiles [] [] = return ()
writeFiles (n : ns) (b : bs) = BS.writeFile n b >> writeFiles ns bs
writeFiles _ _ =
  error
    "the length of the filenames don't match the length of the serialized modules"

-- | Removes all files located at the given list of filepaths.
removeAllFiles :: [FilePath] -> IO ()
removeAllFiles = foldr ((>>) . removeFile) (return ())

test :: IO [(ASTExpression, String)]
test = do
  let params = [0.913487512345]
  exprs <- genASTExpressions 10 1 (length params) 0.5 6
  createWasmFiles exprs params
