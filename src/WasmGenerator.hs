{-# LANGUAGE OverloadedStrings #-}

module WasmGenerator where

import AST
import Binaryen.Expression (Expression, binary, block, call, constFloat64, globalGet, globalSet, if_, unary)
import Binaryen.Function (Function, getName)
import Binaryen.Index (Index (Index))
import Binaryen.Module
  ( Module,
    addFunction,
    addFunctionExport,
    addFunctionImport,
    addGlobalExport,
    addGlobalImport,
    create,
  )
import Binaryen.Op
import Binaryen.Type (float64, int32, none)
import BinaryenTranslation
import BinaryenUtils
import Control.Monad.Reader
import qualified Data.ByteString as BS hiding (length, map)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Generators
import ImportedFunction
import Pool
import System.Directory

-- | Translates and ASTExpression to a binaryen expression and adds it to the given module.
-- The given list of doubles is used to initialize the parameters in the ASTExpression.
generateExpression :: Module -> ASTExpression -> ReaderT (ImportedFunctions, [CString]) IO Expression
generateExpression m (Const d) = lift $ constFloat64 m d
generateExpression m (Param i) = do
  (_, names) <- ask
  lift $ globalGet m (names !! i) float64
generateExpression m (BinOp op e1 e2) = do
  ge1 <- generateExpression m e1
  ge2 <- generateExpression m e2
  lift $ binary m (translateOp op) ge1 ge2
generateExpression m (UnOp op e) = do
  ge <- generateExpression m e
  lift $ unary m (translateOp op) ge
generateExpression m (RelOp op e1 e2) = do
  ge1 <- generateExpression m e1
  ge2 <- generateExpression m e2
  be <- lift $ binary m (translateOp op) ge1 ge2
  lift $ unary m convertUInt32ToFloat64 be
generateExpression m (GlobalTee e) = do
  ge <- generateExpression m e
  (internal, _, _) <- lift getInternalNames
  glblset <- lift $ globalSet m internal ge
  glblget <- lift $ globalGet m internal float64
  exprPtr <- lift $ newArray [glblset, glblget]
  lift $ block m nullPtr exprPtr 2 float64
generateExpression m GlobalGet = do
  (internal, _, _) <- lift getInternalNames
  lift $ globalGet m internal float64
generateExpression m (GlobalSet e1 e2) = do
  ge1 <- generateExpression m e1
  ge2 <- generateExpression m e2
  (internal, _, _) <- lift getInternalNames
  glblset <- lift $ globalSet m internal ge1
  exprPtr <- lift $ newArray [glblset, ge2]
  lift $ block m nullPtr exprPtr 2 float64
generateExpression m (IfStatement c l r) = do
  cond <- generateExpression m c
  lft <- generateExpression m l
  rght <- generateExpression m r
  (md, _, _) <- lift getModNames
  condExpression <- lift $ newArray [cond]
  conditional <- lift $ call m md condExpression 1 int32
  lift $ if_ m conditional lft rght
generateExpression m (ImportedFunctionCall name list) = do
  args <- sequence (generateExpression m <$> list)
  argsArray <- lift $ newArray args
  cName <- lift $ newCString name
  lift $ call m cName argsArray (Index $ fromIntegral $ length list) float64

-- | Translates a list of ASTExpressions to binaryen expressions and adds them to the corresponding module of al ist of modules.
generateExpressions :: [Module] -> ReaderT (ImportedFunctions, [CString]) IO [Expression]
generateExpressions mods = do
  (functions, names) <- ask
  exprs <- lift $ genASTExpressions 10 5 (length names) 0.5 4 functions
  sequence [generateExpression m e | (e, m) <- zip exprs mods]

-- | Translates an ASTExpression to a binaryen function and adds that function the given module.
generateFunction :: Module -> [CString] -> ASTExpression -> ImportedFunctions -> IO Function
generateFunction m params e functions = do
  pool <- newPool
  namePtr <- pooledNewByteString0 pool "main"
  typePtr <- pooledNew pool none
  expr <- runReaderT (generateExpression m e) (functions, params)
  addFunction m namePtr none float64 typePtr (Index 0) expr

-- | Generates a binaryen module from an ASTExpression by generating a binaryen function wiht that ASTExpression
-- and adding that function to the default binaryen module,
createModule :: ASTExpression -> Int -> ImportedFunctions -> IO Module
createModule e params functions
  | params > 5 = error "currently a maximum of 5 parameters is allowed"
  | otherwise = do
    print params
    parameters <- generateGlobalNames params
    print ("parameters" :: String)
    print parameters
    m <- create
    let firsts = [first | (first, _, _) <- parameters]
    function <- generateFunction m firsts e functions
    functionName <- getName function
    _ <- addFunctionExport m functionName functionName
    let zipped = zip ([0, 1 ..] :: [CInt]) parameters
    (fInternal, sInternal, tInternal) <- getInternalNames
    addGlobalImport m fInternal sInternal tInternal float64 5
    (fMod, sMod, tMod) <- getModNames
    addFunctionImport m fMod sMod tMod float64 int32
    functionNames <- sequence [getImportedNames name | (ImportedFunction name _) <- functions]
    sequence_ [addFunctionImport m f s t float64 float64 | (f, s, t) <- functionNames]
    _ <- addGlobalExport m fInternal tInternal
    mapM_ (addglobal m) zipped
    return m

addglobal :: Module -> (CInt, (CString, CString, CString)) -> IO ()
addglobal m (i, (f, s, t)) = addGlobalImport m f s t float64 i

-- | Generates the names for the global variables.
generateGlobalNames :: Int -> IO [(CString, CString, CString)]
generateGlobalNames n = sequence [generateGlobalName i | i <- [0 .. (n -1)]]

generateGlobalName :: Int -> IO (CString, CString, CString)
generateGlobalName n = do
  first <- newCString $ "param" ++ show n
  second <- newCString "external"
  third <- newCString $ "param" ++ show n
  return (first, second, third)

-- returns the names of the internal state
getInternalNames :: IO (CString, CString, CString)
getInternalNames = do
  first <- newCString "state"
  second <- newCString "internal"
  third <- newCString "state"
  return (first, second, third)

-- returns the names of the internal state
getModNames :: IO (CString, CString, CString)
getModNames = do
  first <- newCString "mod"
  second <- newCString "specialImports"
  third <- newCString "mod"
  return (first, second, third)

getImportedNames :: String -> IO (CString, CString, CString)
getImportedNames name = do
  first <- newCString name
  second <- newCString "ImportedFunctions"
  third <- newCString name
  return (first, second, third)

-- | Generates wasm files from the given list of ASTExpression by making modules of them and printing the bytestrings to files.
createWasmFiles :: [ASTExpression] -> Int -> ImportedFunctions -> IO [(ASTExpression, String)]
createWasmFiles exprs params functions = do
  let dir = "./src/wasm/"
  mods <- sequence [createModule e params functions | e <- exprs]
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
  [ASTExpression] -> Int -> ImportedFunctions -> IO [(ASTExpression, BS.ByteString)]
serializeExpressions exprs params functions = do
  mods <- sequence [createModule e params functions | e <- exprs]
  serializedMods <- sequence [serializeModule m | m <- mods]
  return $ zip exprs serializedMods

-- | Returns the bytestring by generating a module of the given ASTExpression and serializing it to wasm.
serializeExpression :: ASTExpression -> Int -> ImportedFunctions -> IO BS.ByteString
serializeExpression expr params functions = do
  m <- createModule expr params functions
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
  let params = 1
  let e = IfStatement (Const 2) (Const 1) (Const 2)
  print e
  createWasmFiles [e] params []
