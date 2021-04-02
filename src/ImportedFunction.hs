module ImportedFunction where

data ImportedFunction = ImportedFunction String Int

type ImportedFunctions = [ImportedFunction]

findFunction :: ImportedFunctions -> String -> Either String ImportedFunction
findFunction [] _ = Left "could not find the function with given name"
findFunction (imp@(ImportedFunction s _) : rest) name
  | s == name = Right imp
  | otherwise = findFunction rest name
