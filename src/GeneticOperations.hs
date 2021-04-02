module GeneticOperations where

import AST
import Control.Monad.Reader
import Generators
import ImportedFunction
import Seeding
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Random

-- | Returns the sub-expression of a given expression at the given place.
getSubExpression :: Int -> ASTExpression -> ASTExpression
getSubExpression 0 e = e
getSubExpression _ (Const c) = Const c
getSubExpression _ (Param p) = Param p
getSubExpression i (UnOp _ e) = getSubExpression (i - 1) e
getSubExpression i (BinOp _ e1 e2)
  | i <= nrNodes1 = getSubExpression dec e1
  | otherwise = getSubExpression (dec - nrNodes1) e2
  where
    nrNodes1 = getNrNodes e1
    dec = i - 1
getSubExpression i (RelOp _ e1 e2)
  | i <= nrNodes1 = getSubExpression dec e1
  | otherwise = getSubExpression (dec - nrNodes1) e2
  where
    nrNodes1 = getNrNodes e1
    dec = i - 1
getSubExpression i (GlobalTee e) = getSubExpression (i - 1) e
getSubExpression _ GlobalGet = GlobalGet
getSubExpression i (GlobalSet e1 e2)
  | i <= nrNodes1 = getSubExpression dec e1
  | otherwise = getSubExpression (dec - nrNodes1) e2
  where
    nrNodes1 = getNrNodes e1
    dec = i - 1
getSubExpression i (IfStatement c l r)
  | i <= nrNodesC = getSubExpression dec c
  | i <= nrNodesL + nrNodesC = getSubExpression (dec - nrNodesC) l
  | otherwise = getSubExpression (dec - nrNodesC - nrNodesL) r
  where
    nrNodesC = getNrNodes c
    nrNodesL = getNrNodes l
    dec = i - 1
getSubExpression i (ImportedFunctionCall _ list) = getSubExpression newi e
  where
    nrList = getNrNodes <$> list
    (index, newi) = findIndex 0 i nrList
    e = list !! index

findIndex :: Int -> Int -> [Int] -> (Int, Int)
findIndex index n [] = (index, n) -- returns the last index but this is potentially risky so be careful
findIndex index n (x : xs)
  | n <= x = (index, n)
  | otherwise = findIndex (index + 1) (n - x) xs

-- | Inserts an expression into another expression at the givin place.
insertSubExpression :: Int -> ASTExpression -> ASTExpression -> ASTExpression
insertSubExpression 0 _ e2 = e2
insertSubExpression _ (Const _) e = e
insertSubExpression _ (Param _) e = e
insertSubExpression i (UnOp o e1) e2 =
  UnOp o $ insertSubExpression (i - 1) e1 e2
insertSubExpression i (BinOp o e1 e2) e3
  | i <= nrNodes1 = BinOp o (insertSubExpression dec e1 e3) e2
  | otherwise = BinOp o e1 (insertSubExpression (dec - nrNodes1) e2 e3)
  where
    nrNodes1 = getNrNodes e1
    dec = i - 1
insertSubExpression i (RelOp o e1 e2) e3
  | i <= nrNodes1 = RelOp o (insertSubExpression dec e1 e3) e2
  | otherwise = RelOp o e1 (insertSubExpression (dec - nrNodes1) e2 e3)
  where
    nrNodes1 = getNrNodes e1
    dec = i - 1
insertSubExpression i (GlobalTee e1) e2 =
  GlobalTee $ insertSubExpression (i - 1) e1 e2
insertSubExpression _ GlobalGet e = e
insertSubExpression i (GlobalSet e1 e2) e3
  | i <= nrNodes1 = GlobalSet (insertSubExpression dec e1 e3) e2
  | otherwise = GlobalSet e1 (insertSubExpression (dec - nrNodes1) e2 e3)
  where
    nrNodes1 = getNrNodes e1
    dec = i - 1
insertSubExpression i (IfStatement c l r) e
  | i <= nrNodesC = IfStatement (insertSubExpression dec c e) l r
  | i <= nrNodesL + nrNodesC = IfStatement c (insertSubExpression (dec - nrNodesC) l e) r
  | otherwise = IfStatement c l (insertSubExpression (dec - nrNodesC - nrNodesL) r e)
  where
    nrNodesC = getNrNodes c
    nrNodesL = getNrNodes l
    dec = i - 1
insertSubExpression i (ImportedFunctionCall name list) e =
  ImportedFunctionCall name $
    first
      ++ [insertSubExpression newi (head second) e]
      ++ tail second
  where
    nrList = getNrNodes <$> list
    (index, newi) = findIndex 0 i nrList
    (first, second) = splitAt index list

-- | Returns a tuple of a randomly generated place and the next generator.
selectGenOpPoint :: RandomGen g => g -> ASTExpression -> (Int, g)
selectGenOpPoint g e = randomR (0, getNrNodes e) g

-- | Performs crossover on he two given expressions using the given generator. The result is the two resulting expressions and the nex generator.
crossover ::
  RandomGen g =>
  g ->
  ASTExpression ->
  ASTExpression ->
  (ASTExpression, ASTExpression, g)
crossover g1 e1 e2 = (child1, child2, g3)
  where
    (p1, g2) = selectGenOpPoint g1 e1
    (p2, g3) = selectGenOpPoint g2 e2
    sub1 = getSubExpression p1 e1
    sub2 = getSubExpression p2 e2
    child1 = insertSubExpression p1 e1 sub2
    child2 = insertSubExpression p2 e2 sub1

-- | Generates a random binary oeration using an inition generator.
getRandomBinOp :: QCGen -> IO BinaryOperation
getRandomBinOp g = generate $ useSeed g rBinOp

-- | Generates a random unary operation using an initial generator.
getRandomUnOp :: QCGen -> IO UnaryOperation
getRandomUnOp g = generate $ useSeed g rUnOp

-- | Generates a random leaf in an AST using an initial generator.
getRandomLeaf :: QCGen -> Int -> IO ASTExpression
getRandomLeaf g i = generate $ useSeed g $ oneof [rConst, rParam i]

-- | Replaces a a node at the given place in an expression.
replaceNode ::
  UnaryOperation ->
  BinaryOperation ->
  ASTExpression ->
  Int ->
  ASTExpression ->
  ASTExpression
replaceNode _ _ leaf _ (Const _) = leaf
replaceNode _ _ leaf _ (Param _) = leaf
replaceNode unOp _ _ 0 (UnOp _ e) = UnOp unOp e
replaceNode _ binOp _ 0 (BinOp _ e1 e2) = BinOp binOp e1 e2
replaceNode unOp binOp leaf i (UnOp o e) =
  UnOp o $ replaceNode unOp binOp leaf (i - 1) e
replaceNode unOp binOp leaf i (BinOp o e1 e2)
  | i <= nrNodes1 = BinOp o (replaceNode unOp binOp leaf dec e1) e2
  | otherwise = BinOp o e1 (replaceNode unOp binOp leaf (dec - nrNodes1) e2)
  where
    nrNodes1 = getNrNodes e1
    dec = i - 1
replaceNode unOp binOp leaf i (RelOp o e1 e2)
  | i <= nrNodes1 = RelOp o (replaceNode unOp binOp leaf dec e1) e2
  | otherwise = RelOp o e1 (replaceNode unOp binOp leaf (dec - nrNodes1) e2)
  where
    nrNodes1 = getNrNodes e1
    dec = i - 1
replaceNode _ _ _ _ node = node

-- | Performs point-mutation which is achieved by randomly replacing one node in the expression.
pointMutation :: QCGen -> ASTExpression -> Int -> IO ASTExpression
pointMutation g1 e i = do
  unOp <- u
  binOp <- b
  leaf <- l
  return $ replaceNode unOp binOp leaf p e
  where
    (p, g2) = selectGenOpPoint g1 e
    u = getRandomUnOp g2
    b = getRandomBinOp g2
    l = getRandomLeaf g2 i

-- | Randomly generates a sub-expression with a givne maximum depth.
generateSubTree :: QCGen -> Int -> Int -> ImportedFunctions -> IO ASTExpression
generateSubTree g1 maxD nrParam functions
  | grow = generateExpr growOneInit
  | otherwise = generateExpr fullOneInit
  where
    (grow, g2) = randomR (True, False) g1
    (d, g3) = randomR (0, maxD) g2
    generateExpr method = generate $ runReader (method g3) (d, nrParam, functions)

-- | Performs sub-tree mutation which is achieved by randomly replacing a sub-tree of an expression by another randomly generated sub-tree.
subTreeMutation :: QCGen -> ASTExpression -> Int -> ImportedFunctions -> IO ASTExpression
subTreeMutation g1 e nrParam functions = do
  let (p, g2) = selectGenOpPoint g1 e
  let maxD = getMaxDepth $ getSubExpression p e
  subTree <- generateSubTree g2 maxD nrParam functions
  return $ insertSubExpression p e subTree

-- | Perfroms reproduction which just returns the given expression.
reproduction :: ASTExpression -> ASTExpression
reproduction = id

-- | Performs permutation which is achieved by permutating the sub-expressions at a randomlly chosen node.
permutation :: QCGen -> ASTExpression -> ASTExpression
permutation g1 e = insertSubExpression point e permuted
  where
    (point, g2) = selectGenOpPoint g1 e
    subTree = getSubExpression point e
    permuted = permute g2 subTree

-- | Permutes the sub-expressions of an expression when possible.
permute :: QCGen -> ASTExpression -> ASTExpression
permute _ (Const i) = Const i
permute _ (Param p) = Param p
permute _ (UnOp o e) = UnOp o e
permute g (BinOp o e1 e2) = BinOp o e11 e22
  where
    (e11, e22) = switch g (e1, e2)
permute g (RelOp o e1 e2) = RelOp o e11 e22
  where
    (e11, e22) = switch g (e1, e2)
permute _ (GlobalTee e) = GlobalTee e
permute _ GlobalGet = GlobalGet
permute g (GlobalSet e1 e2) = GlobalSet e11 e22
  where
    (e11, e22) = switch g (e1, e2)
permute g (IfStatement c l r) = IfStatement cc ll rr
  where
    (cc, ll, rr) = switchThree g (c, l, r)

-- | Randomly switches the elements in a tuple based on a random number generator that either returns 0 or 1.
switch :: RandomGen g => g -> (a, a) -> (a, a)
switch g (l, r)
  | i == 0 = (l, r)
  | otherwise = (r, l)
  where
    i = fst $ randomR (0, 1) g :: Int

switchThree :: RandomGen g => g -> (a, a, a) -> (a, a, a)
switchThree g (l, m, r)
  | i == 0 = (l, m, r)
  | i == 1 = (m, l, r)
  | i == 0 = (l, r, m)
  | i == 0 = (m, r, l)
  | i == 0 = (r, m, l)
  | otherwise = (r, l, m)
  where
    i = fst $ randomR (0, 5) g :: Int

testExpression :: ASTExpression
testExpression =
  BinOp Mul (UnOp Floor (Const 4)) (BinOp Add (Const 34) (UnOp Ceil (Param 4)))
