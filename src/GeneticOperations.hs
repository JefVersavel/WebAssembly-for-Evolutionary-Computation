module GeneticOperations where

import AST
import System.Random (RandomGen, Random(randomR))
import Test.QuickCheck.Random (QCGen, mkQCGen)
import Generators
import Test.QuickCheck
import Seeding

getSubExpression :: Int -> ASTExpression -> ASTExpression
getSubExpression 0 e = e
getSubExpression _ (Const c) = Const c
getSubExpression _ (Param p) = Param p
getSubExpression i (UnOp _ e) = getSubExpression (i-1) e
getSubExpression i (BinOp _ e1 e2)
    | i <= nrNodes1 = getSubExpression dec e1
    | otherwise = getSubExpression (dec - nrNodes1) e2
    where
        nrNodes1 = getNrNodes e1
        dec = i - 1

insertSubExpression :: Int -> ASTExpression -> ASTExpression -> ASTExpression
insertSubExpression 0 _ e2 = e2
insertSubExpression _ (Const _) e = e
insertSubExpression _ (Param _) e = e
insertSubExpression i (UnOp o e1) e2 = UnOp o $ insertSubExpression (i-1) e1 e2
insertSubExpression i (BinOp o e1 e2) e3
    | i <= nrNodes1 = BinOp o (insertSubExpression dec e1 e3) e2
    | otherwise = BinOp o e1 (insertSubExpression (dec - nrNodes1) e2 e3)
    where
        nrNodes1 = getNrNodes e1
        dec = i - 1


selectGenOpPoint :: RandomGen g => g -> ASTExpression -> (Int, g)
selectGenOpPoint g e = randomR (0, getNrNodes e) g

crossover :: RandomGen g => g -> ASTExpression -> ASTExpression -> (ASTExpression,ASTExpression,g)
crossover g1 e1 e2 = (child1, child2, g3)
    where 
        (p1, g2) = selectGenOpPoint g1 e1
        (p2, g3) = selectGenOpPoint g2 e2
        sub1 = getSubExpression p1 e1
        sub2 = getSubExpression p2 e2
        child1 = insertSubExpression p1 e1 sub2
        child2 = insertSubExpression p2 e2 sub1

getRandomBinOp :: QCGen -> IO BinaryOperation
getRandomBinOp g = generate $ useSeed g $ rBinOp

getRandomUnOp :: QCGen -> IO UnaryOperation
getRandomUnOp g = generate $ useSeed g $ rUnOp

getRandomLeaf :: QCGen -> Int -> IO ASTExpression
getRandomLeaf g i = generate $ useSeed g $ oneof [rConst, rParam i]

replaceNode :: UnaryOperation -> BinaryOperation -> ASTExpression -> Int -> ASTExpression -> ASTExpression
replaceNode _ _ leaf _ (Const _) = leaf
replaceNode _ _ leaf _ (Param _) = leaf
replaceNode unOp _ _ 0 (UnOp _ e) = UnOp unOp e
replaceNode _ binOp _ 0 (BinOp _ e1 e2) = BinOp binOp e1 e2
replaceNode unOp binOp leaf i (UnOp o e) = UnOp o $ replaceNode unOp binOp leaf (i-1) e
replaceNode unOp binOp leaf i (BinOp o e1 e2)
    | i <= nrNodes1 = BinOp o (replaceNode unOp binOp leaf dec e1) e2
    | otherwise = BinOp o e1 (replaceNode unOp binOp leaf (dec - nrNodes1) e2)
    where
        nrNodes1 = getNrNodes e1
        dec = i - 1

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



testGen i = pointMutation (mkQCGen i) (BinOp Mul (UnOp Floor (Const 4) )(Param 5)) 10