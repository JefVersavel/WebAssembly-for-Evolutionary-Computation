module Ancestor where

import AST
import Seeding
import SysCall
import Test.QuickCheck
import Test.QuickCheck.Random

seeder :: Int -> Int -> Int -> IO SysCall
seeder seed sze dpth =
  generate $
    useSeed gen $
      frequency
        [(sze, return Reproduction), (sze - dpth, elements [Up, Down, Rght, Lft]), (dpth, return None)]
  where
    gen = mkQCGen seed

type Seeding = (Int, Int, Int)

showVerticalList :: Show a => [a] -> IO ()
showVerticalList [] = putStrLn ""
showVerticalList (x : xs) = do
  print x
  showVerticalList xs

redirectIO :: (IO a, b) -> IO (a, b)
redirectIO (l, r) = do
  lft <- l
  return (lft, r)

checkForReproduction :: [Int] -> IO [Seeding]
checkForReproduction seeds = do
  let sizes = [15 .. 20]
  let depths = [5 .. 15]
  syscallsSeedings <-
    sequence
      [ redirectIO (seeder seed sze dpth, (seed, sze, dpth))
        | sze <- sizes,
          dpth <- depths,
          sze > 2 * dpth,
          seed <- seeds
      ]
  return $ map snd $ filter (isReproduction . fst) syscallsSeedings

checkForDoingNothing :: [Int] -> IO [Seeding]
checkForDoingNothing seeds = do
  let sizes = [1 .. 10]
  let depths = [1 .. 20]
  syscallsSeedings <-
    sequence
      [ redirectIO (seeder seed sze dpth, (seed, sze, dpth))
        | sze <- sizes,
          dpth <- depths,
          sze > 2 * dpth,
          seed <- seeds
      ]
  return $ map snd $ filter (isDoingNothing . fst) syscallsSeedings

isReproduction :: SysCall -> Bool
isReproduction Reproduction = True
isReproduction _ = False

isDoingNothing :: SysCall -> Bool
isDoingNothing None = True
isDoingNothing _ = False

lookForSeeds :: IO ()
lookForSeeds = do
  let seeds = [0 .. 10]
  reproductions <- checkForDoingNothing seeds
  showVerticalList reproductions

getSizesDepths :: IO ()
getSizesDepths = do
  print "ancestor1"
  print $ size ancestor1
  print $ getMaxDepth ancestor1
  print "ancestor2"
  print $ size ancestor2
  print $ getMaxDepth ancestor2
  print "ancestor3"
  print $ size ancestor3
  print $ getMaxDepth ancestor3
  print "ancestor4"
  print $ size ancestor4
  print $ getMaxDepth ancestor4
  print "ancestor6"
  print $ size ancestor6
  print $ getMaxDepth ancestor6

start1 :: Double
start1 = -1

ancestor1 :: ASTExpression
ancestor1 =
  BinOp
    Mul
    ( GlobalSet
        ( BinOp
            Add
            GlobalGet
            (Const 1)
        )
        ( UnOp
            Ceil
            (Const 2.5)
        )
    )
    ( UnOp
        Nearest
        ( IfStatement
            (Param 0)
            (UnOp Neg (BinOp Max (Param 0) (Const 4)))
            (Const 1)
        )
    )

start2 :: Double
start2 = 0

ancestor2 :: ASTExpression
ancestor2 =
  BinOp
    Sub
    ( GlobalSet
        ( BinOp
            Add
            GlobalGet
            (Const 1)
        )
        ( BinOp
            Add
            (Const 2.5)
            (Param 0)
        )
    )
    ( UnOp
        Floor
        ( IfStatement
            (Param 1)
            (Const 10)
            (UnOp Neg (BinOp Mul (Param 0) (Const 1.6)))
        )
    )

start3 :: Double
start3 = 1

ancestor3 :: ASTExpression
ancestor3 =
  BinOp
    Max
    ( GlobalSet
        ( BinOp
            Mul
            GlobalGet
            (Const 3)
        )
        ( UnOp
            Ceil
            (BinOp Copysign (Const 5) (Param 0))
        )
    )
    ( UnOp
        Nearest
        ( IfStatement
            (Param 0)
            (UnOp Neg (BinOp Max (Param 0) (Const 4)))
            (Const 1)
        )
    )

start4 :: Double
start4 = 1

ancestor4 :: ASTExpression
ancestor4 =
  BinOp
    Add
    ( GlobalSet
        ( BinOp
            Add
            GlobalGet
            (Const 2)
        )
        ( BinOp
            Add
            (Const 2.5)
            (Param 0)
        )
    )
    ( UnOp
        Floor
        ( IfStatement
            (BinOp Max (Const 45) (Param 1))
            (Const 10)
            (UnOp Neg (BinOp Mul (Param 0) (Const 1.6)))
        )
    )

start6 :: Double
start6 = 5

ancestor6 :: ASTExpression
ancestor6 =
  BinOp
    Add
    ( GlobalSet
        ( BinOp
            Add
            GlobalGet
            (Const 1)
        )
        ( BinOp
            Min
            (Const 2.5)
            (Param 1)
        )
    )
    ( IfStatement
        (Param 0)
        (UnOp Neg (BinOp Max (Param 0) (Const 4)))
        ( BinOp
            Max
            (Const 1)
            ( UnOp
                Ceil
                (BinOp Sub (Param 1) (Param 0))
            )
        )
    )

generator1 :: ASTExpression
generator1 =
  GlobalSet
    (Const 4)
    (Const 1)

generatorStart1 :: Double
generatorStart1 = 2

generator2 :: ASTExpression
generator2 =
  GlobalSet
    (BinOp Add GlobalGet (Const 1))
    (UnOp Ceil (Const 2))

generatorStart2 :: Double
generatorStart2 = 0
