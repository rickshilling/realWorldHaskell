{-# LANGUAGE NamedFieldPuns #-}

module Lib
    (
    ) where

import Data.List
import Data.Graph
import Data.Function
import Data.Char
import Control.Arrow

-- http://book.realworldhaskell.org/read/getting-started.html

-- 1.1
e1_1 = 5 + 8

-- 1.2
-- let x = 1
-- :show bindings

-- 1.3
e1_3 = interact wordCount
    where wordCount input = show (length (words input)) ++ "\n"

-- 1.4
e1_4 = interact wordCount
    where wordCount input = show (length ( input)) ++ "\n"

-- http://book.realworldhaskell.org/read/types-and-functions.html
-- 2.1
{-Traversing through the list by calling itself until it reaches empty list-}

--2.2
lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (z:y:ys) = lastButOne (y:ys)
-- Jamin: consider using Maybe or Either monad if pattern is not matched 

--2.3
-- If the list length is less than 2 than ghci throws an exception

--http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
--3.1, 3.2
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Tail recursion here enables a faster while loop with no context switch
myOtherLength xs = helper xs 0
  where helper [] n = n
        helper (x:xs) n = helper xs (n+1)

-- 3.3
myMean n = (foldl (+) 0 n) / (fromIntegral $ length n)

--3.4
toPal [] = []
toPal (x:xs) = [x] ++ toPal xs ++ [x]

--3.5
isPal :: (Eq a) => [a] -> Bool
isPal [] = True
isPal (x:[]) = False
isPal (x:xs) = (x == last xs) && (isPal $ init xs)
{- Keep eye out for how to do an eager evaluation -}

--3.6
sortByLength :: [[a]] -> [[a]]
sortByLength x = sortBy myOrder x
{-
Observe:
λ> :t sortOn
sortOn :: Ord b => (a -> b) -> [a] -> [a]
λ> :t sortBy
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-}

myOrder :: [a] -> [a] -> Ordering
myOrder x y | xn == yn = EQ
            | xn > yn  = GT
            | otherwise = LT
  where
    xn = length x
    yn = length y

--3.7
myIntersperse :: a -> [[a]] -> [a]
myIntersperse c [] = []
myIntersperse c (x:[]) = x
myIntersperse c (x:xs) = x ++ [c] ++ (myIntersperse c xs)

--3.8
data MyTree a = MyNode a (MyTree a) (MyTree a)
            | MyEmpty
              deriving (Show)
myHeight :: MyTree a -> Int
myHeight MyEmpty = 0
myHeight (MyNode x leftTree rightTree) = 1 + max (myHeight leftTree) (myHeight rightTree)

sampleTree = MyNode 4 (MyNode 3 (MyNode 2 MyEmpty MyEmpty) MyEmpty) (MyNode 1 MyEmpty MyEmpty)

--3.9
data Direction = Left | Right | Straight deriving (Show,Eq)

--3.10
data Point = Point Float Float deriving (Show,Eq)
data Segment = Segment Point Point deriving (Show,Eq)

sp1 = Point 0 0
sp2 = Point 0 1
sp3 = Point 1 1

s12 = Segment sp1 sp2
s23 = Segment sp2 sp3

data TriangleAngle = TriangleAngle {
  p1 :: Point,
  p2 :: Point,
  p3 :: Point
} deriving (Show,Eq)

sampleTriangle1 = TriangleAngle { p1=Point 0 1, p2=Point 0 0, p3=Point 1 0}
sampleTriangle2 = TriangleAngle { p1=Point 0 1, p2=Point 0 0, p3=Point 1 1}

{-
instance Ord (TriangleAngle) where
  ()
-}

turnType :: Point -> Point -> Point -> Direction
turnType (Point x1 y1) (Point x2 y2) (Point x3 y3)
  | rightComponentOfS2 >  epsilon = Lib.Right
  | rightComponentOfS2 < -epsilon = Lib.Left
  | otherwise                     = Lib.Straight
  where
    (s1x,s1y) = (x2-x1,y2-y1)
    (s2x,s2y) = (x3-x2,y3-y2)
    rightVectorOfS1 = (s1y,-s1x)
    rightComponentOfS2 = s2x*(fst rightVectorOfS1) + s2y*(snd rightVectorOfS1)
    epsilon = 1e-6

sampleTurn = turnType (Point 0 0) (Point 0 1) (Point 1 1)

--3.11
getDirections :: [Point] -> [Direction]
getDirections [] = []
getDirections (a:b:c:[]) = [turnType a b c]
getDirections (a:b:c:ps) = [turnType a b c] ++ getDirections (b:c:ps)

getDirectionsFaster :: [Point] -> [Direction]
getDirectionsFaster ps = helper ps []
  where helper []         ds = ds
        helper (a:b:c:[]) ds = ds ++ [turnType a b c]
        helper (a:b:c:ps) ds = helper (b:c:ps) (ds ++ [turnType a b c])

samplePoints = [(Point 0 0), (Point 0 1),(Point 1 1),(Point 2 1),(Point 2 2)]
sampleDirections = getDirections samplePoints
sampleDirections2 = getDirectionsFaster samplePoints

--3.12
getConvexHull :: [Point] -> [Point]
getConvexHull = undefined

findLowestAndLeftistPoint :: [Point] -> Point
findLowestAndLeftistPoint (p:ps) = helper p ps 
  where
    helper (Point x y) [] = Point x y
    helper (Point x y) ((Point a b):ps)
      | y < b = helper (Point x y) ps
      | otherwise = if x < a then helper (Point x y) ps else helper (Point a b) ps

p0 = findLowestAndLeftistPoint samplePoints

--
{-
sortByTriangle :: Point -> [Point] -> [Point]
sortByTriangle p0 ps = sortBy $ (on compare (angle p0)) ps
-}
angle :: Point -> Point -> Float
angle (Point x y) (Point a b) = atan2 (b-y) (a-x)

--sampleTriangle = angle (Point 0 0) (Point 1 1)

angleCompare :: Point -> Point -> Point -> Ordering
angleCompare p0 p1 p2
  | angle1_0 < angle2_0 = LT
  | angle1_0 > angle2_0 = GT
  | otherwise = EQ
  where
    angle1_0 = angle p0 p1
    angle2_0 = angle p0 p2
  

{-
***********************************************************
-}
{-
-- consider using zipper
removeLowestAndLeftistPoint :: [Point] -> [Point]
removeLowestAndLeftistPoint (p:ps) = helper p ps 
-}
findLowestPoint :: [Point] -> Point
findLowestPoint = minimumBy lowestPointCompare

sortByLowestPoint = sortBy lowestPointCompare

lowestPointCompare :: Point -> Point -> Ordering
lowestPointCompare (Point x1 y1) (Point x2 y2)
  | y1 < y2 = LT
  | y1 > y2 = GT
  | otherwise = EQ

{-
 A all too common work flow/environment problem that I am experiencing right now ...
 I looked at https://en.wikipedia.org/wiki/Graham_scan.
 It uses a stack data structure I want to use.
 So I looked at https://hackage.haskell.org/package/Stack-0.4.0/docs/Data-Stack.html
 I want to use it.
 Now I struggle finding the right import statement to use the stack data structure into GHCI.
 https://hackage.haskell.org/package/Stack-0.4.0/docs/src/Data.Stack.html#Stack shows a module "Data.Stack"
   λ> import Data.Stack

   <no location info>: error:
      Could not find module `Data.Stack'
      It is not a module in the current program, or in any known package.

  I don't know now if I need to add a dependency in the package.yaml  or the stack data structure is
  listed by another name in the base library.  Nor do I know how to find out.

  I now want to look at https://www.stackage.org/

  I could not find the Data.Stack hacakge.org code on Stackage.org

  https://www.stackage.org/package/containers

  added "containers" in package.yaml dependencies
-}

-- https://hackage.haskell.org/package/Stack-0.4.0/docs/src/Data.Stack.html#stackNew
data Stack a = Stack Int [a] deriving (Show, Read)

stackNew :: Stack a
stackNew = Stack 0 []

stackPush :: a -> Stack a -> Stack a
stackPush x (Stack n xs) = Stack (n+1) (x:xs)

stackPop :: Stack a -> Stack a
stackPop (Stack _ []) = stackNew
stackPop (Stack n (x:xs)) = Stack (n-1) xs

stackEmpty :: Stack a -> Bool
stackEmpty (Stack n _) = n == 0

stackPeek :: Stack a -> a
stackPeek (Stack _ (x:xs)) = x

{-
http://book.realworldhaskell.org/read/functional-programming.html
-}
--4.1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail [] = Nothing
safeTail xs = Just $ tail xs

safeLast [] = Nothing
safeLast xs = Just $ last xs

safeInit [] = Nothing
safeInit xs = Just $ init xs

--4.2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = [first] ++ splitWith f (dropWhile f second)
  where
    (first, second) = break f (dropWhile f xs)

someList = [1,2,4,5,6,4,7]
testSplit = splitWith (==4) someList

--4.3
firstWordOfEachLine s= map head (map words (lines s))
myString = "Hello World\nGood Bye"
test4_3 = firstWordOfEachLine myString

--4.4, ok I cheated on this one. I've seen matrix transpose using a list of lists  pattern before 
transposeText :: String -> String
transposeText s = unlines $ helper (lines s)
  where
    helper x
      | null x = []
      | any null x = []
      | otherwise = (map head x) : helper (map tail x)

test_4_4_string = "Hello\nWorld\nRicky"
answer_4_4_sample = transposeText test_4_4_string

transposeMatrix :: [[a]] -> [[a]]
transposeMatrix ([]:_) = []
transposeMatrix x = (map head x) : transposeMatrix (map tail x)
x = [[1,2,3],[4,5,6]]

-- 4b.1
asInt_fold :: String -> Int
asInt_fold ('-':ys) = -asInt_foldHelper ys
asInt_fold ys       =  asInt_foldHelper ys
asInt_foldHelper xs = foldl (\b -> \a -> (10 * b +  Data.Char.digitToInt a)) 0 xs


-- 4b.2
{-
asInt_fold2 :: String -> Either ErrorMessage Int
asInt_fold2 ('-':ys) = -asInt_foldHelper ys
asInt_fold2 ys       =  asInt_foldHelper ys
asInt_foldHelper2 xs = foldl (\b -> \a -> (10 * b +  Data.Char.digitToInt a)) 0 xs
-}
--Data.Either.Left 4

--4b.3
myConcat :: [[a]] -> [a]
myConcat x = foldr (++) [] x

{-
myGroupBy :: (a-> a-> Bool)-> [a] -> [[a]]
myGroupBy f [] = []
myGroupBy f (x:xs) = 
-}

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f xs = foldr step [] xs
  where step element grps = let relElem = f element in
          case find (all relElem) grps of
            Just grp -> (element : grp) : filter (all (not.relElem)) grps
            Nothing -> [element] : grps

-- 5.1
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)
{-
fill :: Int -> Doc -> Doc
fill w d = undefined
-}

data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = False

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

class BasicEq a where
  isEqual :: a -> a -> Bool
  next :: a -> a

instance BasicEq Color where
  isEqual Red Red = True
  isEqual Green Green = True
  isEqual Blue Blue = True
  isEqual _ _ = False
  next Red = Green
  next Green = Blue
  next Blue = Red

-- Chapter 6
newtype NewtypeInt = N Int deriving (Eq, Ord, Show)

data TwoFields = TwoFields Int Int

newtype Okay = ExactlyOne Int
newtype Param a b = Param (Either a b)
newtype Record = Record {
      getInt :: Int
    }

--e1
