module Lib
    ( graphToAdj,
      ipl,
      nnodes,
      countLeaves,
      someFunc,
      myLast,
      myButLast,
      elementAt,
      myLength,
      myReverse,
      isPalindrome,
      flatten,
      compress,
      pack,
      pack2,
      encode,
      encodeModified,
      decodes,
      decodesModified,
      dupli,
      repli,
      dropEvery,
      split,
      slice,
      rotate,
      removeAt,
      insertAt,
      range,
      combinations,
      lsort,
      lfsort,
      isPrime,
      myGCD,
      coprime,
      totient,
      primeFactors,
      primeFactorsMult,
      totient2,
      primesR,
      goldbach,
      not',
      and',
      or',
      nand',
      xor',
      impl',
      equ',
      table,
      gray,
      symmetric,
      Tree(Empty,Branch),
      MTree(Node),
    ) where

import Data.List
--import System.Random
import Control.Monad (replicateM)
import Data.Ord (comparing)
import Data.Function

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myLast :: [a]-> a
myLast []  = error "Not applicable"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "Not applicable"
myButLast [x] = error "Not applicable"
myButLast (x:(_:[])) = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _      = error "Not applicable"
elementAt (_:xs) n
  | n < 1 = error "Not applicable"
  | otherwise = elementAt xs (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

data NestedList a = Elem a | List [NestedList a] deriving (Show)

tt =  (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

compress :: Eq a => [a] -> [a]
compress (x:ys@(y:_))
  | x == y    = compress ys
  | otherwise = x:compress ys
compress ys   = ys

pack :: Eq a => [a]->[[a]]
pack (x:xs) = (x:first):pack rest where
  (first,rest)=span (==x) xs
pack [] = []

pack2:: Eq a => [a]->[[a]]
pack2 [] = []
pack2 (x:xs) = (x:(filter (==x) xs)):(pack2 (filter (/=x) xs))

encode :: Eq a => [a]->[(Int,a)]
encode xs = map (\x -> (length x,head x)) (group xs)

data ListItem a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a =>[a]->[ListItem a]
encodeModified = map encodeHelper . encode
  where
    encodeHelper (1,x) = Single x
    encodeHelper (n,x) = Multiple n x

decodes :: ListItem a -> [a]
decodes (Single x) = [x]
decodes (Multiple n x) = replicate n x

decodesModified :: [ListItem a] -> [a]
decodesModified = concatMap decodes

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

repli ::[a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helpDrop xs n 1
  where
    helpDrop []     _ _ = []
    helpDrop (x:xs) n i =
      (if (mod i n)==0 then [] else [x])
      ++
      (helpDrop xs n (i+1))

split :: [a] -> Int -> ([a],[a])
split (x:xs) n
  | n > 0      = let (f,l) = split xs (n-1) in (x:f,l)
split xs     _ = ([],xs)

{-
split (x:xs) 1 = ([x],xs)
split [x]    _ = ([x],[])
split []     _ = ([],[])
split (x:xs) n = ([x]++fst z,snd z)
  where z = split xs (n-1)
-}

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) i k
  | i > 1 = slice xs (i-1) (k-1)
  | k < 1 = []
  | otherwise = x:slice xs (i-1) (k-1)


rotate :: [a] -> Int -> [a]
rotate [] _     = []
rotate xs 0     = xs
rotate (x:xs) (n)
  | n >= 1    = rotate (xs++[x]) (n-1)
  | otherwise = rotate (reverse (x:xs)) (negate n)

removeAt :: Int -> [a] -> [a]
removeAt n xs = take (n-1) xs ++ drop n xs

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n-1) xs ++ [x] ++ drop n xs

range :: Int -> Int -> [Int]
range a b
  | a <= b    = a:range (a+1) b
  | otherwise = []

{-
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select l n
  | n < 0 = error "N must be greater than zero."
  | otherwise = do pos <- replicateM n
-}

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy ((==) `on` length) . lsort

isPrime :: Integral a => a -> Bool
isPrime n | n < 4 = n /= 1
isPrime n = all ((/=0).mod n) $ takeWhile (<=m) candidates
  where candidates = (2:3:[x+i|x<-[6,12..],i<-[-1,1]])
        m = floor . sqrt $ fromIntegral n

myGCD :: Integral a => a -> a -> a
myGCD a b | b == 0    = abs a
          | otherwise = myGCD b (mod a b)

coprime :: Integral a => a -> a -> Bool
coprime a b = myGCD a b == 1

totient :: Integral a => a -> a
totient 1 = 1
totient n = fromIntegral $ length $ filter (coprime n) [1..n-1]

primeFactors :: Integral a => a -> [a]
primeFactors n = primeFactorsHelper n 2
  where
    primeFactorsHelper n f
      | f*f > n      = [n]
      | mod n f == 0 = f : primeFactorsHelper (div n f) f
      | otherwise    = primeFactorsHelper n (f+1)

primeFactorsMult n = zip (map head grouped_factors) (map length grouped_factors)
  where
    grouped_factors = group . sort $ primeFactors n

totient2 :: Integral a => a -> a
totient2 1 = 1
totient2 n = product [(p-1)*p^(m-1) | (p,m) <- primeFactorsMult n]

primesR :: Integral a => a -> a -> [a]
primesR a b = filter (isPrime) [a..b]

goldbach n = head [(x,y)|x<-pr,y<-pr,x+y==n]
  where pr = primesR 2 (n-2)

not' :: Bool -> Bool
not' True = False
not' False = True

and',or',nand',xor',impl',equ' :: Bool -> Bool -> Bool
and'  a b = a && b
or'   a b = a || b
nand' a b = not (and' a b)
nor'  a b = not (or' a b)
xor'  a b = not (equ' a b)
impl' a b = or' (not a) b
equ'  a b = a == b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show(f a b)
                         | a <- [True,False], b<-[True,False]]

gray :: Int -> [String]
gray 0 = [""]
gray n = ['0':x | x <- prev ] ++ ['1':x | x <- reverse prev]
  where prev = gray (n-1)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

symmetric :: Tree a -> Bool
symmetric t = helper t t
  where
    helper Empty Empty = True
    helper (Branch _ ll lr) (Branch _ rl rr) = helper ll rr && helper lr rl
    helper _ _ = False

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

atLevel :: Tree a -> Int -> [a]
atLevel Empty _  = []
atLevel (Branch x l r) n
  | n == 1 = [x]
  | n > 1  = atLevel l (n-1) ++ atLevel r (n-1)
  | otherwise = []

data MTree a = Node a [MTree a] deriving (Eq, Show)

nnodes :: MTree a -> Int
nnodes (Node _ ts) = 1 + sum (map nnodes ts)

newtype P a = P { runP :: String -> Maybe (a,String) }

ipl :: MTree a -> Int
ipl = helper 0
  where helper d (Node _ ts) = d + sum (map (helper (d+1)) ts)

data Graph a     = Graph [a] [(a,a)] deriving (Show, Eq)
data Adjacency a = Adj   [(a,[a])]   deriving (Show, Eq)
data Friendly a  = Edge  [(a,a)]     deriving (Show, Eq)

-- graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])
-- [n(b,[c,f]), n(c,[b,f]), n(d,[]), n(f,[b,c,k]), ...]

graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _ )     = Adj []
graphToAdj (Graph (x:xs) ys) = Adj ((x,ys>>=f):zs)
  where
    f (a,b)
      | a == x = [b]
      | b == x = [a]
      |otherwise = []
    Adj zs = graphToAdj (Graph xs ys)


