module Lib
    ( 
    ) where
data Bit = Zero | One deriving (Eq,Show)
type Natural = Integer
type Cantor = Natural -> Bit
(#) :: Bit -> Cantor -> Cantor
x # a = \i -> if i == 0 then x else a (i-1)
forsome, forevery :: (Cantor -> Bool) -> Bool
find :: (Cantor -> Bool) -> Cantor
forsome p = p (find (\a -> p a))
forevery p = not forsome (\a -> not (p a))
find p = if forsome (\a -> p (Zero # a))
  then Zero # find(\a -> p(Zero # a))
  else One  # find(\a -> p(One  # a))


