{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Paper.Seq where

import Paper.Nat



{-  PATTERN Indexed datatypes
    Data types indexed with custom types which assert some properties
-}
-- A type of length indexed sequences
data Seq :: Nat -> * -> * where
  Nil :: Seq Z a
  Cons :: a -> Seq n a -> Seq (S n) a



-- Head of nonempty sequence
head :: Seq (S _n) a -> a
head (Cons x _) = x



-- Appending two sequences
app :: Seq n a -> Seq m a -> Seq (Add n m) a
app Nil ys = ys
app (Cons x xs) ys = Cons x (app xs ys)



-- Mapping a function over sequence preserves length
map :: (a -> b) -> Seq n a -> Seq n b
map f Nil = Nil
map f (Cons x xs) = Cons (f x) (Paper.Seq.map f xs)



-- EXERCISE 1
transpose :: Seq n (Seq m a) -> Seq m (Seq n a)
transpose = undefined



{- Helper functions -}
toList :: Seq n a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

instance Show a => Show (Seq n a) where
  show = show . toList
