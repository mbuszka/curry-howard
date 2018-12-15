{-# LANGUAGE 
    DataKinds
  , TypeFamilies
  , GADTs
  , MultiParamTypeClasses
  , FlexibleInstances
  #-}

module Paper.Sorted where

import Paper.Nat
import Data.Kind



{-  Sorted sequence, which holds evidence that that at each cons head is
    greater than or equal to tail. It's called dynamically sorted because
    evidence had to be established at runtime -}
data Dss :: Nat -> Type where
  DNil :: Dss Z
  DCons :: SNat n -> LE m n -> Dss m -> Dss n



{-  Existentially quantified wrapper around things which are parameterized
    by Nat. It allows us to have a list of different SNat's -}
data Covert :: (Nat -> Type) -> Type where
  Hide :: t n -> Covert t



example1 :: [Covert SNat]
example1 = [ Hide one, Hide four, Hide three ]



-- Merges two dynamically sorted sequences, using comp for generating proofs
merge :: Dss n -> Dss m -> Either (Dss n) (Dss m)
merge DNil ys = Right ys
merge xs DNil = Left xs
merge a@(DCons x xgt xs) b@(DCons y ygt ys) = case comp x y of
  Left  xley -> case merge a ys of
    Left  ws -> Right (DCons y xley ws)
    Right ws -> Right (DCons y ygt ws)
  Right ylex -> case merge b xs of
    Left  ws -> Left (DCons x ylex ws)
    Right ws -> Left (DCons x xgt ws)



-- Helper function, splits two list, accumulator style
split :: [a] -> ([a], [a]) -> ([a], [a])
split [] acc = acc
split [x] (xs, ys) = (x:xs, ys)
split (x:y:xs) (ys, zs) = split xs (x:ys, y:zs)


-- Sorts a list of SNat's
-- Results of recursive calls have to be unwrapped with a case construct
-- because of implementation limitations of existential types
sort :: [Covert SNat] -> Covert Dss
sort [] = Hide DNil
sort [Hide n] = Hide (DCons n Base DNil)
sort xs = let 
    (l, r) = split xs ([], [])
    in case (sort l, sort r) of 
      (Hide ys, Hide zs) ->
        case merge ys zs of
          Left res -> Hide res
          Right res -> Hide res



-- Typeclass which statically estabilishes that n <= m
class SLE (n :: Nat) (m :: Nat) where
instance SLE Z m where
instance SLE n m => SLE (S n) (S m) where



{- PATTERN Witness -}
-- Witness object for SLE
data LE' :: Nat -> Nat -> Type where
  LE' :: SLE n m => LE' n m



-- Statically sorted sequence, ie it does not contain proof objects
data Sss :: Nat -> Type where
  SNil :: Sss Z
  SCons :: SLE n m => SNat m -> Sss n -> Sss m



-- Compares to SNat's and return a witness
comp2 :: SNat n -> SNat m -> Either (LE' n m) (LE' m n)
comp2 SZ _m = Left  LE'
comp2 _n SZ = Right LE'
comp2 (SS n) (SS m) = case comp2 n m of
  Left LE'  -> Left LE'
  Right LE' -> Right LE'



-- Merges two statically sorted sequences
merge2 :: Sss n -> Sss m -> Either (Sss n) (Sss m)
merge2 SNil ys = Right ys
merge2 xs SNil = Left xs
merge2 a@(SCons x xs) b@(SCons y ys) = case comp2 y x of
  Left LE'  -> case merge2 xs b of
    Left  w -> Left (SCons x w)
    Right w -> Left (SCons x w)
  Right LE' -> case merge2 a ys of
    Left  w -> Right (SCons y w)
    Right w -> Right (SCons y w)



-- Sorts a list and returns a statically sorted sequence
sort2 :: [Covert SNat] -> Covert Sss
sort2 [] = Hide SNil
sort2 [Hide x] = Hide (SCons x SNil)
sort2 xs =
  let (l, r) = split xs ([], [])
  in case (sort2 l, sort2 r) of
    (Hide ys, Hide zs) -> case merge2 ys zs of
      Left res -> Hide res
      Right res -> Hide res



{- Helper functions -}

toList :: Dss n -> [Int]
toList DNil = []
toList (DCons x _ xs) = toInt x : toList xs 

toList2 :: Sss n -> [Int]
toList2 SNil = []
toList2 (SCons x xs) = toInt x : toList2 xs