{-# LANGUAGE 
    DataKinds
  , TypeFamilies
  , GADTs
  #-}

module Nat where

import Data.Kind



data Nat where
  Z :: Nat
  S :: Nat -> Nat



type One = S Z
type Two = S One
type Three = S Two
type Four = S Three



type family Add n m :: Nat where
  Add Z n = n
  Add (S n) m = S (Add n m)


{-  PATTERN Singleton types
    A one-to-one reflection of a type into
    runtime data structure.
-}
data SNat :: Nat -> Type where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

sthree :: SNat (S (S (S Z)))
sthree = SS (SS (SS SZ))


one = SS SZ
two = SS one
three = SS two
four = SS three



add :: SNat n -> SNat m -> SNat (Add n m)
add SZ n = n
add (SS n) m = SS (add n m)


{-  PATTERN Propositions
    Data types which are proofs of some propositions
    about types
-}
data Even :: Nat -> Type where
  EvenZ  :: Even Z
  EvenS :: Odd n -> Even (S n)



data Odd :: Nat -> Type where
  OddS :: Even n -> Odd (S n)



even4 :: Even Four
even4 = EvenS (OddS (EvenS (OddS EvenZ)))



data LE :: Nat -> Nat -> Type where
  Base :: LE Z n
  Step :: LE n m -> LE (S n) (S m)



{-  PATTERN Runtime proposition building -}
-- Comp returns value which is a proof that either n <= m or m <= n
comp :: SNat n -> SNat m -> Either (LE n m) (LE m n)
comp SZ _ = Left Base
comp (SS n) SZ = Right Base
comp (SS n) (SS m) = case comp n m of
  Left ev -> Left (Step ev)
  Right ev -> Right (Step ev)



{- Helper functions -}
toInt :: SNat n -> Int
toInt SZ = 0
toInt (SS n) = 1 + toInt n