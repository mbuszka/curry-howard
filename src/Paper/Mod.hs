{-# LANGUAGE 
    DataKinds
  , TypeFamilies
  , GADTs
  , FlexibleInstances
  , TypeSynonymInstances
  #-}

module Paper.Mod where

import Data.Kind
import Paper.Nat


{- Datatype for integers modulo n -}
data Mod :: Nat -> Type where
  Mod :: Int -> SNat n -> Mod n



normalize :: Mod n -> Mod n
normalize (Mod x n) = Mod (mod x (toInt n)) n

plusM :: Mod n -> Mod n -> Mod n
plusM (Mod x n) (Mod y _) = normalize $ Mod (x + y) n

minusM :: Mod n -> Mod n -> Mod n
minusM (Mod x n) (Mod y _) = normalize $ Mod (x - y) n

timesM :: Mod n -> Mod n -> Mod n
timesM (Mod x n) (Mod y _) = normalize $ Mod (x * y) n



{- For demonstration purposes lists first four prime numbers -}
class Prime (n :: Nat) where

instance Prime Two where
instance Prime Three where
instance Prime (S Four) where
instance Prime (S (S (S Four))) where



-- Get inverse of a number modulo p, where p is prime
invM :: Prime n => Mod n -> Mod n
invM (Mod a n) = Mod (b `mod` n') n where
  n' = toInt n
  (_, _, b) = Paper.Mod.gcd n' a



{- Helper functions -}

instance Show (Mod n) where
  show (Mod x n) = "Mod " ++ show x ++ " " ++ show (toInt n)

gcd p q | p < q = Paper.Mod.gcd q p
gcd p q | q == 0 = (p, 1, 0)
gcd p q = (g, y1, x1 - (p `div` q) * y1)
  where (g, x1, y1) = Paper.Mod.gcd q (p `mod` q)
  
