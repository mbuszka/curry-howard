{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Homework where

import Nat
import Seq

-- EXERCISE 1
length :: Seq n a -> SNat n
length = undefined

-- EXERCISE 2
replicate :: SNat n -> a -> Seq n a
replicate = undefined

-- EXERCISE 3
tail :: Seq (S n) a -> Seq n a
tail = undefined

-- EXERCISE 4
transpose :: Seq n (Seq m a) -> Seq m (Seq n a)
transpose = undefined