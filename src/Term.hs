{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Term where

data Term a where
  Const :: a -> Term a
  App :: Term (a -> b) -> Term a -> Term b

eval :: Term a -> a
eval (Const x) = x
eval (App f x) = eval f $ eval x

example1 :: Term Int
example1 = App (Const (+1)) (Const 7)

example2 :: Term String
example2 = App (App (Const (++)) (Const "Hello ")) (Const "World!")