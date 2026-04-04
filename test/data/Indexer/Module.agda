module Indexer.Module where

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

module M where
  f : Nat -> Nat
  f n = n

  data N : Set where
    n : N
