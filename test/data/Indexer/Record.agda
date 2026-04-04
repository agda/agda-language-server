module Record where

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

record R (n : Nat) (A : Nat -> Set) (x : A n) : Set where
  constructor makeR
  field
    fieldX : Nat
    fieldY : A n
    fieldZ : A fieldX
