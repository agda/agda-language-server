module FunWithoutDeclOrDef where

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

-- No declaration
sucsuc = \n -> suc (suc n)

-- No definition
sucsucsuc : Nat -> Nat
