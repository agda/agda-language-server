module Dep where

data T : Set where
  t : T

f : T -> T -> T
f x _ = x
