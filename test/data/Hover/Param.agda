module Param (A : Set) where

open import Dep

data D : Set where
  d : A → D

g : Set
g = D

e : A → D
e = d
