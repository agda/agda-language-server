module Tokens where

open import Dep

_⊕_ : T → T → T
x ⊕ y = x

id′ : T → T
id′ x = x

h : T → T
h x = x ⊕ helper
  where
  helper : T
  helper = Dep.f x
