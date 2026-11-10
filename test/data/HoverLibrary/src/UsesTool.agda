{-# OPTIONS --allow-exec #-}
module UsesTool where

open import Dep

g : _
g = f

open import Agda.Builtin.List
open import Agda.Builtin.Reflection
open import Agda.Builtin.Reflection.External
open import Agda.Builtin.Unit

macro
  run : Term -> TC ⊤
  run hole = bindTC (execTC "tool" [] "") \ _ -> unify hole (con (quote tt) [])

u : ⊤
u = run

-- the hole stops Agda from writing an interface file, so every load
-- checks this module and runs the tool again
h : ⊤
h = {! !}
