module Basic where

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

one : Nat
one = suc zero

two : Nat
two = suc one

data Test : Set where
    test : Test

data Test' : Set where
    test : Test'

hello : Test
hello = test

data Empty : Set where

Not : Set -> Set
Not A = A -> Empty

Goodbye : Set
Goodbye = Not Test

postulate A : Test -> Set

testId : Test -> Test
testId test = test

id : {A : Set} -> A -> A
id = \where x -> x

id' : {A : Set} -> A -> A
id' = \x -> let x = x in x

id'' : {A : Set} -> A -> A
id'' {A = A} x =
  let x = x
      A = A
    in x

record World : Set where
  field
    north : Test
    south : Test'

earth : World
earth .World.north = test
earth .World.south = test
