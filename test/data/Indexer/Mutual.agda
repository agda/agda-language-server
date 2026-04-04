module Mutual where

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

module Forward where
  interleaved mutual
    record MutualRecord (A : Set) (n : Nat) : Set
    data MutualData : Set
    data MutualData' (A : Set) : Nat -> Set
    mutualFun : MutualRecord MutualData zero -> MutualRecord MutualData zero

    record MutualRecord A m where
      constructor makeMutualRecord
      field
        mutFieldX : A
      field mutFieldY : Nat

    data MutualData where
      mutCtorX : MutualData
    
    data _ where
      mutCtorX' : MutualData' A zero
      mutCtorY : MutualData
    
    mutualFun x = x

module Backward where
  interleaved mutual
    data MutualData where
      mutCtorX : MutualData
    
    data MutualData' where
      mutCtorX' : MutualData' A zero
    
    data MutualData where
      mutCtorY : MutualData
    
    data MutualData : Set
    data MutualData' (A : Set) : Nat -> Set
