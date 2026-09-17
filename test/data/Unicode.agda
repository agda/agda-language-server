module Unicode where

data ℕ' : Set where
  zero' : ℕ'
  suc'  : ℕ' → ℕ'

double : ℕ' → ℕ'
double zero'    = zero'
double (suc' n) = suc' (suc' (double n))

αβγ : ℕ' → ℕ'
αβγ = double
