module Example where

-- Basic natural numbers
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

-- Addition with holes for testing
_+_ : Nat → Nat → Nat
zero + n = {! !}
suc m + n = {! !}

-- Multiplication to test
_*_ : Nat → Nat → Nat
zero * n = {! !}
suc m * n = {! !}

-- A simple proof to test refinement
identity : (A : Set) → A → A
identity A x = {! !}
