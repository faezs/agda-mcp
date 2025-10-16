module PostulateTest where

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

data Bool : Set where
  true  : Bool
  false : Bool

-- Some postulates to test the list_postulates tool
postulate
  foo : Nat
  bar : Bool
  baz : Nat → Nat

postulate
  identity : {A : Set} → A → A
  composition : {A B C : Set} → (B → C) → (A → B) → A → C

-- A regular function (should not be listed)
add : Nat → Nat → Nat
add zero m = m
add (suc n) m = suc (add n m)
