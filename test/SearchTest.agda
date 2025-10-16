module SearchTest where

open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.List
open import Agda.Builtin.Equality

-- Use Set for testing Type searches
data _⊎_ (A B : Set) : Set where
  inl : A → A ⊎ B
  inr : B → A ⊎ B

-- Local definitions with Set in types
example-id : {A : Set} → A → A
example-id x = x

example-const : {A B : Set} → A → B → A
example-const x y = x

example-compose : {A B C : Set} → (B → C) → (A → B) → (A → C)
example-compose f g x = f (g x)

-- Equality-related
example-refl : {A : Set} {x : A} → x ≡ x
example-refl = refl

example-sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
example-sym refl = refl

-- Sum types using our local definition
example-inl : {A B : Set} → A → A ⊎ B
example-inl = inl

example-inr : {A B : Set} → B → A ⊎ B
example-inr = inr
