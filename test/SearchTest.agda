module SearchTest where

-- Import from 1lab for rich type environment
open import 1Lab.Prelude
open import 1Lab.Path
open import 1Lab.Type

-- Local definitions with interesting types
example-id : {A : Type} → A → A
example-id x = x

example-const : {A B : Type} → A → B → A
example-const x y = x

example-compose : {A B C : Type} → (B → C) → (A → B) → (A → C)
example-compose f g x = f (g x)

-- Path-related (from 1lab)
example-refl : {A : Type} {x : A} → x ≡ x
example-refl = refl

example-sym : {A : Type} {x y : A} → x ≡ y → y ≡ x
example-sym = sym

-- Sum types
example-inl : {A B : Type} → A → A ⊎ B
example-inl = inl

example-inr : {A B : Type} → B → A ⊎ B
example-inr = inr
