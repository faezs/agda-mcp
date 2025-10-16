module EditPersistenceTest where

-- Section 1: Simple give tests
data Bool : Set where
  true : Bool
  false : Bool

simpleGive : Bool
simpleGive = {! !}

-- Section 2: Refinement tests
data Nat : Set where
  zero : Nat
  suc : Nat → Nat

refineTest : Nat
refineTest = {! !}

-- Section 3: Case split tests
_+_ : Nat → Nat → Nat
n + m = {! !}

_*_ : Nat → Nat → Nat
n * m = {! !}

-- Section 4: Multiple holes for batch tests
batchTest1 : Bool
batchTest1 = {! !}

batchTest2 : Nat
batchTest2 = {! !}

batchTest3 : Bool
batchTest3 = {! !}

-- Section 5: Unicode tests (ensure UTF-8 handling)
data ⊤ : Set where
  tt : ⊤

unicodeTest : ⊤
unicodeTest = {! !}

-- Section 6: Nested case splits
nestedSplit : Nat → Nat → Nat
nestedSplit n m = {! !}

-- Section 7: Expression give test
expressionTest : Nat
expressionTest = {! !}
