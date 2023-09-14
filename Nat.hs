module Nat where
import Prelude 
  hiding ((+), (*), (^), min, max, pred)

data Nat = O | S Nat
  deriving (Eq , Show)

(+) :: Nat -> Nat -> Nat
(+) m O = m
(+) m (S n) = S(m + n)
  
(*) :: Nat -> Nat -> Nat
(*) m O = O
(*) m (S n) = m + (m * n)

(^) :: Nat -> Nat -> Nat
(^) m O = (S O)
(^) m (S n) = m * (m ^ n)

double :: Nat -> Nat
double O = O
double (S n) = S(S(double n))

pred :: Nat -> Nat
pred O = O
pred (S m) = m

fact :: Nat -> Nat
fact O = (S O)
fact (S m) = (S m) * (fact m)

fib :: Nat -> Nat
fib O = O
fib (S O) = (S O)
fib (S (S m)) = fib (S m) + fib m

min :: Nat -> Nat -> Nat
min O m = O
min m O = O
min (S m) (S n) = S(min m n)

max :: Nat -> Nat -> Nat
max O m = m
max m O = m
max (S m) (S n) = S(max m n)

-- div :: Nat -> Nat -> Nat

