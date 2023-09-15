module Nat where
import Prelude 
  hiding ((+), (*), (^), quot, min, div, max, pred, rem)

data Nat = O | S Nat
  deriving (Eq , Show)

(+) :: Nat -> Nat -> Nat
(+) m O = m
(+) m (S n) = S (m + n)
  
(*) :: Nat -> Nat -> Nat
(*) m O = O
(*) m (S n) = m + (m * n)

(^) :: Nat -> Nat -> Nat
(^) m O = (S O)
(^) m (S n) = m * (m ^ n)

double :: Nat -> Nat
double O = O
double (S n) = S (S (double n))

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
min (S m) (S n) = S (min m n)

max :: Nat -> Nat -> Nat
max O m = m
max m O = m
max (S m) (S n) = S (max m n)

quot :: Nat -> Nat -> Nat
quot m n = quot' m n n
  where
    quot' :: Nat -> Nat -> Nat -> Nat
    quot' O O k = S O
    quot' (S m) (S n) k = quot' m n k
    quot' m O k = S (quot' m k k)
    quot' O m k = O

rem :: Nat -> Nat -> Nat
rem O n = O
rem (S m) O = S (rem m O)
rem m n = rem' m (n * (quot m n))
  where
    rem' :: Nat -> Nat -> Nat
    rem' (S m) (S n) = rem' m n
    rem' m O = m

div :: Nat -> Nat -> (Nat, Nat)
div m n = (quot m n, rem m n)

-- gcd :: Nat -> Nat -> Nat
-- mdc

-- lcm :: Nat -> Nat -> Nat
-- mmc