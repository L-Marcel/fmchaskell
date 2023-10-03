module Nat where

import Prelude
  hiding ((-), last, (<), (>), (>=), (<=), compare, init, isPrefixOf, maximum, minimum, drop, take, enumFromTo, reverse, (++), product, sum, elem, length, (+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem)
import Data
import Bool
import Ordering

(+) :: Nat -> Nat -> Nat
m + O = m
m + (S n) = S (m + n)

(*) :: Nat -> Nat -> Nat
m * O = O
m * (S n) = m + (m * n)

(^) :: Nat -> Nat -> Nat
m ^ O = S O
m ^ (S n) = m * (m ^ n)

(-) :: Nat -> Nat -> Nat
O - m = O
n - O = n
(S n) - (S m) = n - m

double :: Nat -> Nat
double O = O
double (S n) = S (S (double n))

pred :: Nat -> Nat
pred O = O
pred (S m) = m

fact :: Nat -> Nat
fact O = S O
fact (S m) = S m * fact m

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S m)) = fib (S m) + fib m

min :: Nat -> Nat -> Nat
min (S m) (S n) = S (min m n)
min _ _ = O

max :: Nat -> Nat -> Nat
max (S m) (S n) = S (max m n)
max O m = m
max m O = m

div :: (Nat, Nat) -> (Nat, Nat)
div (a, b)
  | a < b     = (O, a)
  | otherwise = let (q', r') = div (a - b, b)
                 in (S q', r')

quot :: Nat -> Nat -> Nat
quot m n = fst (div (m, n))
-- quot m n = quot' m n n
--  where
--    quot' :: Nat -> Nat -> Nat -> Nat
--    quot' O O k = S O
--    quot' (S m) (S n) k = quot' m n k
--    quot' m O k = S (quot' m k k)
--    quot' O m k = O

rem :: Nat -> Nat -> Nat
rem m n = snd (div (m, n))
-- rem O n = O
-- rem (S m) O = S (rem m O)
-- rem m n = rem' m (n * quot m n)
--  where
--    rem' :: Nat -> Nat -> Nat
--    rem' (S m) (S n) = rem' m n
--    rem' m O = m


-- máximo divisor comum -- fix
gcd :: Nat -> Nat -> Nat
gcd m n = gcd' m n (S O) (S (min m n))
  where
    gcd' :: Nat -> Nat -> Nat -> Nat -> Nat
    gcd' m n p (S O) = S O
    gcd' m n O q = q
    gcd' m n p (S q) = gcd' m n (max (rem m q) (rem n q)) q

-- mínimo multiplo comum -- fix
lcm :: Nat -> Nat -> Nat
lcm m n = lcm' m n (S O) (pred (max m n))
  where
    lcm' :: Nat -> Nat -> Nat -> Nat -> Nat
    lcm' m n O q = q
    lcm' O n p q = O
    lcm' m O p q = O
    lcm' m n p q = lcm' m n (max (rem (S q) m) (rem (S q) n)) (S q)