module Nat where
import Prelude 
  hiding ((+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem)

data Nat = O | S Nat
  deriving (Eq , Show)

-- adição
(+) :: Nat -> Nat -> Nat
(+) m O = m
(+) m (S n) = S (m + n)

-- multiplicação
(*) :: Nat -> Nat -> Nat
(*) m O = O
(*) m (S n) = m + (m * n)

-- exponenciação
(^) :: Nat -> Nat -> Nat
(^) m O = (S O)
(^) m (S n) = m * (m ^ n)

-- dobro
double :: Nat -> Nat
double O = O
double (S n) = S (S (double n))

-- predecessor
pred :: Nat -> Nat
pred O = O
pred (S m) = m

-- fatorial
fact :: Nat -> Nat
fact O = (S O)
fact (S m) = (S m) * (fact m)

-- fibonacci
fib :: Nat -> Nat
fib O = O
fib (S O) = (S O)
fib (S (S m)) = fib (S m) + fib m

-- valor mínimo
min :: Nat -> Nat -> Nat
min O m = O
min m O = O
min (S m) (S n) = S (min m n)

-- valor máximo
max :: Nat -> Nat -> Nat
max O m = m
max m O = m
max (S m) (S n) = S (max m n)

-- quociente
quot :: Nat -> Nat -> Nat
quot m n = quot' m n n
  where
    quot' :: Nat -> Nat -> Nat -> Nat
    quot' O O k = S O
    quot' (S m) (S n) k = quot' m n k
    quot' m O k = S (quot' m k k)
    quot' O m k = O

-- resto
rem :: Nat -> Nat -> Nat
rem O n = O
rem (S m) O = S (rem m O)
rem m n = rem' m (n * (quot m n))
  where
    rem' :: Nat -> Nat -> Nat
    rem' (S m) (S n) = rem' m n
    rem' m O = m

-- (quociente, resto)
div :: Nat -> Nat -> (Nat, Nat)
div m n = (quot m n, rem m n)

-- máximo divisor comum
gcd :: Nat -> Nat -> Nat
gcd m n = gcd' m n (S O) (S (min m n))
  where
    gcd' :: Nat -> Nat -> Nat -> Nat -> Nat
    gcd' m n p (S O) = S O
    gcd' m n O q = q
    gcd' m n p (S q) = gcd' m n (max (rem m q) (rem n q)) q

-- mínimo multiplo comum
lcm :: Nat -> Nat -> Nat
lcm m n = lcm' m n (S O) (pred (max m n))
  where 
    lcm' :: Nat -> Nat -> Nat -> Nat -> Nat
    lcm' m n O q = q
    lcm' O n p q = O
    lcm' m O p q = O
    lcm' m n p q = lcm' m n (max (rem (S q) m) (rem (S q) n)) (S q)