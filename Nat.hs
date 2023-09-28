{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use guards" #-}

module Nat where

import Prelude
  hiding ((==), drop, take, enumFromTo, not, reverse, (++), product, sum, elem, length, (+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem)

data Nat = O | S Nat
  deriving (Eq, Show)

-- adição
(+) :: Nat -> Nat -> Nat
m + O = m
m + (S n) = S (m + n)

-- multiplicação
(*) :: Nat -> Nat -> Nat
m * O = O
m * (S n) = m + (m * n)

-- exponenciação
(^) :: Nat -> Nat -> Nat
m ^ O = (S O)
m ^ (S n) = m * (m ^ n)

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
min (S m) (S n) = S (min m n)
min _ _ = O

-- valor máximo
max :: Nat -> Nat -> Nat
max (S m) (S n) = S (max m n)
max O m = m
max m O = m

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

-- Bool -----------------

(==) :: Nat -> Nat -> Bool
O == O = True
(S n) == (S m) = n == m
_ == _ = False

not :: Bool -> Bool
not True = False
not False = True

ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S(S n)) = ev n

od :: Nat -> Bool
od n = not (ev n)

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- Inequality -----------------

gt :: Nat -> Nat -> Bool
gt (S n) (S m) = gt n m
gt O (S m) = False
gt (S n) O = True

-- ListNat --------------

type ListNat = [Nat]

length :: ListNat -> Nat
length [] = O
length (n : ns) = S(length ns)

elem :: Nat -> ListNat -> Bool
elem _ [] = False
elem m (n : ns) = if n == m
                    then True
                    else elem m ns

sum :: ListNat -> Nat
sum [] = O
sum (n : ns) = n + sum ns

product :: ListNat -> Nat
product [] = S O
product (n : ns) = n * product ns

(++) :: ListNat -> ListNat -> ListNat
[] ++ ns = ns
(m : ms) ++ ns = m : (ms ++ ns)

append :: Nat -> ListNat -> ListNat
append n = (++ [n])

reverse :: ListNat -> ListNat
reverse [] = []
reverse (n : ns) = append n (reverse ns)

allEven :: ListNat -> Bool
allEven [] = True
allEven (n : ns) = ev n && allEven ns

anyEven :: ListNat -> Bool
anyEven [] = False
anyEven (n : ns) = ev n || anyEven ns

allOdd :: ListNat -> Bool
allOdd [] = True
allOdd (n : ns) = od n && allOdd ns

anyOdd :: ListNat -> Bool
anyOdd [] = False
anyOdd (n : ns) = od n || anyOdd ns

allZero :: ListNat -> Bool
allZero [] = True
allZero (n : ns) = isZero n && allZero ns

anyZero :: ListNat -> Bool
anyZero [] = False
anyZero (n : ns) = isZero n || anyZero ns

addNat :: Nat -> ListNat -> ListNat
addNat m [] = []
addNat m (n : ns) = (m + n) : (addNat m ns)

mulNat :: Nat -> ListNat -> ListNat
mulNat m [] = []
mulNat m (n : ns) = (m * n) : (mulNat m ns)

expNat :: Nat -> ListNat -> ListNat
expNat m [] = []
expNat m (n : ns) = (n ^ m) : (expNat m ns)

enumFromTo :: Nat -> Nat -> ListNat
enumFromTo n m = if n == m
                  then m : []
                  else if gt n m
                    then m : (enumFromTo n (S m))
                    else n : (enumFromTo (S n) m)

enumTo :: Nat -> ListNat
enumTo = enumFromTo O

take :: Nat -> ListNat -> ListNat
take (S m) (n : ns) = n : (take m ns)
take _ _ = []

drop :: Nat -> ListNat -> ListNat
drop (S m) (n : ns) = drop m ns
drop _ ns = ns

elemIndices :: Nat -> ListNat -> ListNat
elemIndices m n = elemIndices' m n O
  where 
    elemIndices' :: Nat -> ListNat -> Nat -> ListNat
    elemIndices' _ [] _ = []
    elemIndices' m (n : ns) i = if m == n
      then i : elemIndices' m ns (S i)
      else elemIndices' m ns (S i)

-- mix [1,2,3,4] [100,200,300,400] = [1,100,2,200,3,300,4,400]
-- intersperse 8 [1,2,3,4] = [1,8,2,8,3,8,4]
-- init [1,2,3,4] = [1,2,3]
-- tail [1,2,3,4] = [2,3,4]
-- elemIndices 42 [7,8,35,42,41,42,42] = [3,5,6]