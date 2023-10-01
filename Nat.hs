{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Use map" #-}

module Nat where

import Prelude
  hiding (last, (<), (>), (>=), (<=), compare, init, isPrefixOf, maximum, minimum, drop, take, enumFromTo, not, reverse, (++), product, sum, elem, length, (+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem)
import System.Win32.DebugApi (DebugEventInfo(Exception))

data Nat = O | S Nat
  deriving (Eq)

instance (Show Nat) where
  show O = "O"
  show (S n) = 'S': show n

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
m ^ O = S O
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
fact O = S O
fact (S m) = S m * fact m

-- fibonacci
fib :: Nat -> Nat
fib O = O
fib (S O) = S O
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
rem m n = rem' m (n * quot m n)
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

-- Inequality ----------

compare :: Nat -> Nat -> Ordering
compare O O = EQ
compare _ O = GT
compare O _ = LT
compare (S n) (S m) = compare n m

(<) :: Nat -> Nat -> Bool
n < m = compare n m == LT

(>) :: Nat -> Nat -> Bool
n > m = compare n m == GT

(>=) :: Nat -> Nat -> Bool
n >= m = n > m || n == m 

(<=) :: Nat -> Nat -> Bool
n <= m = n < m || n == m

-- ListNat --------------

type ListNat = [Nat]

length :: ListNat -> Nat
length [] = O
length (n : ns) = S (length ns)

elem :: Nat -> ListNat -> Bool
elem _ [] = False
elem m (n : ns) = n == m || elem m ns

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
addNat m (n : ns) = (m + n) : addNat m ns

mulNat :: Nat -> ListNat -> ListNat
mulNat m [] = []
mulNat m (n : ns) = (m * n) : mulNat m ns

expNat :: Nat -> ListNat -> ListNat
expNat m [] = []
expNat m (n : ns) = (n ^ m) : expNat m ns

--fix
enumFromTo :: Nat -> Nat -> ListNat
enumFromTo n m
  | n == m = [m]
  | n > m = m : enumFromTo n (S m)
  | otherwise = n : enumFromTo (S n) m

enumTo :: Nat -> ListNat
enumTo = enumFromTo O

take :: Nat -> ListNat -> ListNat
take (S m) (n : ns) = n : take m ns
take _ _ = []

drop :: Nat -> ListNat -> ListNat
drop (S m) (n : ns) = drop m ns
drop _ ns = ns

elemIndices :: Nat -> ListNat -> ListNat
elemIndices m n = elemIndices' m n O
  where
    elemIndices' :: Nat -> ListNat -> Nat -> ListNat
    elemIndices' _ [] _ = []
    elemIndices' m (n : ns) i =
      if m == n
        then i : elemIndices' m ns (S i)
        else elemIndices' m ns (S i)

pwAdd :: ListNat -> ListNat -> ListNat
pwAdd (n : ns) (m : ms) = (n + m) : pwAdd ns ms
pwAdd _ _ = []

pwMul :: ListNat -> ListNat -> ListNat
pwMul (n : ns) (m : ms) = (n * m) : pwMul ns ms
pwMul _ _ = []

isSorted :: ListNat -> Bool
isSorted (n : (m : ms)) = n <= m && isSorted (m : ms)
isSorted _ = True

filterEven :: ListNat -> ListNat
filterEven [] = []
filterEven (n : ns) =
  if ev n
    then n : filterEven ns
    else filterEven ns

filterOdd :: ListNat -> ListNat
filterOdd [] = []
filterOdd (n : ns) =
  if od n
    then n : filterOdd ns
    else filterOdd ns

-- bonus
decrease :: ListNat -> ListNat
decrease [] = []
decrease (O : ns) = O : decrease ns
decrease ((S n) : ns) = n : decrease ns

minimum :: ListNat -> Nat
minimum [] = error "Empty list has no minimum."
minimum [n] = n
minimum (n : ns) = min n (minimum ns)

maximum :: ListNat -> Nat
maximum [] = error "Empty list has no maximum."
maximum [n] = n
maximum (n : ns) = max n (maximum ns)

isPrefixOf :: ListNat -> ListNat -> Bool 
isPrefixOf [] _ = True
isPrefixOf (n : ns) (m : ms) = n == m && isPrefixOf ns ms
isPrefixOf _ _ = False

mix :: ListNat -> ListNat -> ListNat
mix [] ms = ms
mix ns [] = ns 
mix (n : ns) (m : ms) = n : (m : mix ns ms)

intersperse :: Nat -> ListNat -> ListNat
intersperse n [] = []
intersperse _ [m] = [m]
intersperse n (m : ms) = m : (n : intersperse n ms)

head :: ListNat -> Nat
head [] = error "Empty list has no head."
head (n : _) = n

tail :: ListNat -> ListNat
tail [] = error "Empty list has no tail."
tail (_ : ns) = ns

init :: ListNat -> ListNat
init [] = error "Empty list has no init."
init [n] = []
init (n : ns) = n : init ns

last :: ListNat -> Nat
last [] = error "Empty list has no last."
last [n] = n
last (_ : ns) = last ns

-- Int ------------------