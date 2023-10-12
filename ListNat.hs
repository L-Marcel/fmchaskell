{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# LANGUAGE GADTs, OverloadedLists, TypeFamilies #-}

module ListNat where

import Prelude
  hiding ((<=), (>), last, init, max, maximum, minimum, min, drop, take, enumFromTo, (^), reverse, (++), (*), product, (+), sum, length, elem)

import DataTypes
import Nat
import Bool
import Ordering ((>), (<=))

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
drop (S m) (_ : ns) = drop m ns
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

remove :: Nat -> ListNat -> ListNat
remove n [] = []
remove n (m : ms)
  | n == m = ms
  | otherwise = m : (remove n ms)

removeAll :: Nat -> ListNat -> ListNat
removeAll n [] = []
removeAll n (m : ms)
  | n == m = removeAll n ms
  | otherwise = m : (removeAll n ms)

sort :: ListNat -> ListNat
sort [] = []
sort [n] = [n]
sort ns = (minimum ns) : sort (remove (minimum ns) ns)

filterUnique :: ListNat -> ListNat
filterUnique [] = []
filterUnique (n : ns) = n : filterUnique (removeAll n ns)