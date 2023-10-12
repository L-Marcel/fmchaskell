module List where
import Prelude hiding (map, (<=), (>=), compare)

import DataTypes
import Nat
import Bool
import Ordering

natToBool :: Nat -> Bool
natToBool O = False
natToBool _ = True

boolToNat :: Bool -> Nat
boolToNat False = O
boolToNat True = S O

orderingToNat :: Ordering -> Nat
orderingToNat LT = O
orderingToNat EQ = S O
orderingToNat GT = S (S O)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = (f x) : (map f xs)