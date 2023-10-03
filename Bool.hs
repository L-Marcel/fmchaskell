module Bool where

import Prelude hiding (not)
import DataTypes

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