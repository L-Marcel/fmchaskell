module Int where
import Prelude hiding (Just, Nothing, Maybe, mod, gcd, abs, div, rem, quot)
import DataTypes
import Bool
import List

abs :: Int -> Int
abs a
  | a < 0 = -a
  | otherwise = a

div :: Int -> Int -> (Int, Int)
div 0 b = (0, 0)
div a b
  | abs a < abs b = (0, a)
  | (a < 0) == (b < 0) = 
      let (q', r') = div (a - b) b
        in (q' + 1, r')
  | otherwise =
      let (q', r') = div (a + b) b
        in (q' - 1, r')

gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b =
  let r' = rem a b
    in gcd b r'

rem :: Int -> Int -> Int
rem a b = snd (div a b)

quot :: Int -> Int -> Int
quot a b = fst (div a b)

eucgcd' :: Int -> Int -> Int -> (Int, Int, Int)
eucgcd' n a 0 = (a, 1, n)
eucgcd' n a b = 
  let 
    (m', x', y') = eucgcd' n b (rem a b)
    q = quot a b
  in (m', y', x' - (q * y'))

eucgcd :: Int -> Int -> (Int, Int, Int)
eucgcd = eucgcd' 0

-- x ≡ₘ b
mod :: Int -> Module -> Set
mod b (Mod 0) = FiniteSet [b]
mod b (Mod m) = InfiniteSet f' ISCenter 4
  where 
    f' x = m * x + b

coprime :: Int -> Int -> Bool
a `coprime` b = 
  let (m', _, _) = eucgcd a b 
    in m' == 1 

inverse :: Int -> Module -> Maybe Int
inverse a (Mod m)
  | a `coprime` m =
    let (_, _, a') = eucgcd' 0 m a
      in Just a'
  | otherwise = Nothing
      
-- -- ax ≡ₘ b
-- -- ax ≡ₘ mod b m
-- altInverse :: Int -> Int -> Int -> Maybe Int
-- altInverse _ 0 _ = Just 0
-- altInverse _ _ 0 = Nothing
-- altInverse 0 _ _ = Nothing
-- altInverse a b m
--   | rem (mod b m) a == 0 = Just (quot (mod b m) a)
--   | otherwise = Nothing