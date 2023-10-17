module Int where
import Prelude hiding (gcd, abs, div, rem, quot)
import Bool

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