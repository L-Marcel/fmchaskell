{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}

module List where
import Prelude hiding (dropWhile, takeWhile, (*), (+), reverse, fold, map, (<=), (>=), compare, replicate, filter, all, any)

import DataTypes
import Nat
import Bool
import Ordering

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

replicate :: Nat -> a -> [a]
replicate O _ = []
replicate (S n) x = x : replicate n x

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs)
  | f x = x : filter f xs
  | otherwise = filter f xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x : xs) = f x && all f xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x : xs) = f x || any f xs

pw :: (a -> a -> a) -> [a] -> [a] -> [a]
pw f (x : xs) (y : ys) = f x y : pw f xs ys
pw _ _ _ = []

fold :: (a -> a -> a) -> a -> [a] -> a
fold _ x [] = x
fold f x (y : ys) = f y (fold f x ys)

-- append :: a -> [a] -> [a]
-- append x ys = fold (:) [x] ys
-- append x [] = [x]
-- append x (y : ys) = y : (append x ys)

-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x : xs) = reverse xs : [x]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x : xs)
  | f x = x : takeWhile f xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x : xs)
  | f x = dropWhile f xs
  | otherwise = x : xs