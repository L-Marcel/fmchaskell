{-# LANGUAGE GADTs, OverloadedLists, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use concatMap" #-}

module List where
import Prelude hiding (Right, Left, (++), concat, zip, dropWhile, takeWhile, reverse, fold, map, (<=), (>=), compare, replicate, filter, all, any)

import DataTypes
import ListNat hiding ((++), expNat, mulNat, reverse, append)
import Bool
import Ordering

(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
(y : ys) ++ xs = y : (ys ++ xs)

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

fold :: (a -> a -> a) -> a -> [a] -> a
fold _ x [] = x
fold f x (y : ys) = f y (fold f x ys)

all :: (a -> Bool) -> [a] -> Bool
all f xs = fold (&&) True (map f xs)
-- all _ [] = True
-- all f (x : xs) = f x && all f xs

any :: (a -> Bool) -> [a] -> Bool
any f xs = fold (||) False (map f xs)
-- any _ [] = False
-- any f (x : xs) = f x || any f xs

pw :: (a -> b -> c) -> [a]-> [b] -> [c]
pw f (x : xs) (y : ys) = f x y : pw f xs ys
pw _ _ _ = []

zip :: [a] -> [b] -> [(a, b)]
zip = pw (,)

append :: a -> [a] -> [a]
append x [] = [x]
append x (y : ys) = y : append x ys

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = append x (reverse xs)

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

-- addNat :: Nat -> [Nat] -> [Nat]
-- addNat m = map (m +)

-- mulNat :: Nat -> [Nat] -> [Nat]
-- mulNat m = map (m *)

-- expNat :: Nat -> [Nat] -> [Nat]
-- expNat m = map (^ m)

concat :: [[a]] -> [a]
concat [] = []
concat ([] : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

type ShowAt = Int
data InfinitySetOrientation = Left Int | Center | Right Int;
data Set where
  FiniteSet :: [Int] -> Set
  InfinitySet :: (Int -> Int) -> InfinitySetOrientation -> ShowAt -> Set

instance (Show Set) where
  show (FiniteSet is) = show is
  show (InfinitySet f Center 0) = "[...]" 
  show (InfinitySet f Center at) 
    = "[..., " ++ concat (map ((++ ", ").show.f) [-at+1 .. at-1]) ++ "...]"
  show (InfinitySet f (Right s) at)
    = "[" ++ concat (map ((++ ", ").show.f) [s .. s+at-1]) ++ "...]"
  show (InfinitySet f (Left s) at)
    = "[..." ++ concat (map ((", " ++).show.f) [s-at+1 .. s]) ++ "]"
