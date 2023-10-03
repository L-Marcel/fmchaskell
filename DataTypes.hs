-- {-# LANGUAGE GADTs, OverloadedLists, TypeFamilies #-}

module DataTypes where
import GHC.Exts (IsList(..))

data Nat = O | S Nat
  deriving (Eq)

instance (Show Nat) where
  show O = "O"
  show (S n) = 'S': show n

--data ListNat = Empty | Cons Nat ListNat
--  deriving (Eq, Show)

type ListNat = [Nat]

-- instance IsList ListNat where
--  type Item ListNat = Nat
--  fromList [] = Empty
--  fromList (n : ns) = Cons n (fromList ns)
--  toList Empty = []
--  toList (Cons n ns) = n : toList ns