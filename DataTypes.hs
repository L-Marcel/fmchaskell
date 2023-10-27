{-# LANGUAGE GADTs, TypeFamilies #-}

module DataTypes where
import GHC.Exts (IsList(..))
import Prelude hiding (Maybe, Nothing, Just)

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

-- data List a = Empty | Cons a (List a)
--  deriving (Eq, Show)

data OneOf a b = First a | Second b;
data Maybe a = Nothing | Just a;

instance (Show a, Show b) => (Show (OneOf a b)) where
  show (First a) = show a
  show (Second b) = show b

instance (Show a) => (Show (Maybe a)) where
  show Nothing = "Nothing"
  show (Just a) = show a

data Module where
  Mod :: Int -> Module
  deriving Eq

instance (Show Module) where
  show (Mod a) = "mod " ++ show a