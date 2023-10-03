module Nat where

data Nat = O | S Nat
  deriving (Eq)

instance (Show Nat) where
  show O = "O"
  show (S O) = "S O"
  show (S n) = ('S' : ' ': '(' : show n) ++ [')']