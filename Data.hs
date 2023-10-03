module Data where 

data Nat = O | S Nat
  deriving (Eq)

instance (Show Nat) where
  show O = "O"
  show (S n) = 'S': show n