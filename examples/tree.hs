{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics
import Data.Unfoldable
import Data.Unfolder

import Data.Maybe
import System.Random


data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Generic1, Unfoldable)

tree7 :: Tree Int
tree7 = fromJust $ fromList [0..6]

treeShapes :: [Tree ()]
treeShapes = take 20 unfoldBF_

treeShapes' :: [Tree ()]
treeShapes' = take 20 $ bfsBySum unfold_

data Pair a = Pair a a
  deriving (Show, Functor, Generic1, Unfoldable)
data PerfectTree a = Leaf a | Branch (PerfectTree (Pair a))
  deriving (Show, Functor, Generic1, Unfoldable)

ptreeShapes :: [PerfectTree ()]
ptreeShapes = take 5 unfoldBF_
