{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics
import Data.Unfoldable
import Data.Unfolder

import Data.Maybe
import Test.QuickCheck.Gen (sample, resize, Gen)


data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Generic1, Foldable, Unfoldable)

tree7 :: Tree Int
tree7 = fromJust $ fromList [0..6]

treeShapes :: [Tree ()]
treeShapes = take 20 unfoldBF_

treeShapes' :: [Tree ()]
treeShapes' = take 20 $ bfsBySum unfold_

arbitraryTrees :: Int -> IO ()
arbitraryTrees size = sample (resize size arbitraryDefault :: Gen (Tree ()))

data Pair a = Pair a a
  deriving (Show, Functor, Foldable, Generic1, Unfoldable)
data PerfectTree a = Leaf a | Branch (PerfectTree (Pair a))
  deriving (Show, Functor, Foldable, Generic1, Unfoldable)

ptreeShapes :: [PerfectTree ()]
ptreeShapes = take 5 unfoldBF_
