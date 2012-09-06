{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Control.Applicative
import Data.Unfoldable
import Data.Unfolder

import Data.Maybe
import System.Random


data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Generic1)

instance Unfoldable Tree
  -- where
  --   unfold fa = choose
  --     [ pure Empty
  --     , Node <$> unfold fa <*> fa <*> unfold fa
  --     ]
    
tree7 :: Tree Int
tree7 = fromJust $ fromList [0..6]

treeShapes :: [Tree ()]
treeShapes = take 20 unfoldBF_

treeShapes' :: [Tree ()]
treeShapes' = take 20 $ bfsBySum unfold_

randomTree :: IO (Tree Bool)
randomTree = getStdRandom randomDefault
