import Control.Applicative
import Data.Unfoldable
import Data.Unfolder

import Data.Maybe
import System.Random


data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Unfoldable Tree where
  unfold fa = choose
    [ pure Empty
    , Leaf <$> fa
    , Node <$> unfold fa <*> fa <*> unfold fa
    ]
    
tree7 :: Tree Int
tree7 = fromJust $ fromList [0..6]

treeShapes :: [Tree ()]
treeShapes = take 10 unfoldBF_

randomTree :: IO (Tree Bool)
randomTree = getStdRandom randomDefault
