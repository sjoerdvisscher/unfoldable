import Control.Applicative
import Data.Unfoldable

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


data TB a = LB a | BB (TB (a, a)) deriving Show

instance Unfoldable TB where
  unfold fa = choose
    [ LB <$> fa
    , BB <$> unfold ((,) <$> fa <*> fa)
    ]

btree8 :: TB Int
btree8 = fromJust $ fromList [0..7]

btreeShapes :: [TB ()]
btreeShapes = take 5 unfold_

randomBTree :: IO (TB Bool)
randomBTree = getStdRandom randomDefault
