import Control.Applicative
import Data.Unfoldable
import Data.Unfolder

import Data.Maybe
import System.Random


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
randomBTree = getStdRandom randomValue
