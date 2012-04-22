{-# LANGUAGE 
    ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  #-}
module Data.Unfolder 
  (
    Unfolder(..)
  , chooseDefault
  
  , boundedEnum
  
  , Left(..)
  , Right(..)
  , Random(..)
  , FromList(..)

  , BFS(..)
  , runBFS
  , packBFS
  
  ) 
  where 

import Control.Applicative
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Compose
import Data.Functor.Reverse
import Control.Monad.Trans.State
import qualified System.Random as R
import Data.Foldable (foldMap, asum)
import Data.Maybe (catMaybes)

-- | Unfolders provide a way to unfold data structures. The minimal implementation is 'choose'.
class Applicative f => Unfolder f where
  choose :: [f x] -> f x
  chooseInt :: Int -> f Int
  chooseInt n = choose $ map pure [0 .. n - 1]

-- | If an unfolder is monadic, 'choose' can be implemented in terms of 'chooseInt'.
chooseDefault :: (Monad m, Unfolder m) => [m x] -> m x
chooseDefault ms = chooseInt (length ms) >>= (ms !!)

-- | If a datatype is bounded and enumerable, we can use 'chooseInt' to generate a value.
boundedEnum :: forall f a. (Unfolder f, Bounded a, Enum a) => f a
boundedEnum = (\x -> toEnum (x + lb)) <$> chooseInt (1 + ub - lb)
  where
    lb = fromEnum (minBound :: a)
    ub = fromEnum (maxBound :: a)

newtype Left x = L { getL :: Identity x } deriving (Functor, Applicative, Monad)
-- | Always choose the first item.
instance Unfolder Left where
  choose = head
  chooseInt _ = pure 0

newtype Right x = R { getR :: Identity x } deriving (Functor, Applicative, Monad)
-- | Always choose the last item.
instance Unfolder Right where
  choose = last
  chooseInt n = pure (n - 1)

-- | Don't choose but return all items.
instance Unfolder [] where
  chooseInt n = [0 .. n - 1]
  choose = concat

fstP :: Product p q a -> p a
fstP (Pair p _) = p

sndP :: Product p q a -> q a
sndP (Pair _ q) = q

instance (Unfolder p, Unfolder q) => Unfolder (Product p q) where
  chooseInt n = Pair (chooseInt n) (chooseInt n)
  choose ps = Pair (choose $ map fstP ps) (choose $ map sndP ps)

instance (Unfolder p, Applicative q) => Unfolder (Compose p q) where
  chooseInt n = Compose $ pure <$> chooseInt n
  choose = Compose . choose . map getCompose

instance Unfolder m => Unfolder (Reverse m) where
  chooseInt n = Reverse $ (\x -> n - 1 - x) <$> chooseInt n
  choose = Reverse . choose . reverse . map getReverse
  
newtype Random g m a = Random { getRandom :: StateT g m a } 
  deriving (Functor, Applicative, Monad)
-- | Choose randomly
instance (Functor m, Monad m, R.RandomGen g) => Unfolder (Random g m) where
  choose = chooseDefault
  chooseInt n = Random . StateT $ return . R.randomR (0, n - 1)
  
newtype FromList a x = FromList { getFromList :: StateT [a] [] x } 
  deriving (Functor, Applicative, Alternative, Monad)
instance Unfolder (FromList a) where
  choose ms = FromList . StateT $ \as -> foldMap (flip runStateT as . getFromList) ms


newtype BFS f x = BFS { getBFS :: Int -> Maybe (f x) }

instance Functor f => Functor (BFS f) where 
  fmap f = BFS . (fmap (fmap f) .) . getBFS

instance Alternative f => Applicative (BFS f) where
  pure = packBFS . pure
  BFS ff <*> BFS fx = BFS $ \d -> flattenBFS asum $
    [ (<*>) <$> ff i <*> fx d | i <- [0 .. d - 1] ] ++
    [ (<*>) <$> ff d <*> fx i | i <- [0 .. d] ]

instance (Alternative f, Unfolder f) => Unfolder (BFS f) where
  choose ms = BFS $ \d -> if d == 0 
    then Just empty
    else flattenBFS choose (map (`getBFS` (d - 1)) ms)

runBFS :: Alternative f => BFS f x -> f x
runBFS (BFS f) = loop 0 where loop d = maybe empty (<|> loop (d + 1)) (f d)

packBFS :: f x -> BFS f x
packBFS r = BFS $ \d -> if d == 0 then Just r else Nothing

flattenBFS :: ([a] -> a) -> [Maybe a] -> Maybe a
flattenBFS f ms = case catMaybes ms of
  [] -> Nothing
  ms' -> Just (f ms')
