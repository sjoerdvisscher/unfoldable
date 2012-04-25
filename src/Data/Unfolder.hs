-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unfolder
-- Copyright   :  (c) Sjoerd Visscher 2012
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Unfolders provide a way to unfold data structures.
-- They are applicative functors that can perform a choice.
-- (Which is basically @Alternative@ without @empty@.)
-----------------------------------------------------------------------------
{-# LANGUAGE 
    ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  #-}
module Data.Unfolder 
  (
  
  -- * Unfolder
    Unfolder(..)
  , chooseAltDefault
  , chooseMonadDefault
  
  , boundedEnum
  
  -- ** Unfolder instances
  , Left(..)
  , Right(..)
  , Random(..)

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
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified System.Random as R
import Data.Maybe (catMaybes)

-- | Unfolders provide a way to unfold data structures. The minimal implementation is 'choose'.
class Applicative f => Unfolder f where
  -- | Choose one of the values from the list.
  choose :: [f x] -> f x
  -- | Given a number 'n', return a number between '0' and 'n - 1'.
  chooseInt :: Int -> f Int
  chooseInt n = choose $ map pure [0 .. n - 1]

-- | If an unfolder is an instance of 'Alternative', 'choose' can be implemented in terms of '<|>'.
chooseAltDefault :: (Alternative f, Unfolder f) => [f x] -> f x
chooseAltDefault = foldr (<|>) empty

-- | If an unfolder is monadic, 'choose' can be implemented in terms of 'chooseInt'.
chooseMonadDefault :: (Monad m, Unfolder m) => [m x] -> m x
chooseMonadDefault ms = chooseInt (length ms) >>= (ms !!)

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
  choose = concat
  chooseInt n = [0 .. n - 1]

fstP :: Product p q a -> p a
fstP (Pair p _) = p

sndP :: Product p q a -> q a
sndP (Pair _ q) = q

instance (Unfolder p, Unfolder q) => Unfolder (Product p q) where
  choose ps = Pair (choose $ map fstP ps) (choose $ map sndP ps)
  chooseInt n = Pair (chooseInt n) (chooseInt n)

instance (Unfolder p, Applicative q) => Unfolder (Compose p q) where
  choose = Compose . choose . map getCompose
  chooseInt n = Compose $ pure <$> chooseInt n

instance Unfolder m => Unfolder (Reverse m) where
  choose = Reverse . choose . reverse . map getReverse
  chooseInt n = Reverse $ (\x -> n - 1 - x) <$> chooseInt n
  
instance (Monad m, Unfolder m) => Unfolder (StateT s m) where
  choose ms = StateT $ \as -> choose $ map (`runStateT` as) ms

instance Unfolder m => Unfolder (ContT r m) where
  choose ms = ContT $ \k -> choose $ map (`runContT` k) ms

instance Unfolder m => Unfolder (ReaderT r m) where
  choose ms = ReaderT $ \r -> choose $ map (`runReaderT` r) ms
  
newtype Random g m a = Random { getRandom :: StateT g m a } 
  deriving (Functor, Applicative, Monad)
-- | Choose randomly.
instance (Functor m, Monad m, R.RandomGen g) => Unfolder (Random g m) where
  choose = chooseMonadDefault
  chooseInt n = Random . StateT $ return . R.randomR (0, n - 1)

-- | Return a generator of values of a given depth.
--   Returns 'Nothing' if there are no values of that depth or deeper.
newtype BFS f x = BFS { getBFS :: Int -> Maybe [f x] }

instance Functor f => Functor (BFS f) where 
  fmap f = BFS . (fmap (map (fmap f)) .) . getBFS

instance Applicative f => Applicative (BFS f) where
  pure = packBFS . pure
  BFS ff <*> BFS fx = BFS $ \d -> flattenBFS $
    [ liftA2 (liftA2 (<*>)) (ff i) (fx d) | i <- [0 .. d - 1] ] ++
    [ liftA2 (liftA2 (<*>)) (ff d) (fx i) | i <- [0 .. d] ]

-- | Choose between values of a given depth only.
instance Applicative f => Unfolder (BFS f) where
  choose ms = BFS $ \d -> if d == 0 then Just [] else flattenBFS (map (`getBFS` (d - 1)) ms)

runBFS :: Unfolder f => BFS f x -> f x
runBFS (BFS f) = choose (loop 0) where loop d = maybe [] (++ loop (d + 1)) (f d)

packBFS :: f x -> BFS f x
packBFS r = BFS $ \d -> if d == 0 then Just [r] else Nothing

flattenBFS :: [Maybe [a]] -> Maybe [a]
flattenBFS ms = case catMaybes ms of
  [] -> Nothing
  ms' -> Just (concat ms')