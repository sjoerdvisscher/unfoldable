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
-- They are basically 'Alternative' instances, but the 'choose' method
-- allows the unfolder to do something special for the recursive positions
-- of the data structure.
-----------------------------------------------------------------------------
{-# LANGUAGE 
    ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  #-}
module Data.Unfolder 
  (
  
  -- * Unfolder
    Unfolder(..)
  , chooseMonadDefault
  
  , boundedEnum
  
  -- ** Unfolder instances
  , DualA(..)
  , Random(..)

  , BFS(..)
  , runBFS
  , packBFS
  
  , Arb(..)
  , arbUnit
  
  ) 
  where 

import Control.Applicative
import Control.Monad
import Control.Arrow (ArrowZero, ArrowPlus)

import Data.Functor.Product
import Data.Functor.Compose
import Data.Functor.Reverse
import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import qualified System.Random as R
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..))

import Data.Monoid (Monoid)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Foldable (asum)
import Data.Traversable (traverse)

-- | Unfolders provide a way to unfold data structures.
-- The methods have default implementations in terms of 'Alternative',
-- but you can implement 'choose' to act on recursive positions of the
-- data structure, or simply to provide a faster implementation than 'asum'.
class Alternative f => Unfolder f where
  -- | Choose one of the values from the list.
  choose :: [f x] -> f x
  choose = asum
  -- | Given a number 'n', return a number between '0' and 'n - 1'.
  chooseInt :: Int -> f Int
  chooseInt n = choose $ map pure [0 .. n - 1]

-- | If an unfolder is monadic, 'choose' can be implemented in terms of 'chooseInt'.
chooseMonadDefault :: (Monad m, Unfolder m) => [m x] -> m x
chooseMonadDefault ms = chooseInt (length ms) >>= (ms !!)

-- | If a datatype is bounded and enumerable, we can use 'chooseInt' to generate a value.
boundedEnum :: forall f a. (Unfolder f, Bounded a, Enum a) => f a
boundedEnum = (\x -> toEnum (x + lb)) <$> chooseInt (1 + ub - lb)
  where
    lb = fromEnum (minBound :: a)
    ub = fromEnum (maxBound :: a)

-- | Derived instance.
instance MonadPlus m => Unfolder (WrappedMonad m)

-- | Derived instance.
instance (ArrowZero a, ArrowPlus a) => Unfolder (WrappedArrow a b)

-- | Don't choose but return all items.
instance Unfolder [] where
  choose = concat
  chooseInt n = [0 .. n - 1]

-- | Always choose the first item.
instance Unfolder Maybe where
  choose [] = Nothing
  choose ms = head ms
  chooseInt 0 = Nothing
  chooseInt _ = Just 0

-- | 'DualA' flips the '(<|>)' operator.
newtype DualA f a = DualA { getDualA :: f a }
  deriving (Functor, Applicative)
instance Alternative f => Alternative (DualA f) where
  empty = DualA empty
  DualA a <|> DualA b = DualA (b <|> a)
-- | Reverse the list passed to choose.
instance Unfolder f => Unfolder (DualA f) where
  choose = DualA . choose . reverse . map getDualA
  chooseInt n = DualA $ (\x -> n - 1 - x) <$> chooseInt n

fstP :: Product p q a -> p a
fstP (Pair p _) = p

sndP :: Product p q a -> q a
sndP (Pair _ q) = q

-- | Derived instance.
instance (Unfolder p, Unfolder q) => Unfolder (Product p q) where
  choose ps = Pair (choose $ map fstP ps) (choose $ map sndP ps)
  chooseInt n = Pair (chooseInt n) (chooseInt n)

-- | Derived instance.
instance (Unfolder p, Applicative q) => Unfolder (Compose p q) where
  choose = Compose . choose . map getCompose
  chooseInt n = Compose $ pure <$> chooseInt n

-- | Derived instance.
instance Unfolder f => Unfolder (Reverse f) where
  choose = Reverse . choose . map getReverse
  chooseInt n = Reverse $ chooseInt n

-- | Derived instance.
instance Unfolder f => Unfolder (Backwards f) where
  choose = Backwards . choose . map forwards
  chooseInt n = Backwards $ chooseInt n

-- | Derived instance.
instance Unfolder f => Unfolder (Lift f)

-- | Derived instance.
instance (Functor m, Monad m, Error e) => Unfolder (ErrorT e m)

-- | Derived instance.
instance Applicative f => Unfolder (ListT f) where
  choose ms = ListT $ concat <$> traverse runListT ms
  chooseInt n = ListT $ pure [0 .. n - 1]

-- | Derived instance.
instance (Functor m, Monad m) => Unfolder (MaybeT m) where
  choose ms = MaybeT $ fmap (listToMaybe . catMaybes) (mapM runMaybeT ms)
  chooseInt 0 = MaybeT $ return Nothing
  chooseInt _ = MaybeT $ return (Just 0)
  
-- | Derived instance.
instance (Monoid w, MonadPlus m, Unfolder m) => Unfolder (RWST r w s m) where
  choose ms = RWST $ \r s -> choose $ map (\m -> runRWST m r s) ms

-- | Derived instance.
instance (MonadPlus m, Unfolder m) => Unfolder (StateT s m) where
  choose ms = StateT $ \s -> choose $ map (`runStateT` s) ms

-- | Derived instance.
instance Unfolder m => Unfolder (ReaderT r m) where
  choose ms = ReaderT $ \r -> choose $ map (`runReaderT` r) ms
  
-- | Derived instance.
instance (Monoid w, Unfolder m) => Unfolder (WriterT w m) where
  choose = WriterT . choose . map runWriterT



newtype Random g m a = Random { getRandom :: StateT g m a } 
  deriving (Functor, Applicative, Monad)
instance (Functor m, Monad m, R.RandomGen g) => Alternative (Random g m) where
  empty = choose []
  a <|> b = choose [a, b]
instance (Functor m, Monad m, R.RandomGen g) => MonadPlus (Random g m) where
  mzero = choose []
  mplus a b = choose [a, b]
-- | Choose randomly.
instance (Functor m, Monad m, R.RandomGen g) => Unfolder (Random g m) where
  choose = chooseMonadDefault
  chooseInt 0 = Random . StateT $ const (fail "Random chooseInt 0")
  chooseInt n = Random . StateT $ return . R.randomR (0, n - 1)


-- | Return a generator of values of a given depth.
-- Returns 'Nothing' if there are no values of that depth or deeper.
-- The depth is the number of 'choose' calls.
newtype BFS f x = BFS { getBFS :: Int -> Maybe [f x] }

instance Functor f => Functor (BFS f) where 
  fmap f = BFS . (fmap (map (fmap f)) .) . getBFS

instance Applicative f => Applicative (BFS f) where
  pure = packBFS . pure
  BFS ff <*> BFS fx = BFS $ \d -> flattenBFS $
    [ liftA2 (liftA2 (<*>)) (ff i) (fx d) | i <- [0 .. d - 1] ] ++
    [ liftA2 (liftA2 (<*>)) (ff d) (fx i) | i <- [0 .. d] ]

instance Applicative f => Alternative (BFS f) where
  empty = BFS $ \d -> if d == 0 then Just [] else Nothing
  BFS fa <|> BFS fb = BFS $ \d -> flattenBFS [fa d, fb d]
  
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


-- | A variant of Test.QuickCheck.Gen, with failure 
-- and a count of the number of recursive positions.
data Arb a = Arb Int (R.StdGen -> Int -> Maybe a)

instance Functor Arb where
  fmap f (Arb i g) = Arb i $ fmap (fmap (fmap f)) g

instance Applicative Arb where
  pure = Arb 0 . pure . pure . pure
  Arb i1 ff <*> Arb i2 fx = Arb (i1 + i2) $
    \r -> let (r1, r2) = R.split r in liftA2 (<*>) (ff r1) (fx r2)

instance Alternative Arb where
  empty = Arb 0 (\_ _ -> Nothing)
  Arb ia fa <|> Arb ib fb = Arb ((ia + ib + 1) `div` 2) $
    \r n -> let (r1, r2) = R.split r in flattenArb r1 [fa r2 n, fb r2 n]

-- | Limit the depth of the generated data structure by 
-- dividing the given size by the number of recursive positions.
instance Unfolder Arb where
  choose ms = Arb 1 g
    where
      g _ 0 = Nothing
      g r n = let (r1, r2) = R.split r in 
        flattenArb r1 $ map (\(Arb i f) -> f r2 (n `div` max i 1)) ms

flattenArb :: R.StdGen -> [Maybe a] -> Maybe a
flattenArb r ms = case catMaybes ms of
  [] -> Nothing
  ms' -> Just $ ms' !! fst (R.randomR (0, length ms' - 1) r)

arbUnit :: Arbitrary a => Arb a
arbUnit = Arb 0 (\r n -> Just $ unGen arbitrary r n)
