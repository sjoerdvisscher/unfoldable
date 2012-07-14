-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Biunfoldable
-- Copyright   :  (c) Sjoerd Visscher 2012
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Class of data structures with 2 type arguments that can be unfolded.
-----------------------------------------------------------------------------
{-# LANGUAGE Safe #-}
module Data.Biunfoldable 
  (

  -- * Biunfoldable
    Biunfoldable(..)
  , biunfold_
  , biunfoldBF
  , biunfoldBF_

  -- ** Specific unfolds
  , biunfoldr
  , fromLists
  , randomDefault
  , arbitraryDefault

  ) 
  where

import Control.Applicative
import Data.Unfolder
import Data.Functor.Constant
import Control.Monad.Trans.State
import qualified System.Random as R
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..))
import Data.Maybe

-- | Data structures with 2 type arguments (kind @* -> * -> *@) that can be unfolded.
--
-- For example, given a data type
--
-- > data Tree a b = Empty | Leaf a | Node (Tree a b) b (Tree a b)
--
-- a suitable instance would be
--
-- > instance Biunfoldable Tree where
-- >   biunfold fa fb = choose
-- >     [ pure Empty
-- >     , Leaf <$> fa
-- >     , Node <$> biunfold fa fb <*> fb <*> biunfold fa fb
-- >     ]
--
-- i.e. it follows closely the instance for 'Bitraversable', but instead of matching on an input value,
-- we 'choose' from a list of all cases.
class Biunfoldable t where
  -- | Given a way to generate elements, return a way to generate structures containing those elements.
  biunfold :: Unfolder f => f a -> f b -> f (t a b)

-- | Unfold the structure, always using @()@ as elements.
biunfold_ :: (Biunfoldable t, Unfolder f) => f (t () ())
biunfold_ = biunfold (pure ()) (pure ())

-- | Breadth-first unfold, which orders the result by the number of 'choose' calls.
biunfoldBF :: (Biunfoldable t, Unfolder f) => f a -> f b -> f (t a b)
biunfoldBF = ala2 bfs biunfold

-- | Unfold the structure breadth-first, always using @()@ as elements.
biunfoldBF_ :: (Biunfoldable t, Unfolder f) => f (t () ())
biunfoldBF_ = bfs biunfold_

-- | @biunfoldr@ builds a data structure from a seed value.
biunfoldr :: Biunfoldable t => (c -> Maybe (a, c)) -> (c -> Maybe (b, c)) -> c -> Maybe (t a b)
biunfoldr fa fb z = terminate . flip runStateT z $ biunfoldBF (StateT $ maybeToList . fa) (StateT $ maybeToList . fb)
  where
    terminate [] = Nothing
    terminate ((t, c):ts) = if isNothing (fa c) && isNothing (fb c) then Just t else terminate ts

-- | Create a data structure using the lists as input.
-- This can fail because there might not be a data structure with the same number
-- of element positions as the number of elements in the lists.
fromLists :: Biunfoldable t => [a] -> [b] -> Maybe (t a b)
fromLists = curry $ biunfoldr unconsA unconsB
  where
    unconsA ([], _) = Nothing
    unconsA (a:as, bs) = Just (a, (as, bs))
    unconsB (_, []) = Nothing
    unconsB (as, b:bs) = Just (b, (as, bs))

-- | Generate a random value, can be used as default instance for 'R.Random'.
randomDefault :: (R.Random a, R.Random b, R.RandomGen g, Biunfoldable t) => g -> (t a b, g)
randomDefault = runState . getRandom $ biunfold (Random . state $ R.random) (Random . state $ R.random)

-- | Provides a QuickCheck generator, can be used as default instance for 'Arbitrary'.
arbitraryDefault :: (Arbitrary a, Arbitrary b, Biunfoldable t) => Gen (t a b)
arbitraryDefault = MkGen $ \r n -> let Arb _ f = biunfold arbUnit arbUnit in 
  fromMaybe (error "Failed to generate a value.") (f r (n + 1))

instance Biunfoldable Either where
  biunfold fa fb = choose 
    [ Left <$> fa
    , Right <$> fb
    ]

instance Biunfoldable (,) where
  biunfold fa fb = choose 
    [ (,) <$> fa <*> fb ]

instance Biunfoldable Constant where
  biunfold fa _ = choose 
    [ Constant <$> fa ]
