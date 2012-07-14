-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Triunfoldable
-- Copyright   :  (c) Sjoerd Visscher 2012
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Class of data structures with 3 type arguments that can be unfolded.
-----------------------------------------------------------------------------
{-# LANGUAGE Safe #-}
module Data.Triunfoldable 
  (

  -- * Triunfoldable
    Triunfoldable(..)
  , triunfold_
  , triunfoldBF
  , triunfoldBF_

  -- ** Specific unfolds
  , triunfoldr
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

-- | Data structures with 3 type arguments (kind @* -> * -> * -> *@) that can be unfolded.
--
-- For example, given a data type
--
-- > data Tree a b c = Empty | Leaf a | Node (Tree a b c) b (Tree a b c)
--
-- a suitable instance would be
--
-- > instance Triunfoldable Tree where
-- >   triunfold fa fb fc = choose
-- >     [ pure Empty
-- >     , Leaf <$> fa
-- >     , Node <$> triunfold fa fb fc <*> fb <*> triunfold fa fb fc
-- >     ]
--
-- i.e. it follows closely the instance for 'Biunfoldable', but for 3 type arguments instead of 2.

class Triunfoldable t where
  -- | Given a way to generate elements, return a way to generate structures containing those elements.
  triunfold :: Unfolder f => f a -> f b -> f c -> f (t a b c)

-- | Unfold the structure, always using @()@ as elements.
triunfold_ :: (Triunfoldable t, Unfolder f) => f (t () () ())
triunfold_ = triunfold (pure ()) (pure ()) (pure ())

-- | Breadth-first unfold, which orders the result by the number of 'choose' calls.
triunfoldBF :: (Triunfoldable t, Unfolder f) => f a -> f b -> f c -> f (t a b c)
triunfoldBF = ala3 bfs triunfold

-- | Unfold the structure breadth-first, always using @()@ as elements.
triunfoldBF_ :: (Triunfoldable t, Unfolder f) => f (t () () ())
triunfoldBF_ = bfs triunfold_

-- | @triunfoldr@ builds a data structure from a seed value.
triunfoldr :: Triunfoldable t => (d -> Maybe (a, d)) -> (d -> Maybe (b, d)) -> (d -> Maybe (c, d)) -> d -> Maybe (t a b c)
triunfoldr fa fb fc z = terminate . flip runStateT z $ triunfoldBF (StateT $ maybeToList . fa) (StateT $ maybeToList . fb) (StateT $ maybeToList . fc)
  where
    terminate [] = Nothing
    terminate ((t, d):ts) = if (isNothing (fa d) && isNothing (fb d) && isNothing (fc d)) then Just t else terminate ts


-- | Create a data structure using the lists as input.
-- This can fail because there might not be a data structure with the same number
-- of element positions as the number of elements in the lists.
fromLists :: Triunfoldable t => [a] -> [b] -> [c] -> Maybe (t a b c)
fromLists = curry3 $ triunfoldr unconsA unconsB unconsC
  where
    unconsA ([], _, _) = Nothing
    unconsA (a:as, bs, cs) = Just (a, (as, bs, cs))
    unconsB (_, [], _) = Nothing
    unconsB (as, b:bs, cs) = Just (b, (as, bs, cs))
    unconsC (_, _, []) = Nothing
    unconsC (as, bs, c:cs) = Just (c, (as, bs, cs))

-- | Generate a random value, can be used as default instance for 'R.Random'.
randomDefault :: (R.Random a, R.Random b, R.Random c, R.RandomGen g, Triunfoldable t) => g -> (t a b c, g)
randomDefault = runState . getRandom $ triunfold (Random . state $ R.random) (Random . state $ R.random) (Random . state $ R.random)

-- | Provides a QuickCheck generator, can be used as default instance for 'Arbitrary'.
arbitraryDefault :: (Arbitrary a, Arbitrary b, Arbitrary c, Triunfoldable t) => Gen (t a b c)
arbitraryDefault = MkGen $ \r n -> let Arb _ f = triunfold arbUnit arbUnit arbUnit in 
  fromMaybe (error "Failed to generate a value.") (f r (n + 1))


curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

instance Triunfoldable (,,) where
  triunfold fa fb fc = choose 
    [ (,,) <$> fa <*> fb <*> fc ]

