-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unfoldable
-- Copyright   :  (c) Sjoerd Visscher 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Class of data structures that can be unfolded.
-----------------------------------------------------------------------------
{-# LANGUAGE CPP, Safe, TupleSections #-}
#ifdef GENERICS
{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts, TypeApplications #-}
#endif
module Data.Unfoldable
  (

  -- * Unfoldable
    Unfoldable(..)
  , unfold_
  , unfoldBF
  , unfoldBF_

  -- ** Specific unfolds
  , unfoldr
  , fromList
  , leftMost
  , rightMost
  , allDepthFirst
  , allToDepth
  , allBreadthFirst
  , randomDefault
  , arbitraryDefault

  )
  where

import Control.Applicative
import Data.Unfolder
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor.Sum
import Control.Monad.Trans.State
import qualified System.Random as R
import Test.QuickCheck (Arbitrary(..), Gen, sized, resize)
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Tree as T

#ifdef GENERICS
import GHC.Generics
import Generics.OneLiner
#endif

-- | Data structures that can be unfolded.
--
-- For example, given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Unfoldable Tree where
-- >   unfold fa = choose
-- >     [ pure Empty
-- >     , Leaf <$> fa
-- >     , Node <$> unfold fa <*> fa <*> unfold fa
-- >     ]
--
-- i.e. it follows closely the instance for 'Traversable', but instead of matching on an input value,
-- we 'choose' from a list of all cases.
--
-- Instead of manually writing the `Unfoldable` instance, you can add a @deriving@ `Generic1`
-- to your datatype and declare an `Unfoldable` instance without giving a definition for `unfold`.
--
-- For example the previous example can be simplified to just:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import GHC.Generics
-- >
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Generic1
-- >
-- > instance Unfoldable Tree
class Unfoldable t where
  -- | Given a way to generate elements, return a way to generate structures containing those elements.
  unfold :: Unfolder f => f a -> f (t a)

#ifdef GENERICS
  default unfold :: (ADT1 t, Constraints1 t Unfoldable, Unfolder f) => f a -> f (t a)
  unfold = choose . getCompose . createA1 @Unfoldable (Compose . return . unfold . foldr (<|>) empty . getCompose) . Compose . return
  {-# INLINE unfold #-}
#endif

-- | Unfold the structure, always using @()@ as elements.
unfold_ :: (Unfoldable t, Unfolder f) => f (t ())
unfold_ = unfold (pure ())

-- | Breadth-first unfold, which orders the result by the number of 'choose' calls.
unfoldBF :: (Unfoldable t, Unfolder f) => f a -> f (t a)
unfoldBF = ala bfs unfold

-- | Unfold the structure breadth-first, always using @()@ as elements.
unfoldBF_ :: (Unfoldable t, Unfolder f) => f (t ())
unfoldBF_ = bfs unfold_

-- | @unfoldr@ builds a data structure from a seed value. It can be specified as:
--
-- > unfoldr f z == fromList (Data.List.unfoldr f z)
unfoldr :: Unfoldable t => (b -> Maybe (a, b)) -> b -> Maybe (t a)
unfoldr f z = terminate . flip runStateT z . unfoldBF . StateT $ maybeToList . f
  where
    terminate [] = Nothing
    terminate ((t, b):ts) = if isNothing (f b) then Just t else terminate ts

-- | Create a data structure using the list as input.
-- This can fail because there might not be a data structure with the same number
-- of element positions as the number of elements in the list.
fromList :: Unfoldable t => [a] -> Maybe (t a)
fromList = unfoldr uncons
  where
    uncons [] = Nothing
    uncons (a:as) = Just (a, as)

-- | Always choose the first constructor.
leftMost :: Unfoldable t => Maybe (t ())
leftMost = unfold_

-- | Always choose the last constructor.
rightMost :: Unfoldable t => Maybe (t ())
rightMost = getDualA unfold_

-- | Generate all the values depth-first.
allDepthFirst :: Unfoldable t => [t ()]
allDepthFirst = unfold_

-- | Generate all the values upto a given depth, depth-first.
allToDepth :: Unfoldable t => Int -> [t ()]
allToDepth d = limitDepth d unfold_

-- | Generate all the values breadth-first.
allBreadthFirst :: Unfoldable t => [t ()]
allBreadthFirst = unfoldBF_

-- | Generate a random value, can be used as default instance for 'R.Random'.
randomDefault :: (R.Random a, R.RandomGen g, Unfoldable t) => g -> (t a, g)
randomDefault = runState . getRandom . unfold . Random . state $ R.random

-- | Provides a QuickCheck generator, can be used as default instance for 'Arbitrary'.
arbitraryDefault :: (Arbitrary a, Unfoldable t) => Gen (t a)
arbitraryDefault = let Arb _ gen = unfold arbUnit in
  fromMaybe (error "Failed to generate a value.") <$> sized (\n -> resize (n + 1) gen)

instance Unfoldable [] where
  unfold fa = go where
    go = choose
      [ pure []
      , (:) <$> fa <*> go ]

instance Unfoldable Maybe where
  unfold fa = choose
    [ pure Nothing
    , Just <$> fa
    ]

instance (Bounded a, Enum a) => Unfoldable (Either a) where
  unfold fa = choose
    [ Left <$> boundedEnum
    , Right <$> fa
    ]

instance (Bounded a, Enum a) => Unfoldable ((,) a) where
  unfold fa = choose
    [ (,) <$> boundedEnum <*> fa ]

instance Unfoldable Identity where
  unfold fa = choose
    [ Identity <$> fa ]

instance (Bounded a, Enum a) => Unfoldable (Constant a) where
  unfold _ = choose
    [ Constant <$> boundedEnum ]

instance (Unfoldable p, Unfoldable q) => Unfoldable (Product p q) where
  unfold fa = choose
    [ Pair <$> unfold fa <*> unfold fa ]

instance (Unfoldable p, Unfoldable q) => Unfoldable (Sum p q) where
  unfold fa = choose
    [ InL <$> unfold fa
    , InR <$> unfold fa
    ]

instance (Unfoldable p, Unfoldable q) => Unfoldable (Compose p q) where
  unfold fa = choose
    [ Compose <$> unfold (unfold fa) ]

instance Unfoldable f => Unfoldable (Reverse f) where
  unfold fa = choose
    [ Reverse <$> getDualA (unfold (DualA fa)) ]

instance Unfoldable S.Seq where
  unfold fa = go where
    go = choose
      [ pure empty
      , (S.<|) <$> fa <*> go ]

instance Unfoldable T.Tree where
  unfold fa = go where
    go = choose [ T.Node <$> fa <*> unfold go ]
