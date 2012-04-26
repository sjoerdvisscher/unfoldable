-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unfoldable
-- Copyright   :  (c) Sjoerd Visscher 2012
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Class of data structures that can be unfolded.
-----------------------------------------------------------------------------
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
import Control.Monad.Trans.State
import qualified System.Random as R
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..))
import Data.Maybe

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
class Unfoldable t where
  -- | Given a way to generate elements, return a way to generate structures containing those elements.
  unfold :: Unfolder f => f a -> f (t a)

-- | Unfold the structure, always using @()@ as elements.
unfold_ :: (Unfoldable t, Unfolder f) => f (t ())
unfold_ = unfold (pure ())

-- | Breadth-first unfold, which orders the result by the number of 'choose' calls.
unfoldBF :: (Unfoldable t, Unfolder f) => f a -> f (t a)
unfoldBF = runBFS . unfold . packBFS

-- | Unfold the structure breadth-first, always using @()@ as elements.
unfoldBF_ :: (Unfoldable t, Unfolder f) => f (t ())
unfoldBF_ = unfoldBF (pure ())

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

-- | Generate all the values depth first.
allDepthFirst :: Unfoldable t => [t ()]
allDepthFirst = unfold_

-- | Generate all the values breadth first.
allBreadthFirst :: Unfoldable t => [t ()]
allBreadthFirst = unfoldBF_

-- | Generate a random value, can be used as default instance for 'R.Random'.
randomDefault :: (R.Random a, R.RandomGen g, Unfoldable t) => g -> (t a, g)
randomDefault = runState . getRandom . unfold . Random . state $ R.random

-- | Provides a QuickCheck generator, can be used as default instance for 'Arbitrary'.
arbitraryDefault :: (Arbitrary a, Unfoldable t) => Gen (t a)
arbitraryDefault = MkGen $ \r n -> let Arb _ f = unfold arbUnit in 
  fromMaybe (error "Failed to generate a value.") (f r (n + 1))

instance Unfoldable [] where
  unfold f = choose 
    [ pure []
    , (:) <$> f <*> unfold f
    ]

instance Unfoldable Maybe where
  unfold f = choose 
    [ pure Nothing
    , Just <$> f
    ]

instance (Bounded a, Enum a) => Unfoldable (Either a) where
  unfold f = choose 
    [ Left <$> boundedEnum
    , Right <$> f
    ]

instance (Bounded a, Enum a) => Unfoldable ((,) a) where
  unfold f = (,) <$> boundedEnum <*> f

instance Unfoldable Identity where
  unfold = fmap Identity

instance (Bounded a, Enum a) => Unfoldable (Constant a) where
  unfold = fmap Constant . const boundedEnum
  
instance (Unfoldable p, Unfoldable q) => Unfoldable (Product p q) where
  unfold f = Pair <$> unfold f <*> unfold f

instance (Unfoldable p, Unfoldable q) => Unfoldable (Compose p q) where
  unfold = fmap Compose . unfold . unfold

instance Unfoldable f => Unfoldable (Reverse f) where
  unfold = fmap Reverse . getDualA . unfold . DualA