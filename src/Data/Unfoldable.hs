module Data.Unfoldable 
  (
    Unfolder(..)
  
  , Unfoldable(..)
  , unfold_
  , unfoldBF
  , unfoldBF_
  
  -- ** Specific unfolds
  , leftMost
  , rightMost
  , allDepthFirst
  , allBreadthFirst
  , randomDefault
  , fromList
  
  ) 
  where
    
import Control.Applicative
import Control.Monad
import Data.Unfolder
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import Control.Monad.Trans.State
import qualified System.Random as R
import Data.Maybe (listToMaybe)

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

-- | Unfold the structure, always using '()' as elements.
unfold_ :: (Unfoldable t, Unfolder f) => f (t ())
unfold_ = unfold (pure ())

-- | Breadth-first unfold
unfoldBF :: (Unfoldable t, Unfolder f, Alternative f) => f a -> f (t a)
unfoldBF = runBFS . unfold . packBFS

-- | Unfold the structure breadth-first, always using '()' as elements.
unfoldBF_ :: (Unfoldable t, Unfolder f, Alternative f) => f (t ())
unfoldBF_ = unfoldBF (pure ())

-- | Always choose the first constructor.
leftMost :: Unfoldable t => t ()
leftMost = runIdentity $ getL unfold_

-- | Always choose the last constructor.
rightMost :: Unfoldable t => t ()
rightMost = runIdentity $ getR unfold_

-- | Generate all the values depth first.
allDepthFirst :: Unfoldable t => [t ()]
allDepthFirst = unfold_

-- | Generate all the values breadth first.
allBreadthFirst :: Unfoldable t => [t ()]
allBreadthFirst = unfoldBF_

-- | Generate a random value, can be used as default instance for Random.
randomDefault :: (R.Random a, R.RandomGen g, Unfoldable t) => g -> (t a, g)
randomDefault = runState . getRandom . unfold . Random . state $ R.random

fromList' :: (Unfolder f, MonadPlus f, Unfoldable t) => [a] -> f (t a, [a])
fromList' as = flip runStateT as . unfoldBF . StateT $ uncons
  where
    uncons [] = mzero
    uncons (a:as') = return (a, as')

-- | Create a data structure using the list as input.
--   This can fail because there might not be a data structure with the same number
--   of element positions as the number of elements in the list.
fromList :: Unfoldable t => [a] -> Maybe (t a)
fromList as = listToMaybe [ t | (t, []) <- fromList' as ]

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
  unfold f = Identity <$> f

instance (Bounded a, Enum a) => Unfoldable (Constant a) where
  unfold _ = Constant <$> boundedEnum
  
instance (Unfoldable p, Unfoldable q) => Unfoldable (Product p q) where
  unfold f = Pair <$> unfold f <*> unfold f

instance (Unfoldable p, Unfoldable q) => Unfoldable (Compose p q) where
  unfold f = Compose <$> unfold (unfold f)

instance Unfoldable f => Unfoldable (Reverse f) where
  unfold f = Reverse <$> getReverse (unfold (Reverse f))