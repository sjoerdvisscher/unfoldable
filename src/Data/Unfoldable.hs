module Data.Unfoldable (

    Unfoldable(..)

  -- ** Specific unfolds
  , unfold
  , leftMost
  , rightMost

  -- ** Helper functions
  , spread
  , to

  ) where
    
import Control.Applicative
import Control.Monad.Trans.State
import Data.Splittable
import Data.Monoid (Dual(..))
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse

-- | Data structures that can be unfolded.
--
-- For example, given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Unfoldable Tree where
-- >   unfoldMap f = choose
-- >     [ spread $ pure Empty
-- >     , spread $ Leaf <$> to f
-- >     , spread $ Node <$> to (unfoldMap f) <*> to f <*> to (unfoldMap f)
-- >     ]
--
-- i.e. it follows closely the instance for 'Traversable', with the addition of 'choose', 'spread' and 'to'.
-- 
-- The instance can be simplified to:
--
-- > instance Unfoldable Tree where
-- >   unfoldMap f = choose
-- >     [ const Empty
-- >     , Leaf . f
-- >     , spread $ Node <$> to (unfoldMap f) <*> to f <*> to (unfoldMap f)
-- >     ]
class Unfoldable f where
  -- | Given a function to generate an element from a seed, 
  -- and an initial seed, generate a structure.
  unfoldMap :: Splittable s => (s -> a) -> s -> f a

unfold :: (Unfoldable f, Splittable s) => s -> f s
unfold = unfoldMap id

leftMost :: Unfoldable f => f ()
leftMost = unfoldMap (const ()) L

rightMost :: Unfoldable f => f ()
rightMost = unfoldMap (const ()) R

spread :: Splittable s => State ([s], Int) a -> s -> a
spread f s = let (a, (_, i)) = runState f (split i s, 0) in a

to :: (s -> a) -> State ([s], Int) a
to f = state $ \(ss, i) -> (f (head ss), (tail ss, i + 1))

instance Unfoldable [] where
  unfoldMap f = go
    where
      go = choose [const [], spread $ (:) <$> to f <*> to go]

instance Unfoldable Maybe where
  unfoldMap f = choose [const Nothing, Just . f]

instance (Bounded a, Enum a) => Unfoldable (Either a) where
  unfoldMap f = choose [Left . boundedEnum, Right . f]

instance (Bounded a, Enum a) => Unfoldable ((,) a) where
  unfoldMap f = spread $ (,) <$> to boundedEnum <*> to f

instance Unfoldable Identity where
  unfoldMap f = Identity . f

instance (Bounded a, Enum a) => Unfoldable (Constant a) where
  unfoldMap _ = Constant . boundedEnum
  
instance (Unfoldable p, Unfoldable q) => Unfoldable (Product p q) where
  unfoldMap f = spread $ Pair <$> to (unfoldMap f) <*> to (unfoldMap f)

instance (Unfoldable p, Unfoldable q) => Unfoldable (Compose p q) where
  unfoldMap f = Compose . unfoldMap (unfoldMap f)

instance Unfoldable f => Unfoldable (Reverse f) where
  unfoldMap f = Reverse . unfoldMap (f . getDual) . Dual