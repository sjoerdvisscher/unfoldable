{-# LANGUAGE ScopedTypeVariables #-}
module Data.Unfoldable (

    Unfoldable(..)
  
  , spread
  , to
  , boundedEnum

  ) where
    
import Control.Applicative
import Control.Monad.Trans.State
import Data.Splittable

class Unfoldable f where
  unfold :: Splittable s => (s -> a) -> (s -> f a)

spread :: Splittable s => State ([s], Int) a -> s -> a
spread f s = let (a, (_, i)) = runState f (split i s, 0) in a

to :: (s -> a) -> State ([s], Int) a
to f = state $ \(ss, i) -> (f (head ss), (tail ss, i + 1))

boundedEnum :: forall s a. (Splittable s, Bounded a, Enum a) => s -> a
boundedEnum s = toEnum $ (getInt s `mod` (1 + ub - lb)) + lb
  where 
    lb = fromEnum (minBound :: a)
    ub = fromEnum (maxBound :: a)

instance Unfoldable [] where
  unfold f = go
    where
      go = choose [const [], spread $ (:) <$> to f <*> to go]

instance Unfoldable Maybe where
  unfold f = choose [const Nothing, Just . f]

instance (Bounded a, Enum a) => Unfoldable (Either a) where
  unfold f = choose [Left . boundedEnum, Right . f]

instance (Bounded a, Enum a) => Unfoldable ((,) a) where
  unfold f = spread $ (,) <$> to boundedEnum <*> to f

