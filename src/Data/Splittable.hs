{-# LANGUAGE ScopedTypeVariables #-}
module Data.Splittable (
    Splittable(..)
  
  , boundedEnum
  
  , Left(..)
  , Right(..)
  
  ) where 

import qualified System.Random as R
import Data.List (mapAccumR)
import Data.Monoid (Dual(..))

-- | Splittable datatypes are datatypes that can be used as seeds for unfolds.
class Splittable s where
  -- | @split n s@ splits the seed @s@ in @n@ seeds. 
  split  :: Int -> s -> [s]
  -- | @choose fs s@ uses part of the seed @s@ to choose a function from the list @fs@,
  -- and passes the remainder to that function.
  choose :: [s -> x] -> s -> x
  -- | Convert the seed value to an @int@.
  getInt :: s -> Int

-- | If a datatype is bounded and enumerable, we can use 'getInt' to produce a value from a seed.
boundedEnum :: forall s a. (Splittable s, Bounded a, Enum a) => s -> a
boundedEnum s = toEnum $ (getInt s `mod'` (1 + ub - lb)) + lb
  where 
    lb = fromEnum (minBound :: a)
    ub = fromEnum (maxBound :: a)
    n `mod'` 0 = n - lb
    n `mod'` m = n `mod` m 

data Left = L
-- | Always choose the first item.
instance Splittable Left where
  split = replicate
  choose fs = head fs
  getInt L = 0

data Right = R
-- | Always choose the last item.
instance Splittable Right where
  split = replicate
  choose fs = last fs
  getInt R = 0

-- | Choose randomly
instance Splittable R.StdGen where
  split 0 _ = []
  split 1 s = [s]
  split n s = let (s1, s2) = R.split s in s1 : split (n - 1) s2 
  choose fs s = let (n, s') = R.next s in fs !! (n `mod` length fs) $ s'
  getInt = fst . R.next

-- | The 'Integer' instance uses modulo to choose, and splits breadth-first by 
-- distributing bits in a round-robin fashion.
instance Splittable Integer where
  split n t = split' 1 (t, replicate n 0)
    where
      split' _ (0, l) = l
      split' p (s, l) = split' (p * 2) $ mapAccumR (\s' i -> let (s'', b) = s' `divMod` 2 in (s'', i + b * p)) s l
  choose fs s = let (s', n) = s `divMod` toInteger (length fs) in fs !! fromInteger n $ s'
  getInt = fromInteger

-- | The @(a, b)@ instance uses only @a@ for 'choose' and @b@ for 'getInt'.
instance (Splittable a, Splittable b) => Splittable (a, b) where
  split n (a, b) = zip (split n a) (split n b)
  choose = uncurry . choose . map curry
  getInt (_, b) = getInt b

-- | Choose between 2 ways to split and choose.
instance (Splittable a, Splittable b) => Splittable (Either a b) where
  split n   = either (map Left . split n) (map Right . split n)
  choose fs = either (choose (map (. Left) fs)) (choose (map (. Right) fs))
  getInt (Left a) = getInt a * 2
  getInt (Right a) = getInt a * 2 + 1

-- | Reverse the split output and the choose input.
instance Splittable s => Splittable (Dual s) where
  split n = map Dual . reverse . split n . getDual
  choose fs = choose (map (. Dual) $ reverse fs) . getDual
  getInt = negate . getInt . getDual