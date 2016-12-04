-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unfolder
-- Copyright   :  (c) Sjoerd Visscher 2014
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
    GeneralizedNewtypeDeriving
  , RankNTypes
  , Trustworthy
  , CPP
  #-}

#if !defined(MIN_VERSION_containers)
#define MIN_VERSION_containers(x,y,z) 0
#endif

module Data.Unfolder
  (

  -- * Unfolder
    Unfolder(..)
  , chooseMonadDefault
  , chooseMapMonadDefault

  , between
  , betweenD
  , boundedEnum
  , boundedEnumD

  -- ** Unfolder instances
  , Random(..)

  , Arb(..)
  , arbUnit

  , NumConst(..)
  , Nth(..)

  -- * UnfolderTransformer
  , UnfolderTransformer(..)
  , ala
  , ala2
  , ala3

  -- ** UnfolderTransformer instances
  , DualA(..)

  , NT(..)
  , WithRec(..)
  , withRec
  , limitDepth

  , BFS(..)
  , Split
  , bfs
  , bfsBySum
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
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import qualified System.Random as R
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, sized, resize)

import Data.Monoid (Monoid(..))
import Data.Maybe (catMaybes)
import qualified Data.Sequence as S

-- | Unfolders provide a way to unfold data structures.
-- The methods have default implementations in terms of 'Alternative',
-- but you can implement 'chooseMap' to act on recursive positions of the
-- data structure, or simply to provide a faster implementation than
-- 'foldr ((<|>) . f) empty'.
class Alternative f => Unfolder f where
  -- | Choose one of the values from the list.
  choose :: [f a] -> f a
  choose = chooseMap id
  -- | Choose one of the values from the list and apply the given function.
  chooseMap :: (a -> f b) -> [a] -> f b
  chooseMap f = foldr ((<|>) . f) empty
  -- | Given a number 'n', return a number between '0' and 'n - 1'.
  chooseInt :: Int -> f Int
  chooseInt n = chooseMap pure [0 .. n - 1]

-- | If an unfolder is monadic, 'choose' can be implemented in terms of 'chooseInt'.
chooseMonadDefault :: (Monad m, Unfolder m) => [m a] -> m a
chooseMonadDefault ms = chooseInt (length ms) >>= (ms !!)

-- | If an unfolder is monadic, 'chooseMap' can be implemented in terms of 'chooseInt'.
chooseMapMonadDefault :: (Monad m, Unfolder m) => (a -> m b) -> [a] -> m b
chooseMapMonadDefault f as = chooseInt (length as) >>= f . (as !!)

-- | If a datatype is enumerable, we can use 'chooseInt' to generate a value.
-- This is the function to use if you want to unfold a datatype that has no type arguments (has kind @*@).
between :: (Unfolder f, Enum a) => a -> a -> f a
between lb ub = (\x -> toEnum (x + fromEnum lb)) <$> chooseInt (1 + fromEnum ub - fromEnum lb)

-- | If a datatype is also bounded, we choose between all possible values.
--
-- > boundedEnum = between minBound maxBound
boundedEnum :: (Unfolder f, Bounded a, Enum a) => f a
boundedEnum = between minBound maxBound

-- | 'betweenD' uses 'choose' to generate a value. It chooses between the lower bound and one
--   of the higher values. This means that f.e. breadth-first unfolding and arbitrary will prefer
--   lower values.
betweenD :: (Unfolder f, Enum a) => a -> a -> f a
betweenD lb0 ub = betweenD' lb0 (fromEnum ub - fromEnum lb0)
  where
    betweenD' lb n | n < 0 = empty
                   | otherwise = choose [pure lb, betweenD' (succ lb) (pred n)]

-- | > boundedEnumD = betweenD minBound maxBound
boundedEnumD :: (Unfolder f, Bounded a, Enum a) => f a
boundedEnumD = betweenD minBound maxBound



-- | Derived instance.
instance MonadPlus m => Unfolder (WrappedMonad m)

-- | Derived instance.
instance (ArrowZero a, ArrowPlus a) => Unfolder (WrappedArrow a b)

-- | Don't choose but return all items.
instance Unfolder [] where
  choose = concat
  chooseMap = concatMap
  chooseInt n = [0 .. n - 1]

-- | Always choose the first item.
instance Unfolder Maybe where
  choose = foldr const Nothing
  chooseMap f = foldr (const . f) Nothing
  chooseInt 0 = Nothing
  chooseInt _ = Just 0

-- | Derived instance.
instance (Unfolder p, Unfolder q) => Unfolder (Product p q) where
  chooseMap f as = Pair (chooseMap (fstP . f) as) (chooseMap (sndP . f) as)
    where
      fstP (Pair p _) = p
      sndP (Pair _ q) = q
  chooseInt n = Pair (chooseInt n) (chooseInt n)

-- | Derived instance.
instance (Unfolder p, Applicative q) => Unfolder (Compose p q) where
  chooseMap f = Compose . chooseMap (getCompose . f)
  chooseInt n = Compose $ pure <$> chooseInt n

-- | Derived instance.
instance Unfolder f => Unfolder (Reverse f) where
  chooseMap f = Reverse . chooseMap (getReverse . f)
  chooseInt n = Reverse $ chooseInt n

-- | Derived instance.
instance Unfolder f => Unfolder (Backwards f) where
  chooseMap f = Backwards . chooseMap (forwards . f)
  chooseInt n = Backwards $ chooseInt n

-- | Derived instance.
instance Unfolder f => Unfolder (Lift f)

-- | Derived instance.
instance (Functor m, Monad m, Monoid e) => Unfolder (ExceptT e m)

-- | Derived instance.
instance Applicative f => Unfolder (ListT f) where
  {-# INLINABLE chooseMap #-}
  chooseMap f = ListT . foldr appRun (pure [])
    where
      appRun x ys = (++) <$> runListT (f x) <*> ys
  chooseInt n = ListT $ pure [0 .. n - 1]

-- | Derived instance.
instance (Functor m, Monad m) => Unfolder (MaybeT m) where
  chooseMap _ [] = MaybeT (return Nothing)
  chooseMap f (a : as) = MaybeT $ do
    res <- runMaybeT (f a)
    case res of
      Nothing -> runMaybeT $ chooseMap f as
      Just _ -> return res
  chooseInt 0 = MaybeT $ return Nothing
  chooseInt _ = MaybeT $ return (Just 0)

-- | Derived instance.
instance (Monoid w, MonadPlus m, Unfolder m) => Unfolder (RWST r w s m) where
  chooseMap f as = RWST $ \r s -> chooseMap (\a -> runRWST (f a) r s) as

-- | Derived instance.
instance (MonadPlus m, Unfolder m) => Unfolder (StateT s m) where
  chooseMap f as = StateT $ \s -> chooseMap (\a -> f a `runStateT` s) as

-- | Derived instance.
instance Unfolder m => Unfolder (ReaderT r m) where
  chooseMap f as = ReaderT $ \r -> chooseMap (\a -> f a `runReaderT` r) as

-- | Derived instance.
instance (Monoid w, Unfolder m) => Unfolder (WriterT w m) where
  chooseMap f = WriterT . chooseMap (runWriterT . f)

-- | Don't choose but return all items.
instance Unfolder S.Seq where
#if MIN_VERSION_containers(0,5,6)
  chooseInt n = S.fromFunction n id
#endif


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
  chooseMap = chooseMapMonadDefault
  chooseInt 0 = Random . StateT $ const (fail "Random chooseInt 0")
  chooseInt n = Random . StateT $ return . R.randomR (0, n - 1)


data Nth a = Nth
  { size :: Integer
  , getNth :: Integer -> a
  }
instance Functor Nth where
  fmap f (Nth sizeA as) = Nth sizeA (f . as)
instance Applicative Nth where
  pure a = Nth 1 (const a)
  Nth sizeF fs <*> Nth sizeA as = Nth (sizeF * sizeA) $ \n ->
    let (l, r) = n `divMod` sizeA in fs l (as r)
instance Alternative Nth where
  empty = Nth 0 (const undefined)
  Nth sizeA as <|> Nth sizeB bs = Nth (sizeA + sizeB) $ \n ->
    if n < sizeA then as n else bs (n - sizeA)
instance Unfolder Nth where
  chooseInt n = Nth (toInteger n) fromInteger


-- | An 'UnfolderTransformer' changes the way an 'Unfolder' unfolds.
class UnfolderTransformer t where
  -- | Lift a computation from the argument unfolder to the constructed unfolder.
  lift :: Unfolder f => f a -> t f a

-- | Run an unfolding function with one argument using an 'UnfolderTransformer', given a way to run the transformer.
ala :: (UnfolderTransformer t, Unfolder f) => (t f b -> f b) -> (t f a -> t f b) -> f a -> f b
ala lower f = lower . f . lift

-- | Run an unfolding function with two arguments using an 'UnfolderTransformer', given a way to run the transformer.
ala2 :: (UnfolderTransformer t, Unfolder f) => (t f c -> f c) -> (t f a -> t f b -> t f c) -> f a -> f b -> f c
ala2 lower f = ala lower . f . lift

-- | Run an unfolding function with three arguments using an 'UnfolderTransformer', given a way to run the transformer.
ala3 :: (UnfolderTransformer t, Unfolder f) => (t f d -> f d) -> (t f a -> t f b -> t f c -> t f d) -> f a -> f b -> f c -> f d
ala3 lower f = ala2 lower . f . lift


-- | 'DualA' flips the @\<|>@ operator from `Alternative`.
newtype DualA f a = DualA { getDualA :: f a }
  deriving (Eq, Show, Functor, Applicative)

instance Alternative f => Alternative (DualA f) where
  empty = DualA empty
  DualA a <|> DualA b = DualA (b <|> a)

-- | Reverse the list passed to choose.
instance Unfolder f => Unfolder (DualA f) where
  chooseMap f = DualA . chooseMap (getDualA . f) . reverse
  chooseInt n = DualA $ (\x -> n - 1 - x) <$> chooseInt n

instance UnfolderTransformer DualA where
  lift = DualA


-- | Natural transformations
data NT f g = NT { getNT :: forall a. f a -> g a }

newtype WithRec f a = WithRec { getWithRec :: ReaderT (Int -> NT f f) f a }
  deriving (Functor, Applicative, Alternative)

-- | Applies a certain function depending on the depth at every recursive position.
instance Unfolder f => Unfolder (WithRec f) where
  chooseMap h as = WithRec . ReaderT $ \f ->
    getNT (f 0) $ chooseMap (withRec (f . succ) . h) as

instance UnfolderTransformer WithRec where
  lift = WithRec . ReaderT . const

-- | Apply a certain function of type @f a -> f a@ to the result of a 'choose'.
-- The depth is passed as 'Int', so you can apply a different function at each depth.
-- Because of a @forall@, the function needs to be wrapped in a 'NT' constructor.
-- See 'limitDepth' for an example how to use this function.
withRec :: (Int -> NT f f) -> WithRec f a -> f a
withRec f = (`runReaderT` f) . getWithRec

-- | Limit the depth of an unfolding.
limitDepth :: Unfolder f => Int -> WithRec f a -> f a
limitDepth m = withRec (\d -> NT $ if d == m then const empty else id)



-- | Return a generator of values of a given depth.
-- Returns 'Nothing' if there are no values of that depth or deeper.
-- The depth is the number of 'choose' calls.
newtype BFS f x = BFS { getBFS :: (Int, Split) -> Maybe [f x] }

type Split = Int -> [(Int, Int)]

instance Functor f => Functor (BFS f) where
  fmap f = BFS . (fmap (map (fmap f)) .) . getBFS

instance Applicative f => Applicative (BFS f) where
  pure = packBFS . pure
  BFS ff <*> BFS fx = BFS $ \(d, split) -> flattenBFS $
    [ liftA2 (liftA2 (<*>)) (ff (i, split)) (fx (j, split)) | (i, j) <- split d ]

instance Applicative f => Alternative (BFS f) where
  empty = BFS $ \(d, _) -> if d == 0 then Just [] else Nothing
  BFS fa <|> BFS fb = BFS $ \d -> flattenBFS [fa d, fb d]

-- | Choose between values of a given depth only.
instance Applicative f => Unfolder (BFS f) where
  chooseMap f as = BFS $ \(d, split) -> if d == 0 then Just [] else flattenBFS (map (\a -> f a `getBFS` (d - 1, split)) as)

instance UnfolderTransformer BFS where
  lift = packBFS

bySum :: Split
bySum d = [(i, d - i)| i <- [0 .. d]]

byMax :: Split
byMax d = [(i, d)| i <- [0 .. d - 1]] ++ [(d, i)| i <- [0 .. d]]

bfsBy :: Unfolder f => Split -> BFS f x -> f x
bfsBy split (BFS f) = choose (loop 0) where loop d = maybe [] (++ loop (d + 1)) (f (d, split))

-- | Change the order of unfolding to be breadth-first, by maximum depth of the components.
bfs :: Unfolder f => BFS f x -> f x
bfs = bfsBy byMax

-- | Change the order of unfolding to be breadth-first, by the sum of depths of the components.
bfsBySum :: Unfolder f => BFS f x -> f x
bfsBySum = bfsBy bySum

packBFS :: f x -> BFS f x
packBFS r = BFS $ \(d, _) -> if d == 0 then Just [r] else Nothing

flattenBFS :: [Maybe [a]] -> Maybe [a]
flattenBFS ms = case catMaybes ms of
  [] -> Nothing
  ms' -> Just (concat ms')


-- | A variant of Test.QuickCheck.Gen, with failure
-- and a count of the number of recursive positions.
data Arb a = Arb Int (Gen (Maybe a))

instance Functor Arb where
  fmap f (Arb i g) = Arb i $ fmap (fmap f) g

instance Applicative Arb where
  pure = Arb 0 . pure . pure
  Arb i1 ff <*> Arb i2 fx = Arb (i1 + i2) $ liftA2 (<*>) ff fx

instance Alternative Arb where
  empty = Arb 0 (pure Nothing)
  Arb ia fa <|> Arb ib fb = Arb ((ia + ib + 1) `div` 2) $ oneof [fa, fb]

-- | Limit the depth of the generated data structure by
-- dividing the given size by the number of recursive positions.
instance Unfolder Arb where
  chooseMap f as = Arb 1 $ sized g
    where
      g 0 = pure Nothing
      g n = flatMapArb ((\(Arb i gen) -> resize (n `div` max i 1) gen) . f) as

flatMapArb :: (a -> Gen (Maybe b)) -> [a] -> Gen (Maybe b)
flatMapArb f = go [] where
  go [] [] = pure Nothing
  go as [] = Just <$> elements as
  go as (g:gs) = f g >>= \ma -> go (maybe as (:as) ma) gs

arbUnit :: Arbitrary a => Arb a
arbUnit = Arb 0 (Just <$> arbitrary)

-- | Variant of 'Data.Functor.Constant' that does multiplication of the constants for @\<*>@ and addition for @\<|>@.
newtype NumConst a x = NumConst { getNumConst :: a } deriving (Eq, Show)
instance Functor (NumConst a) where
  fmap _ (NumConst a) = NumConst a
instance Num a => Applicative (NumConst a) where
  pure _ = NumConst 1
  NumConst a <*> NumConst b = NumConst $ a * b
instance Num a => Alternative (NumConst a) where
  empty = NumConst 0
  NumConst a <|> NumConst b = NumConst $ a + b
-- | Unfolds to a constant numeric value. Useful for counting shapes.
instance Num a => Unfolder (NumConst a)
