{-# LANGUAGE 
    ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  #-}
module Data.Unfolder.Arbitrary where
  
import Control.Applicative
import Data.Unfoldable
import Data.Unfolder
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Control.Monad.Trans.Reader
import Data.Functor.Constant
import Data.Monoid (Sum(..))

-- This is somewhat of a hack. It assumes that choose always chooses from a list of Gen (t a),
-- which is true at the top-level, but might not be when recursing.

newtype CountPos a = CountPos { getCountPos :: Constant (Sum Int, Sum Int, [(Int, Int)]) a }
  deriving (Functor, Applicative)
instance Unfolder CountPos where
  choose ms = CountPos . Constant $ 
    (Sum 0, Sum 1, map (\(CountPos (Constant (Sum c, Sum r, _))) -> (c, r)) ms)

newtype Arb a = Arb { getArb :: ReaderT [(Int, Int)] Gen a }
  deriving (Functor, Applicative)
instance Unfolder Arb where
  choose ms = Arb (ReaderT f)
    where 
      f poss = sized (\n -> oneof . map (resz n) . filter ((<= n) . fst . fst) . zip poss $ ms)
        where
          resz n ((c, r), Arb (ReaderT g)) = resize ((n - c) `div` max r 1) (g poss)

arbitraryDefault :: forall t a. (Unfoldable t, Arbitrary a) => Gen (t a)
arbitraryDefault = flip runReaderT poss . getArb $ unfold (Arb . ReaderT $ const arbitrary)
  where
    CountPos (Constant (_, _, poss)) = 
      unfold (CountPos $ Constant (Sum 1, Sum 0, [])) :: CountPos (t ())
