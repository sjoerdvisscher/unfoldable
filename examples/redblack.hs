{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import Data.Unfoldable
import Data.Unfolder

import Data.Maybe
import System.Random

-- red-black tree implementation adapted from https://gist.github.com/2660297
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)
data NatW :: Nat -> * where
  ZeroW :: NatW Zero
  SuccW :: NatW n -> NatW (Succ n)

data RedBlack = Black | Red deriving (Eq, Ord, Show)

data RedBlackTree a where 
  T :: Node Black n a -> RedBlackTree a
deriving instance Show a => Show (RedBlackTree a)

data Node :: RedBlack -> Nat -> * -> * where
  Leaf :: Node Black Zero a
  B :: Node cL    n a -> a -> Node cR    n a -> Node Black (Succ n) a
  R :: Node Black n a -> a -> Node Black n a -> Node Red    n       a
deriving instance Show a => Show (Node c n a)

instance Unfoldable RedBlackTree where
  unfold = u ZeroW
    where
      u :: forall n f a. (Unfoldable (Node Black n), Unfolder f) 
        => NatW n -> f a -> f (RedBlackTree a)
      u n fa = choose 
        [ T <$> (unfold :: Unfolder f => f a -> f (Node Black n a)) fa
        , u (SuccW n) fa
        ]
  
instance Unfoldable (Node Black Zero) where
  unfold _ = choose [ pure Leaf ]

instance Unfoldable (Node Black n) => Unfoldable (Node Black (Succ n)) where
  unfold = u
    where
      u :: forall f a. Unfolder f => f a -> f (Node Black (Succ n) a)
      u fa = choose 
        [ B <$> b <*> fa <*> b
        , B <$> b <*> fa <*> r
        , B <$> r <*> fa <*> b
        , B <$> r <*> fa <*> r
        ]
        where
          r :: Unfolder f => f (Node Red n a)
          r = unfold fa
          b :: Unfolder f => f (Node Black n a)
          b = unfold fa

instance Unfoldable (Node Black n) => Unfoldable (Node Red n) where
  unfold fa = choose [ R <$> unfold fa <*> fa <*> unfold fa ]

rbtree8 :: RedBlackTree Int
rbtree8 = fromJust $ fromList [0..7]

rbtreeShapes :: [RedBlackTree ()]
rbtreeShapes = take 20 unfold_

randomRBTree :: IO (RedBlackTree Bool)
randomRBTree = getStdRandom randomDefault
