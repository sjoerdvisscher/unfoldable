{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Unfoldable
import Data.Unfolder

import Data.Maybe
import System.Random
import Data.List (intercalate)

-- Red-Black tree implementation adapted from https://gist.github.com/2660297
data Nat = Zero | Succ Nat
data NatW :: Nat -> * where
  ZeroW :: NatW 'Zero
  SuccW :: NatW n -> NatW ('Succ n)

data RedBlack = Black | Red

data RedBlackTree a where
  T :: Node 'Black n a -> RedBlackTree a
deriving instance Show a => Show (RedBlackTree a)

data Node :: RedBlack -> Nat -> * -> * where
  Leaf :: Node 'Black 'Zero a
  B :: Node cL    n a -> a -> Node cR    n a -> Node 'Black ('Succ n) a
  R :: Node 'Black n a -> a -> Node 'Black n a -> Node 'Red    n       a
deriving instance Show a => Show (Node c n a)

instance Unfoldable RedBlackTree where
  unfold = u ZeroW
    where
      u :: forall n f a. (Unfoldable (Node 'Black n), Unfolder f)
        => NatW n -> f a -> f (RedBlackTree a)
      u n fa = choose
        [ T <$> (unfold :: f a -> f (Node 'Black n a)) fa
        , u (SuccW n) fa
        ]

instance Unfoldable (Node 'Black 'Zero) where
  unfold _ = choose [ pure Leaf ]

instance Unfoldable (Node 'Black n) => Unfoldable (Node 'Black ('Succ n)) where
  unfold = u
    where
      u :: forall f a. Unfolder f => f a -> f (Node 'Black ('Succ n) a)
      u fa = choose
        [ B <$> b <*> fa <*> b
        , B <$> b <*> fa <*> r
        , B <$> r <*> fa <*> b
        , B <$> r <*> fa <*> r
        ]
        where
          r :: f (Node 'Red n a)
          r = unfold fa
          b :: f (Node 'Black n a)
          b = unfold fa

instance Unfoldable (Node 'Black n) => Unfoldable (Node 'Red n) where
  unfold fa = choose [ R <$> unfold fa <*> fa <*> unfold fa ]

rbtree :: Int -> RedBlackTree Int
rbtree l = fromJust $ fromList [0..l]

rbtreeShapes :: Int -> [RedBlackTree ()]
rbtreeShapes d = limitDepth d unfold_

randomRBTree :: IO (RedBlackTree Bool)
randomRBTree = getStdRandom randomDefault

newtype Formula = Formula [Integer]
instance Show Formula where
  show (Formula xs)
    | all (== 0) xs = "0"
    | otherwise = intercalate " + " $ map showOne $ filter ((/=0) . fst) $ zip xs [(0::Int)..]
        where
          showOne (x, 0) = show x
          showOne (1, 1) = "x"
          showOne (x, 1) = show x ++ "x"
          showOne (1, n) = "x^" ++ show n
          showOne (x, n) = show x ++ "x^" ++ show n

add, mul :: [Integer] -> [Integer] -> [Integer]
add xs [] = xs
add [] ys = ys
add (x:xs) (y:ys) = (x + y) : add xs ys
mul _ [] = []
mul [] _ = []
mul (x:xs) (y:ys) = (x * y) : add (map (x*) ys) (add (map (y*) xs) (0 : mul xs ys))

instance Num Formula where
  fromInteger x = Formula [x]
  Formula xs + Formula ys = Formula (add xs ys)
  Formula xs * Formula ys = Formula (mul xs ys)

varX :: Formula
varX = Formula [0, 1]

-- See http://oeis.org/A001137
rbFormula :: Int -> NumConst Formula (RedBlackTree a)
rbFormula d = ala (limitDepth d) unfold (NumConst varX)
