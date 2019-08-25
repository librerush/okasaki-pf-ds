{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ChapterTwo where

import           Control.Exception

class Set s a where
  empty  :: s a
  insert :: a -> s a -> s a
  member :: a -> s a -> Bool

data UnbalancedSet a
  = E
  | T (UnbalancedSet a) a (UnbalancedSet a)
  deriving Show

sampleTree :: UnbalancedSet Int
sampleTree = T (T E 12 E) 13 (T (T E 14 E) 15 E)

data SetExceptions
  = InsertingExistingElement
  deriving Show

instance Exception SetExceptions


instance Ord a => Set UnbalancedSet a where
  empty = E

  member _ E               = False
  member elem_ t@(T _ z _) = go z elem_ t
    where
      go x y E = x == y
      go cand x (T a y b) | x <= y = go y x a
                          | otherwise = go cand x b

  insert x E         = T E x E
  insert x (T a y b) | x < y = T (insert x a) y b
                     | x > y = T a y (insert x b)
                     | otherwise = throw InsertingExistingElement

complete :: Ord a => a -> Int -> UnbalancedSet a
complete x 0 = T E x E
complete x d = let reComp = complete x (d - 1)
               in T reComp x reComp

createTwo :: Ord a => a -> Int -> (UnbalancedSet a, UnbalancedSet a)
createTwo x m = (lgo x m, rgo x m)
  where
    lgo lx 0  = T E lx E
    lgo lx lm = T (lgo lx (lm - 1)) lx (lgo lx (lm - 1))

    rgo _ 0   = E
    rgo rx rm = T (rgo rx (rm - 1)) rx (rgo rx (rm - 1))
