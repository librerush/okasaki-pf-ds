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

