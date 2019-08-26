{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ChapterTwo where

import           Prelude hiding (lookup)

class FiniteMap m k where
  empty  :: m k a
  bind   :: k -> a -> m k a -> m k a
  lookup :: k -> m k a -> Maybe a

data Tree k v
  = E
  | T (Tree k v) (k, v) (Tree k v)
  deriving Show

instance (Ord k) => (FiniteMap Tree k) where
  empty = E

  bind key val E                    = T E (key, val) E
  bind key val (T l (key_, val_) r) | key == key_ =
                                      T l (key_, val) r
                                    | key > key_  =
                                      T l (key_, val_) (bind key val r)
                                    | otherwise   =
                                      T (bind key val l) (key_, val_) r

  lookup key E                    = Nothing
  lookup key (T l (key_, val_) r) | key == key_ = Just val_
                                  | key > key_  = lookup key r
                                  | otherwise   = lookup key l

sampleFiniteMap :: Tree Int String
sampleFiniteMap = T (T E (1,"Maul") E) (2,"Tyranus") (T E (3,"Vader") E)
