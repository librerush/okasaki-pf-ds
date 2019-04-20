{-# OPTIONS_GHC -Wall #-}

module ChapterTwo where

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs'@(_:xs) = xs' : suffixes xs

-- | suffixes [1,2,3,4] = [[1,2,3,4],[2,3,4],[3,4],[4],[]]
-- [1,2,3,4] : ([2,3,4] : ([3,4] : ([4] : [[]])))

