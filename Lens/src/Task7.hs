{-# LANGUAGE Rank2Types #-}

module Task7 
    ( cd
    , ls
    , file
    ) where

import Lens.Micro (Traversal', failing, filtered, traversed, (^?))
import Task6 (FS (..), dirContents, dirName, fileName, fsName)

cd :: FilePath -> Traversal' FS FS
cd path = dirContents.traversed.filtered correct
    where
        correct folder = case folder ^? dirName of
            Nothing -> False
            Just dir -> dir == path

ls :: Traversal' FS FilePath
ls = dirContents.traversed.fsName

file :: FilePath -> Traversal' FS FilePath
file fname = failing
    (fileName.filtered (== fname))
    (dirContents.traversed.fileName.filtered (== fname))