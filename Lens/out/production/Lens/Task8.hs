{-# LANGUAGE Rank2Types #-}

module Task8
  ( changeExtension
  , deleteDirIfEmpty
  , getPath
  , listContentsRec
  , listDirContentsRec
  , listFilesRec
  , move
  ) where

import Data.Maybe (isJust)
import Lens.Micro (SimpleFold, SimpleGetter, Traversal', failing, filtered, to, traversed, 
                   (%~), (&), (^.), (^..), (^?))
import System.FilePath (addTrailingPathSeparator, replaceExtension, (</>))
import Task6 (FS (..), childrenSubtrees, dirContents, dirName, fileName, fsName, subtree)

changeExtension :: String -> FS -> FS
changeExtension newExt dir = dir & dirContents . traversed . fileName %~ flip replaceExtension newExt

listDir :: Traversal' FS FilePath
listDir = dirContents . traversed . failing fileName listDir

listFilesRec :: FS -> [FilePath]
listFilesRec fs = fs ^.. failing fileName listDir

listContentsRec :: FS -> [FilePath]
listContentsRec fs = fs ^.. subtree

listDirContentsRec :: FS -> [FilePath]
listDirContentsRec fs = fs ^.. childrenSubtrees

deleteDirIfEmpty :: FilePath -> FS -> FS
deleteDirIfEmpty dir fs = fs & dirContents %~ filterEmptyDirs
    where
        emptydir e =
            let f1 = e ^? dirName . filtered (== dir)
                f2 = e ^? dirContents . filtered null
            in isJust f1 && isJust f2
        filterEmptyDirs = filter (not . emptydir)

move :: FilePath -> SimpleFold FS FS
move path f fs@(Dir dname elems) =
    let targets = elems ^.. traversed . filtered ((== path) . (^. fsName))
        updTargets = targets & traversed . fsName %~ (dname </>)
    in (dirContents . traversed) f (fs {contents = updTargets})
move _ _ fs = pure fs

getPath :: SimpleGetter FS FilePath
getPath = to pathGetter
    where
        pathGetter Dir {name = dname} = addTrailingPathSeparator dname
        pathGetter (File fname) = fname