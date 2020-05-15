module Task6
    ( FS(..)
    , childrenSubtrees
    , dirContents
    , dirName
    , fileName
    , fsName
    , scanDirectory
    , subtree
    ) where

import Control.Applicative (liftA2)
import Lens.Micro (Lens', Traversal', lens)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (splitDirectories, takeFileName, (</>))

data FS 
    = Dir 
        { name     :: FilePath
        , contents :: [FS]
        }
    | File
        { name     :: FilePath
        }
    deriving (Show)

isDir :: FilePath -> IO Bool
isDir fp = do
    dir <- doesDirectoryExist fp
    symlink <- pathIsSymbolicLink fp
    return (dir && not symlink)

isFile :: FilePath -> IO Bool
isFile fp = do
    file <- doesFileExist fp
    symlink <- pathIsSymbolicLink fp
    return (file && not symlink)
                
maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast [x] = Just x
maybeLast (_:xs) = maybeLast xs

parseSubdirs :: FilePath -> [FilePath] -> IO [FS]
parseSubdirs _ [] = return []
parseSubdirs fp (curSubdir:subdirs) = do
    maybeTree <- scanDirectory $ fp </> curSubdir
    case maybeTree of
        Nothing -> parseSubdirs fp subdirs
        Just curTree -> (curTree:) <$> parseSubdirs fp subdirs

scanDirectory :: FilePath -> IO (Maybe FS)
scanDirectory fp = do
    file <- isFile fp
    if file
        then return $ Just $ File $ takeFileName fp
        else do
            dir <- isDir fp
            if dir
                then do
                    let maybeDirName = maybeLast (splitDirectories fp)
                    case maybeDirName of
                        Nothing -> return Nothing
                        Just dirName -> do
                            dirItems <- listDirectory fp
                            subTrees <- parseSubdirs fp dirItems
                            return $ Just $ Dir dirName subTrees
            else return Nothing

fsName :: Lens' FS FilePath
fsName = lens getName setName
    where
        getName = name
        setName tree newName = tree {name = newName}
        
dirName :: Traversal' FS FilePath
dirName f fs@Dir {name = curName} = (\newName -> fs {name = newName}) <$> f curName
dirName _ fs = pure fs

fileName :: Traversal' FS FilePath
fileName f (File x) = File <$> f x
fileName _ fs = pure fs

dirContents :: Traversal' FS [FS]
dirContents f fs@Dir {contents = c} = (\newContents -> fs {contents = newContents}) <$> f c
dirContents _ fs = pure fs

subtree :: Traversal' FS FilePath
subtree f (Dir dname dcontents) = liftA2 Dir (f dname) (traverse (subtree f) dcontents)
subtree f (File fname) = File <$> f fname

childrenSubtrees :: Traversal' FS FilePath
childrenSubtrees f (Dir dname dcontents) = Dir dname <$> traverse (subtree f) dcontents
childrenSubtrees _ fs = pure fs