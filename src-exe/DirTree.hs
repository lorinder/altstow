module DirTree (
    DirTree(..)
  , getDirTree
  , modifyAttribs
  , walk
  , merge
) where

import Control.Monad
import Data.List

import qualified System.Directory.OsPath as D
import System.OsPath ((</>), OsPath)

import FileUtils

data DirTree a = File OsPath a | Dir OsPath [DirTree a]

deriving instance Show a => Show (DirTree a)

rootName :: DirTree a -> OsPath
rootName (File x _) = x
rootName (Dir x _) = x

-- Typical typeclasses
--
-- XXX:  While they work, they only act on the attributes, not the
-- names, and are therefor not that good a fit for many uses of the
-- tree.

instance Functor DirTree where
    fmap :: (a -> b) -> DirTree a -> DirTree b
    fmap f (File name x) = File name (f x)
    fmap f (Dir name xs) = Dir name (map (fmap f) xs)

instance Foldable DirTree where
    foldr :: (a -> b -> b) -> b -> DirTree a -> b
    foldr f acc x = foldr f acc $ flattenAttribs x

flattenAttribs :: DirTree a -> [a]
flattenAttribs (File _ x) = [x]
flattenAttribs (Dir _ xs) = concat $ map flattenAttribs xs

-- IO functions

getDirTree :: OsPath -> IO (DirTree ())
getDirTree base = getSubTree mempty mempty
    where   getSubTree :: OsPath -> OsPath -> IO (DirTree ())
            getSubTree location subdir = do
                let bloc = base </> location
                fileNames <- sort <$> D.listDirectory (bloc </> subdir)
                files <- forM fileNames (\fn -> do
                    isDir <- D.doesDirectoryExist $ bloc </> subdir </> fn
                    if isDir then 
                        getSubTree (location </> subdir) fn
                    else
                        return $ File fn ()
                    )
                return $ Dir subdir files

-- Attribute modification.
--
-- Acts in a monad so that error handling or IO in particular can be
-- easily integrated.

modifyAttribs :: (Monad m)
    => OsPath                               -- ^ Current path
    -> (OsPath -> a -> m c)                 -- ^ modifier func
    -> DirTree a                            -- ^ the tree to take
    -> m (DirTree c)
modifyAttribs path f (File name attr) = do
    attr' <- f (path </> name) attr
    return $ File name attr'
modifyAttribs path f (Dir name xs) = do
    let path' = path </> name
    let mxs = map (modifyAttribs path' f) xs
    l <- sequence mxs
    return $ Dir name l

walk :: (Monad m)
    => OsPath                               -- ^ base path
    -> (OsPath -> a -> m b)                 -- ^ visiting function
    -> DirTree a                            -- ^ tree to walk
    -> m [b]
walk path f (File name attr) = do
    r <- f (path </> name) attr
    return [r]
walk path f (Dir name xs) = do
    l <- sequence $ map (walk (path </> name) f) xs
    return $ concat l

--
-- merge functionality
--

leftf
    :: (OsPath -> Maybe a -> Maybe b -> Either String c)
    -> OsPath
    -> a
    -> Either String c
leftf f path x = f path (Just x) Nothing

rightf
    :: (OsPath -> Maybe a -> Maybe b -> Either String c)
    -> OsPath
    -> b
    -> Either String c
rightf f path y = f path Nothing (Just y)

mergeLists
    :: OsPath                                   -- ^ base path
    -> [DirTree a]                              -- ^ left list
    -> [DirTree b]                              -- ^ right list
    -> (OsPath -> Maybe a -> Maybe b -> Either String c)  -- ^ file attribute merger
    -> Either String [DirTree c]
mergeLists _ [] [] _ = return []
mergeLists path xs [] f =
    traverse (modifyAttribs path (leftf f)) xs
mergeLists path [] ys f =
    traverse (modifyAttribs path (rightf f)) ys
mergeLists path (x:xs) (y:ys) f = 
    let nx = rootName x
        ny = rootName y
    in  if nx == ny then do
            m <- mergeNode path x y f
            rest <- mergeLists path xs ys f
            return (m:rest)
        else if nx < ny then do
            ma <- modifyAttribs path (leftf f) x
            rest <- mergeLists path xs (y:ys) f
            return (ma:rest)
        else do
            ma <- modifyAttribs path (rightf f) y
            rest <- mergeLists path (x:xs) ys f
            return (ma:rest)

-- Merge two nodes with identical names.
mergeNode
    :: OsPath                                   -- ^ base path
    -> DirTree a
    -> DirTree b
    -> (OsPath -> Maybe a -> Maybe b -> Either String c)
    -> Either String (DirTree c)
mergeNode path (File name vl) (File _ vr) f = do
    ma <- f (path </> name) (Just vl) (Just vr)
    pure $ File name ma
mergeNode path (Dir name xs) (Dir _ ys) f = do
    ents <- mergeLists path xs ys f
    pure $ Dir name ents
mergeNode path x _ _ =
    Left $ "Node type mismatch for " ++ (osPathToString (path </> rootName x))

merge :: DirTree a
      -> DirTree b
      -> (OsPath -> Maybe a -> Maybe b -> Either String c)
      -> Either String (DirTree c)
merge (Dir _ vl) (Dir _ vr) f = do
    l <- mergeLists mempty vl vr f
    pure $ Dir mempty l
merge _ _ _ = Left "Invalid tree passed to merge function"
