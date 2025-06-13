{-# LANGUAGE QuasiQuotes #-}

module Actions (
    Actions.compare
  , push
  , pull
  , delete
  , link
) where

import Control.Monad
import System.Directory.OsPath

import DirTree
import FileUtils

import System.OsPath (OsPath, (</>), osp, takeDirectory)

-- | Compare the files in production to staging
compare
    :: OsPath       -- ^ staging base path
    -> DirTree ()   -- ^ staging tree
    -> IO Bool
compare base = visitFiles checker base
    where   checker :: OsPath -> OsPath -> IO Bool
            checker sp pp = do
                e <- doesFileExist pp
                if not e then do
                    putStrLn $ "[MISSING] " ++ (osPathToString pp)
                else do
                    same <- filesHaveSameContent sp pp
                    when (not same) $ do
                        putStrLn $ "[DIFFERS] " ++ (osPathToString pp)
                return True


-- | Push (copy) files from staging into production.
--
--   Files in staging missing in production are copied into production;
--   files different in production are overwritten with the updated
--   version.  Unchanged files are left alone.
push
    :: OsPath       -- ^ staging base path
    -> DirTree ()   -- ^ staging tree
    -> IO Bool
push base = visitFiles pusher base
    where   pusher :: OsPath -> OsPath -> IO Bool
            pusher sp pp = do
                e <- doesFileExist pp
                doCopy <- if not e then
                        return True
                    else do
                        same <- filesHaveSameContent sp pp
                        return (not same)
                when doCopy $ do
                    createDirectoryIfMissing True $
                        takeDirectory pp
                    copyFileWithMetadata sp pp
                return True

-- | Pull (copy) changed files from production into staging.
--
--   Files that differ in production from staging are overwritten in
--   staging to match the production version.  Files existant in staging
--   but missing from production are deleted in staging.
--
--   Note that files that have been newly added to production can never
--   be detected in this way; as the manifest of files to considers is
--   in the staging area.  Thus newly added files cannot be pulled
--   automatically but need to be manually copied into staging.
pull
    :: OsPath       -- ^ staging base path
    -> DirTree ()   -- ^ staging tree
    -> IO Bool
pull base = visitFiles puller base
    where   puller :: OsPath -> OsPath -> IO Bool
            puller sp pp = do
                doDelete <- not <$> doesFileExist pp
                doCopy <- if doDelete then
                        return False
                    else do
                        not <$> filesHaveSameContent sp pp
                if doDelete then do 
                    removeFile sp
                else if doCopy then do
                    copyFileWithMetadata pp sp
                else
                    return ()
                return True

-- | Unstow, i.e., remove files from production.
--
--   Files that exist in staging are deleted from production.
delete
    :: OsPath       -- ^ staging base path
    -> DirTree ()
    -> IO Bool
delete base = visitFiles deleter base
    where   deleter :: OsPath -> OsPath -> IO Bool
            deleter _ pp = do
                doDelete <- doesFileExist pp
                when doDelete $
                    removeFile pp
                return True

-- | Link files in production to stagin
link
    :: OsPath       -- ^ staging base path
    -> DirTree ()
    -> IO Bool
link base = visitFiles linker base
    where   linker :: OsPath -> OsPath -> IO Bool
            linker sp pp = do
                createDirectoryIfMissing True $
                    takeDirectory pp
                spAbs <- makeAbsolute sp
                createFileLink spAbs pp
                return True
    

-- Internal helper functions.

-- | Visit each file and execute an action on it.
--
--   The visitor function takes two paths, one for the file in staging,
--   and the other one for the corresponding file in production.
--
--   Only proper files are visited, not directories.
visitFiles
    :: (OsPath -> OsPath -> IO Bool)        -- ^ visitor (action)
    -> OsPath                               -- ^ staging base path
    -> DirTree ()                           -- ^ the tree
    -> IO Bool
visitFiles f base tr = do
    r <- walk mempty (\p _ -> f (base </> p) ([osp|..|] </> p)) tr
    return $ any not r
