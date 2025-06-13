module FileUtils (
    filesHaveSameContent
  , osPathToString
) where

import System.IO (IOMode(ReadMode))
import System.OsPath as P
import System.File.OsPath (withBinaryFile)
import Data.ByteString as B 

-- Check whether two files have the same content.
--
-- We explicitly manage the file handles rather than doing lazy I/O to
-- ensure that file handles are closed immediately after the operation.
-- This is necessary because otherwise a subsequent operation such as a
-- file rename of the same files may fail on Windows (an open file
-- can't be renamed on that OS).
filesHaveSameContent
    :: OsPath
    -> OsPath
    -> IO Bool
filesHaveSameContent a b = do
    withBinaryFile a ReadMode (\ha -> do
        withBinaryFile b ReadMode (\hb -> do
            checkMatch ha hb
            )
        )
    where   checkMatch ha hb = do
                let blockSz = 65536
                ba <- B.hGet ha blockSz
                bb <- B.hGet hb blockSz
                if ba /= bb then
                    return False
                else if ba == B.empty then
                    -- EOF reached without finding a difference
                    return True
                else
                    -- keep checking
                    checkMatch ha hb

osPathToString
    :: OsPath
    -> String
osPathToString p =
    let m_str = (decodeUtf p) :: Maybe String
    in case m_str of
            Just s -> s
            Nothing -> "[unrepresentable]"
