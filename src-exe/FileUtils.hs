module FileUtils (
    filesHaveSameContent
  , osPathToString
) where

import Data.ByteString          as B 
import Data.Maybe               (fromMaybe) 
import System.IO                (IOMode(ReadMode))
import System.OsPath            as P
import System.File.OsPath       (withBinaryFile)

-- | Check whether two files have the same content.
filesHaveSameContent
    :: OsPath                   -- ^ first file
    -> OsPath                   -- ^ second file
    -> IO Bool                  -- ^ true if matching, false otherwise
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

-- | Convert an OsPath to a string.
--
-- Convert to a string for the purpose of displaying error messages.  We
-- assume utf-8 encoding.  Currently the error handling is primitive:
-- any invalid utf-8 will result in "[unrepresentable]" rather than an
-- approximation to be displayed.
osPathToString
    :: OsPath
    -> String
osPathToString p =
    let m_str = decodeUtf p :: Maybe String
    in  fromMaybe "[unrepresentable]" m_str
