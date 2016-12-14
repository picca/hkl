module Hkl.Utils
    ( saveScript )
        where

import Data.Text (Text)
import Data.Text.IO (writeFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Prelude hiding (writeFile)

saveScript :: Text -> FilePath -> IO ()
saveScript c f = do
    createDirectoryIfMissing True (takeDirectory f)
    writeFile f c
    print $ "--> created : " ++ f
