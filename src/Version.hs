module Version ( version ) where

import Data.List ( intercalate )
import Data.Version ( Version(versionBranch) )

import qualified Paths_java_adt as Paths

-- | The program version obtained from the cabal file.

version :: String
version = intercalate "." $ map show $ versionBranch Paths.version
