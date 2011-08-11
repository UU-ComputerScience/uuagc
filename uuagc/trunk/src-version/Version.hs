-- | This module is a placeholder. It it installed in combination with the UUAGC application,
--   so that the uuagc-package shows up as an explicit dependency.
--
-- Note: this is the Version.hs of the bootstrapped UUAG compiler.
module Version where

import UU.UUAGC.Version
import Data.Version

-- | Description of the application including the application's version number
banner :: String
banner = ("Attribute Grammar compiler / HUT project. Version " ++ showVersion version)
