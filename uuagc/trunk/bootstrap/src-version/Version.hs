-- | This module is a placeholder. It it installed in combination with the UUAGC application,
--   so that the uuagc-bootstrap-package shows up as an explicit dependency.
--
-- Note: this is the Version.hs of the bootstrapping UUAG compiler.
module Version where

import Data.Version
import UU.UUAGC.BootstrapVersion

-- | Description of the application including the application's version number
banner :: String
banner = ("Attribute Grammar compiler / HUT project. Version " ++ showVersion version)
