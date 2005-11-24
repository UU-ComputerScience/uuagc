--  $Header: /data/cvs-rep/uust/tools/ag/TokenDef.hs,v 1.8 2004/01/08 12:14:18 uust Exp $
--  $Name:  $ (version name)

module TokenDef where

import UU.Scanner.Token
import UU.Scanner.GenToken
import UU.Scanner.GenTokenOrd
import UU.Scanner.Position
import UU.Parsing.MachineInterface(Symbol(..))

instance Symbol Token  where
 deleteCost (Reserved key _) = case key of 
                "DATA"         -> 7
                "EXT"          -> 7
                "ATTR"         -> 7
                "SEM"          -> 7
                "USE"          -> 7
                "INCLUDE"      -> 7
                _              -> 5
 deleteCost (ValToken v _  _) = case v of
                TkError -> 0
                _       -> 5
                            