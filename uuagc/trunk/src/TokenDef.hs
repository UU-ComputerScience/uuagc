
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
                            