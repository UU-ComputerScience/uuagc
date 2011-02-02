-- #!/bin/env runhaskell

import Distribution.Simple(defaultMainWithHooks, UserHooks)
import Distribution.Simple.UUAGC

main :: IO ()
main = defaultMainWithHooks uuagcUserHook 
