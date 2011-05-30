-- #!/bin/env runhaskell

import Distribution.Simple(defaultMainWithHooks, UserHooks(..))
import Distribution.Simple.UUAGC
import UUAGCTests(uuagcPreTest
                 ,uuagcTestHook
                 ,uuagcPostTest
                 )

main :: IO ()
main = defaultMainWithHooks $ uuagcUserHook { preTest  = uuagcPreTest
                                            , testHook = uuagcTestHook
                                            , postTest = uuagcPostTest
                                            }

