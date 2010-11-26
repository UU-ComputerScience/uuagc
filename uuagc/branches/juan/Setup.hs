-- #!/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.UUAGC

main = defaultMainWithHooks uuagcUserHook 
