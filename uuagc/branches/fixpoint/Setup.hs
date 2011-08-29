module Main where

import Distribution.Simple
import Distribution.Simple.UUAGC
import UUAGCTests(uuagcPreTest
                 ,uuagcTestHook
                 ,uuagcPostTest
                 )

main :: IO ()
main = do putStrLn "Estoy iniciando"
          defaultMainWithHooks $ uuagcUserHook { preTest  = uuagcPreTest
                                               , testHook = uuagcTestHook
                                               , postTest = uuagcPostTest
                                               }

