

-- UUAGC 0.9.38.0 (TestSideEffect.ag)
module TestSideEffect where


import Debug.Trace

main :: IO ()
main = sem_D (D_C2 (D_C)) >>= putStrLn
-- D -----------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         z                    : String
   alternatives:
      alternative C:
         visit 0:
            local _rule_r1    : _
            local _tup1       : _
            local y           : _
            local a           : _
            local _rule_r2    : _
            local x           : _
      alternative C2:
         child k              : D 
-}
data D  = D_C 
        | D_C2 (D ) 
-- cata
sem_D :: D  ->
         T_D 
sem_D (D_C )  =
    (sem_D_C )
sem_D (D_C2 _k )  =
    (sem_D_C2 (sem_D _k ) )
-- semantic domain
type T_D  = ( String)
sem_D_C :: T_D 
sem_D_C  =
    (let _lhsOz :: String
         -- "TestSideEffect.ag"(line 14, column 12)
         __rule_r1 <- do putStrLn "r1!"
                         return ('d', 'a')
         -- "TestSideEffect.ag"(line 14, column 12)
         __tup1 =
             __rule_r1
         -- "TestSideEffect.ag"(line 14, column 12)
         (_y,_) =
             __tup1
         -- "TestSideEffect.ag"(line 14, column 12)
         (_,_a) =
             __tup1
         -- "TestSideEffect.ag"(line 16, column 12)
         __rule_r2 <- do putStrLn "r2!"
                         return 'c'
         -- "TestSideEffect.ag"(line 16, column 12)
         _x =
             __rule_r2
         -- "TestSideEffect.ag"(line 18, column 8)
         _lhsOz <- return [ _a    , _x     , _y     ]
     in  ( _lhsOz))
sem_D_C2 :: T_D  ->
            T_D 
sem_D_C2 k_  =
    (let _lhsOz :: String
         _kIz :: String
         -- use rule "TestSideEffect.ag"(line 11, column 16)
         _lhsOz =
             _kIz
         ( _kIz) <- k_ 
     in  ( _lhsOz))