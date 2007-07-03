-- UUAGC 0.9.5 (TestOkLongFirstName.ag)
module TestOkLongFirstName where
-- Dummy -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         x                    : Int
   alternatives:
      alternative Dummy:
         child thisisaverylongname : Dummy
-}
data Dummy = Dummy_Dummy (Dummy)
-- cata
sem_Dummy :: Dummy ->
             T_Dummy
sem_Dummy (Dummy_Dummy _thisisaverylongname) =
    (sem_Dummy_Dummy (sem_Dummy _thisisaverylongname))
-- semantic domain
type T_Dummy = ( Int)
sem_Dummy_Dummy :: T_Dummy ->
                   T_Dummy
sem_Dummy_Dummy thisisaverylongname_ =
    (let _lhsOx :: Int
         _thisisaverylongnameIx :: Int
         -- "TestOkLongFirstName.ag"(line 8, column 7)
         _lhsOx =
             _thisisaverylongnameIx
                      + let y = 4
                         in _thisisaverylongnameIx
                                     + y
         ( _thisisaverylongnameIx) =
             (thisisaverylongname_ )
     in  ( _lhsOx))
