{-# LANGUAGE  RankNTypes, 
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies, 
              FlexibleInstances, 
              FlexibleContexts, 
              UndecidableInstances,
              NoMonomorphismRestriction#-}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% Some Instances        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module Text.ParserCombinators.UU.BasicInstances where
import Text.ParserCombinators.UU.Parsing

data Error t s pos = Inserted s pos Strings
                   | Deleted  t pos Strings
                   | DeletedAtEnd t

instance (Show t, Show s, Show pos) => Show (Error t s pos) where 
 show (Inserted s pos expecting) = "\nInserted " ++ show s ++ " at position " ++ show pos ++ " expecting one of " ++ show expecting 
 show (Deleted  t pos expecting) = "\nDeleted  " ++ show t ++ " at position " ++ show pos ++ " expecting one of " ++ show expecting 
 show (DeletedAtEnd t)           = "\nThe token " ++ show t ++ "was not consumed by the parsing process." 
data Str     t    = Str   {  input    :: [t]
                          ,  msgs     :: [Error t t Int ]
                          ,  pos      :: !Int
                          ,  deleteOk :: !Bool}

listToStr ls = Str   ls  []  0  True

instance (Show a) => Provides  (Str  a)  (a -> Bool, String, a)  a where
       splitState (p, msg, a) k (Str  tts   msgs pos  ok) 
          = let ins exp =       (5, k a (Str tts (msgs ++ [Inserted a  pos  exp]) pos  False))
                del exp =       (5, splitState (p,msg, a) 
                                    k
                                    (Str (tail tts)  (msgs ++ [Deleted  (head tts)  pos  exp]) (pos+1) True ))
            in case tts of
               (t:ts)  ->  if p t 
                           then  Step 1 (k t (Str ts msgs (pos + 1) True))
                           else  Fail [msg] (ins: if ok then [del] else [])
               []      ->  Fail [msg] [ins]

instance (Ord a, Show a) => Provides  (Str  a)  (a,a)  a where
       splitState a@(low, high) = splitState (\ t -> low <= t && t <= high, show low ++ ".." ++ show high, low)

instance (Eq a, Show a) => Provides  (Str  a)  a  a where
       splitState a  = splitState ((==a), show a, a) 

instance Eof (Str a) where
       eof (Str  i        _    _    _    )                = null i
       deleteAtEnd (Str  (i:ii)   msgs pos ok    )        = Just (5, Str ii (msgs ++ [DeletedAtEnd i]) pos ok)
       deleteAtEnd _                                      = Nothing


instance  Stores (Str a) [Error a a Int] where
       getErrors   (Str  inp      msgs pos ok    )        = (msgs, Str inp [] pos ok)
