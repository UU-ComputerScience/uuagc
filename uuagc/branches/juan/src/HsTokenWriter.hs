module HsTokenWriter(initWriter
                    ,setVTab
                    ,resetVTab
                    ,setPatLhs
                    ,addText
                    ,addTextN
                    ,textJstBefore
                    ,textJstAfter
                    ,textJstAfterN
                    ,getHsTokens
                    ,setBottoms
                    ,setExprRhs
                    ,setHsTokenRhs
                    ,setRelativePos
                    ,setRules
                    ,crlf
                    ,crlfbj
                    ,elemRelativePos
                    )
    where

import Control.Monad.State
import UU.Scanner.Position
import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import HsToken
import Patterns
import Expression
import CommonTypes
import FixedPointHelper(FPAttrInfo(..))
import AbstractSyntax(Rule)

data ControlWriter = ControlWriter { currentPos  :: Pos          -- currentPos
                                   , justify     :: [Pos]        -- currentJust
                                   , relative    :: Maybe Pos    -- prev relative Pos
                                   , content     :: Seq HsToken  -- contents
                                   }

type StateHsWriter a = State ControlWriter a

startPos :: Pos
startPos = Pos 1 1 "Fixed Point Generator"

getControlInfo :: State ControlWriter (Pos,[Pos],Maybe Pos,Seq HsToken,ControlWriter)
getControlInfo = do cw <- get
                    return (currentPos cw, justify cw, relative cw, content cw,cw)

getCWInfo :: (ControlWriter -> a) -> State ControlWriter a
getCWInfo f = do cw <- get
                 return (f cw)

setCPos :: Pos -> State ControlWriter ()
setCPos pos = do cw <- get
                 put $ cw { currentPos = pos }

adjCPRel :: Pos -> State ControlWriter ()
adjCPRel pos = do cw <- get
                  put $ cw { currentPos = pos
                           , relative = Just pos
                           }

resetRelative :: State ControlWriter ()
resetRelative = do cw <- get
                   put $ cw { relative = Nothing }

updateContentPos :: Pos -> HsToken -> State ControlWriter ()
updateContentPos pos tkn = do (cp, _, _, ct,cw) <- getControlInfo
                              put $ cw { currentPos = pos
                                       , content    = ct Seq.>< Seq.singleton (upTknPos tkn cp)
                                       }

setJustify :: [Pos] -> State ControlWriter ()
setJustify poss = do cw <- get
                     put $ cw { justify = poss }

addEqOp = addTextN 2 "=" >> setVTab

setVTab :: State ControlWriter ()
setVTab = do (cp, jts, _, _, cw) <- getControlInfo
             put $ cw { justify = cp:jts }

resetVTab :: State ControlWriter ()
resetVTab = do (Pos cLn _ fn, jst, _, _, cw) <- getControlInfo
               let Pos _ cn _:jstrs  = case jst of
                                         [x]      -> [x]
                                         (_:xss) ->  xss
                   np                = Pos cLn cn fn
               put $ cw { justify = np:jstrs }

initState :: ControlWriter
initState = ControlWriter startPos [startPos] Nothing Seq.empty

initWriter :: State ControlWriter ()
initWriter = put $ initState

getSeq :: State ControlWriter (Seq HsToken)
getSeq = do cw <- get
            return $ content cw

getHsTokens :: State ControlWriter a -> HsTokens
getHsTokens m = Seq.foldrWithIndex (\_ a b -> a:b) [] $ evalState (m >> getSeq) initState

addText :: String -> State ControlWriter ()
addText = addTextN 1

addTextN :: Int -> String -> State ControlWriter ()
addTextN i s = do (cp, _, _, ct, cw) <- getControlInfo
                  let hs   = HsToken s cp
                      ncp  = advc (length s + i) cp
                  updateContentPos ncp hs

textJstBefore :: String -> State ControlWriter ()
textJstBefore s = do cw <- get
                     let cp   = currentPos  cw
                         jtf  = justify     cw
                         ct   = content     cw
                         txt  = s
                         hs   = HsToken txt cp
                         ncp  = advc (length txt + 1) cp
                     put (cw { currentPos  = ncp
                             , justify     = cp:jtf
                             , content     = ct Seq.>< Seq.singleton hs
                             }
                         )

textJstAfter :: String -> State ControlWriter ()
textJstAfter = textJstAfterN 1

textJstAfterN :: Int -> String  -> State ControlWriter ()
textJstAfterN n s = do cw <- get
                       let cp   = currentPos  cw
                           jtf  = justify     cw
                           ct   = content     cw
                           txt  = s
                           hs   = HsToken txt cp
                           ncp  = advc (length txt + n) cp
                       put $ cw { currentPos  = ncp
                                , justify     = ncp:jtf
                                , content     = ct Seq.>< Seq.singleton hs
                                }

setPatLhs :: (Identifier -> Identifier -> String) -> Patterns -> State ControlWriter ()
setPatLhs g ps = do cw <- get
                    let cp        = currentPos  cw
                        jtf       = justify     cw
                        ct        = content     cw
                        (cp',ns') = inPat g ps cp
                    put $ cw { currentPos  = cp'
                             , justify     = cp':jtf
                             , content     = ct Seq.>< ns'
                             }

setRules :: (Identifier -> Identifier -> String)
         -> (Identifier -> Identifier -> Pos -> HsToken -> HsToken)
         -> [(Pattern,Expression)]
         -> State ControlWriter ()
setRules f g pe = mapM_ (setRule f g) pe >> crlfbj

setRule :: (Identifier -> Identifier -> String)
        -> (Identifier -> Identifier -> Pos -> HsToken -> HsToken)
        -> (Pattern,Expression)
        -> State ControlWriter ()
setRule f g (p,e) = setRulePatLhs f p >> addEqOp >> setExprRhs g e >> crlfbj

setRulePatLhs :: (Identifier -> Identifier -> String)
              -> Pattern
              -> State ControlWriter ()
setRulePatLhs g p = do cw <- get
                       let cp        = currentPos  cw
                           jtf       = justify     cw
                           ct        = content     cw
                           (cp',ns') = fromPatToHs g p cp
                       put $ cw { currentPos  = cp'
                                , justify     = cp':jtf
                                , content     = ct Seq.>< ns'
                                }


setExprRhs :: (Identifier -> Identifier -> Pos -> HsToken -> HsToken)
           -> Expression
           -> State ControlWriter ()
setExprRhs g (Expression pos tks) = setRelativePos pos >> mapM_ (setHsTokenRhs g) tks >> crlfbj

setRelativePos :: Pos -> State ControlWriter ()
setRelativePos (Pos ln cn _) = do 
  setVTab
  (Pos _ _ fn,_,_,_,cw) <- getControlInfo
  put $ cw { relative = Just (Pos ln cn fn) }

elemRelativePos :: HsToken -> State ControlWriter ()
elemRelativePos tk =
    do (cp@(Pos curLn curCol curFn), js@(Pos  jLn   jCol   _):jts, relM, ct, cw)  <- getControlInfo
       let rel@(Pos relLn relCol _)    = case relM of
                                           (Just v) -> v
                                           _        -> error "No relative position has been setting"
           rrP@(Pos rcLn  rcCol  _)    = hsTknPos     tk
           npos                        = Pos (jLn + (rcLn - relLn)) (jCol + (rcCol - relCol)) curFn
           evalPos
               | relLn == rcLn && relCol >= rcLn
                   = return ()
               | relLn == rcLn && relCol < rcLn
                   = crlf
               | relLn /= rcLn && relCol >= rcLn
                   = return ()
               | relLn /= rcLn && relCol < rcLn
                   = crlf
           hs = upTknPos tk npos
       evalPos
       put $ cw { currentPos  = npos
                , content     = ct Seq.>< Seq.singleton hs
                }

setHsTokenRhs :: (Identifier -> Identifier -> Pos -> HsToken -> HsToken)
              -> HsToken
              -> State ControlWriter ()
setHsTokenRhs g l@(AGField f a pos _) = elemRelativePos (g f    a pos l)
setHsTokenRhs g l@(AGLocal v pos _)   = elemRelativePos (g _LOC v pos l)
setHsTokenRhs _ hsTkn                 = elemRelativePos hsTkn

strPosPos :: String -> Pos -> Pos
strPosPos  _ = id

hsTknPos :: HsToken -> Pos
hsTknPos (AGLocal _ pos _)   = pos
hsTknPos (HsToken _ pos)     = pos
hsTknPos (CharToken _ pos)   = pos
hsTknPos (StrToken _ pos)    = pos
hsTknPos (Err _ pos)         = pos
hsTknPos (AGField _ _ pos _) = pos

upTknPos :: HsToken -> Pos -> HsToken
upTknPos (AGLocal a _   b)   pos = AGLocal a pos b
upTknPos (HsToken s _  )     pos = HsToken s pos
upTknPos (CharToken a _  )   pos = CharToken a pos
upTknPos (StrToken s _  )    pos = StrToken s pos
upTknPos (Err e _  )         pos = Err e pos
upTknPos (AGField f a _   b) pos = AGField f a pos b

crlf :: State ControlWriter ()
crlf = do (Pos ln _ fn, Pos _ cn _:jstrs, _, _,cw) <- getControlInfo
          let np = Pos (ln + 1) cn fn
          put $ cw { currentPos = np
                   , justify    = np:jstrs
                   }

crlfbj :: State ControlWriter ()
crlfbj = do (Pos cLn cCn fn, jst, _, _, cw) <- getControlInfo
            let Pos _ cn _:jstrs  = case jst of
                                        [x]      -> [x]
                                        (_:xss) ->  xss
                np                  = Pos (cLn + 1) cn fn
            put $ cw { currentPos = np
                     , relative   = Nothing
                     , justify    = np:jstrs
                     }

fromPatToHs :: (Identifier -> Identifier -> String) -> Pattern -> Pos -> (Pos,Seq HsToken)
fromPatToHs g (Constr name ps) pos  = let nn = getName name
                                          np = advc (length nn + 1) pos
                                          (np',ns) = inPat g ps np
                                      in  (np',Seq.singleton (HsToken nn pos) Seq.>< ns)
fromPatToHs g (Product _ ps)   pos  = let np = advc 1 pos
                                          (np',ns) = inPat g ps np
                                      in (advc 1 np', Seq.singleton (HsToken "(" pos)
                                                      Seq.>< ns
                                                      Seq.>< Seq.singleton (HsToken ")" np'))
fromPatToHs g (Alias f a p ps) pos  = let nt = g f a
                                          np = advc (length nt + 1) pos
                                          (np',ns') = case p of
                                                        (Underscore _) -> (np,Seq.empty)
                                                        _              -> fromPatToHs g p np
                                          (np'',ns'') = inPat g ps np
                                      in (advc 1 np'', Seq.singleton (HsToken nt pos)
                                                       Seq.>< ns'
                                                       Seq.>< ns'')
fromPatToHs g (Irrefutable pat) pos = let np         = advc 1 pos
                                          (np', ns') = fromPatToHs g pat np
                                      in (advc 1 np', Seq.singleton (HsToken "~" pos)
                                                      Seq.>< ns')
fromPatToHs g (Underscore _)    pos = (advc 1 pos, Seq.singleton (HsToken "_" pos))


setBottoms :: ((Pos -> State ControlWriter ())
               -> (FPAttrInfo -> Pos -> State ControlWriter ())
               -> Identifier
               -> Identifier
               -> Pos -> State ControlWriter ())
              -> Patterns
              -> State ControlWriter ()
setBottoms trs []     = return ()
setBottoms trs [x]    = setBottom trs x
setBottoms trs (x:ys) = setBottom trs x >> addText "," >> setBottoms trs ys

aBottom :: ((Pos -> State ControlWriter ())
            -> (FPAttrInfo -> Pos -> State ControlWriter ())
            -> Identifier
            -> Identifier
            -> Pos -> State ControlWriter ())
        -> Pattern
        -> State ControlWriter ()
aBottom trs bottom = setVTab >> setBottom trs bottom >> resetVTab

setBottom :: ((Pos -> State ControlWriter ())
              -> (FPAttrInfo -> Pos -> State ControlWriter ())
              -> Identifier
              -> Identifier
              -> Pos -> State ControlWriter ())
          -> Pattern
          -> State ControlWriter ()
setBottom tr (Constr name ps) = 
    do cp <- getCWInfo currentPos
       let nm  = getName name
           cp' = advc (length nm) cp
       updateContentPos cp' (HsToken nm cp)
       setBottoms tr ps
setBottom tr (Product _ ps) = setBottoms tr ps
setBottom tr (Alias f a p ps) =
    do cp <- getCWInfo currentPos
       let cp' = advc (length (getName f ++ getName a) + 2) cp
           df pdf = updateContentPos cp (AGField f a pdf Nothing)
           -- TODO Improve this part. What will do with pos values?
           ce fpInfo sps =  let (Expression pex tks) = bottomFP fpInfo
                            in setRelativePos pex >> mapM_ elemRelativePos tks
           hs  = tr df ce
       hs f a cp
       case p of
         (Underscore _) -> return ()
         _              -> setBottom tr p
       setBottoms tr ps
setBottom tr (Irrefutable p)  = 
    do cp <- getCWInfo currentPos
       let cp' = advc 1 cp
       updateContentPos cp' (HsToken "~" cp)
       setBottom tr p
setBottom tr (Underscore _)   = 
    do cp <- getCWInfo currentPos
       let cp' = advc 1 cp
       updateContentPos cp' (HsToken "_" cp)

inPat :: (Identifier -> Identifier -> String) -> Patterns -> Pos -> (Pos,Seq HsToken)
inPat g []       pos = (pos,Seq.empty)
inPat g [x]      pos = fromPatToHs g x pos
inPat g (x:y:ys) pos = let (np,ns)     = fromPatToHs g x pos
                           (np',ns')   = (advc 1 np, Seq.singleton (HsToken "," np))
                           (np'',ns'') = inPat g (y:ys) np'
                       in (np'',ns Seq.>< ns' Seq.>< ns'')


