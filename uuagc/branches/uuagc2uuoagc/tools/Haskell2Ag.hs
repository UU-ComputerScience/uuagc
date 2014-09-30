{-
 - This tool can extract Haskell datatypes and generate AG definitions
 - for those datatypes.
 -}
import Language.Haskell.Exts
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: ./Haskell2Ag <file.hs>"
    else do
      res <- parseFile (head args)
      let Module _ _ _ _ _ _ decls = fromParseResult res
      mapM_ extractData decls

extractData :: Decl -> IO ()
extractData (DataDecl _ _ _ (Ident name) _ constr _) = do
  putStrLn $ "data " ++ name
  mapM_ showCons constr
  putStrLn ""
extractData (TypeDecl _ (Ident name) _ (TyList (TyCon (UnQual (Ident tp))))) = do
  putStrLn $ "type " ++ name ++ " = [" ++ tp ++ "]"
extractData (TypeDecl _ (Ident name) _ (TyTuple _ [TyCon (UnQual (Ident tp1)),TyCon (UnQual (Ident tp2))])) = do
  putStrLn $ "type " ++ name ++ " = (" ++ tp1 ++ "," ++ tp2 ++ ")"
extractData _ = return ()

showCons :: QualConDecl -> IO ()
showCons (QualConDecl _ _ _ (ConDecl (Ident name) children)) = do
  putStrLn $ "  | " ++ name
  mapM_ showChild $ zip [Ident $ "ch" ++ show n | n <- [1..]] children
showCons (QualConDecl _ _ _ (RecDecl (Ident name) children)) = do
  putStrLn $ "  | " ++ name
  mapM_ showChild $ [(nm,tp) | (nms,tp) <- children, nm <- nms]
showCons _ = return ()

showChild :: (Name, BangType) -> IO ()
showChild (Ident name, tp) = do
  putStrLn $ "      " ++ name ++ " :: " ++ showType tp

showType :: BangType -> String
showType (UnBangedTy (TyCon (UnQual (Ident nm)))) = nm
showType (UnBangedTy (TyParen ty)) = showType (UnBangedTy ty)
showType tp = "{" ++ prettyPrint tp ++ "}"