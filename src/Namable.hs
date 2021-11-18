module Namable where
import Language.C (Ident, SUERef, Pretty(pretty))
import Language.C.Analysis (VarName)
import Language.Haskell.TH (mkName, Name)
import Data.Char (toUpper)

class Namable a where
  toString :: a -> String

instance Namable Ident where
  toString = show . pretty
instance Namable SUERef where
  toString = show . pretty
instance Namable VarName where
  toString = show . pretty


toName :: Namable a => a -> Name
toName = mkName . capFirst . mkNice . toString

toLowerName :: Namable a => a -> Name
toLowerName = mkName . mkNice . toString

mkNice :: String -> String
mkNice ('$':xs) = "anon" ++ xs
mkNice name = name

capFirst :: String -> String
capFirst [] = error "Invalid name"
capFirst (x:xs) = toUpper x : xs
