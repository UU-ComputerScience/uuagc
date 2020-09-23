module JSON where

import           Text.Show

-- |Abstract syntax of JSON values. Does not cover all allowed JSON values.
-- Notably, Double is not as general as JSON's arbitrary sized floating point numbers.
data Value
  = Object  [(String, Value)]
  | Array   [Value]
  | String  String
  | Double  Double
  | Integer Integer
  | Bool    Bool
  | Null

-- |Very basic JSON encoding function. Should always produce valid JSON.
encode :: Value -> String
encode v = go v "" where
  go v = case v of
    Null      -> ("null" ++)
    Bool    b -> if b then ("true" ++) else ("false" ++)
    Integer n -> shows n
    Double n | isNaN n || isInfinite n -> ("null" ++)
             | otherwise               -> shows n
    String s  -> shows s
    Array  xs -> brackets (commaSeparated (map go xs))
    Object xs ->
      braces (commaSeparated (map (\(k, v) -> shows k . (':' :) . go v) xs))

  brackets x = ('[' :) . x . (']' :)
  braces x = ('{' :) . x . ('}' :)

  commaSeparated []       = id
  commaSeparated (x : xs) = x . foldr (\y ys -> (',' :) . y . ys) id xs

-- |Represent tagged data types, should be compatible with aeson.
tagged :: String -> [Value] -> Value
tagged tag contents = case contents of
  []  -> Object [("tag", String tag)]
  [x] -> Object [("tag", String tag), ("contents", x)]
  xs  -> Object [("tag", String tag), ("contents", Array xs)]