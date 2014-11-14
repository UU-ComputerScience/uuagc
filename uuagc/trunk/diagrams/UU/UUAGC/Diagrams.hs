{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Example usage of this package:
-- 
-- > import UU.UUAGC.Diagrams
-- > 
-- > dia :: AGBackend b => AGDiagram b
-- > dia = production ["count", "level"] "Docs" ["html", "count"]
-- >         [ child ["count", "level"] "hd" ["html", "count"]
-- >         , child ["count", "level"] "tl" ["html", "count"]
-- >         ]
-- >       # agrule shaftL "lhs.count" "hd.count"
-- >       # agrule shaftL "lhs.level" "hd.level"
-- >       # agrule shaftR "lhs.level" "tl.level"
-- >       # agrule shaftL "hd.html" "lhs.html"
-- >       # agrule shaftR "tl.html" "lhs.html"
-- >       # agrule shaftR "tl.count" "lhs.count"
-- >       # agrule shaftT "hd.count" "tl.count"
--
-----------------------------------------------------------------------------

module UU.UUAGC.Diagrams
       (production, child, agrule,
        shaftL, shaftR, shaftT, shaftB, shaftD,
        (#),
        AGDiagram, AGBackend, Child) where

import Diagrams.Prelude
import Graphics.SVGFonts (textSVG_, Spacing (..), TextOpts (..), lin2, Mode (..))
import Data.List (isPrefixOf)

-- | Construct a diagram for a full production, given its inherited attributes,
--   name, synthesized attributes and children
production :: AGBackend b =>
              [String] -> String -> [String] -> [Child b] -> AGDiagram b
production = node True

-- | Child with backend @b@, this type has been left abstract on purpose.
newtype Child b = Child { unChild :: AGDiagram b }

-- | Construct a child given its inherited attributes, name and sythesized
--   attributes.
child :: AGBackend b => [String] -> String -> [String] -> Child b
child i n s = Child $ node False i n s []

-- | Construct an arrow between two attributes. The first argument specifies
--   the shape of the arrow and can be 'shaftL', 'shaftR', 'shaftT', 'shaftB'
--   of 'shaftD', or a special trial constructed with the diagrams library.
agrule :: AGBackend b =>
          Trail R2 -> String -> String -> AGDiagram b -> AGDiagram b
agrule sh s1 s2 = connectPerim' (with & headLength .~ (Normalized 0.025) & arrowShaft .~ sh) n1 n2 (tb t1) (tb t2) where
  t1 = "lhs." `isPrefixOf` s1
  t2 = "lhs." `isPrefixOf` s2
  n1 | '.' `notElem` s1 = s1 -- terminal
     | t1               = s1 ++ ".inh"
     | otherwise        = s1 ++ ".syn"
  n2 = if t2 then s2 ++ ".syn" else s2 ++ ".inh"
  tb False  = 90 @@ deg
  tb True   = 270 @@ deg

shaftL, shaftR, shaftT, shaftB, shaftD :: Trail R2

-- | Line that first moves left and then right
shaftL = fromSegments [bezier3 (r2 (0.5,0.3)) (r2 (0.5,-0.3)) (r2 (1,0))]

-- | Line that first moves right and then left
shaftR = fromSegments [bezier3 (r2 (0.5,-0.3)) (r2 (0.5,0.3)) (r2 (1,0))]

-- | Top half of a circle
shaftT = arcCW (0 @@ turn) (3/5 @@ turn)

-- | Bottom half of a circle
shaftB = arc (0 @@ turn) (2/5 @@ turn)

-- | Straight line
shaftD = straightShaft


-- A bit ugly, but now user doesn't need to import diagrams package for just the types
type AGDiagram b = Diagram b R2
class (Renderable (Path R2) b, Backend b R2) => AGBackend b where
instance (Renderable (Path R2) b, Backend b R2) => AGBackend b


attr :: AGBackend b =>
        String -> Bool -> (String -> String) -> AGDiagram b
attr s t f = stack t (unitSquare # named (f s) # lc black) (text' 0.7 s) where
  stack True  a b = beside          unitY  a (b === strutY 0.2)
  stack False a b = beside (negateV unitY) a (strutY 0.2 === b)

-- | Helper function for drawing a node
node :: AGBackend b =>
        Bool -> [String] -> String -> [String] -> [Child b] -> AGDiagram b
node top inh s syn ch = res # applyAll lines where
  res = toprow
        ===
        (if null ch then mempty else strutY 2)
        ===
        (hcats 1.5 $ map unChild ch) # centerX
  lines = alines ++ chLines
  chLines = [ line name (getName $ unChild c) # lc grey | c <- ch ]
  hcats s = hcat' (with & sep .~ s)
  els = inhs ++ [lhs] ++ syns
  toprow = beside unitX (
             beside (negateV unitX) lhs
               (hcats 0.3 inhs ||| strutX 0.3))
             (strutX 0.3 ||| hcats 0.3 syns)
  inhs = map (\i -> attr i top (\n -> name ++ "." ++ n ++ ".inh")) inh
  syns = map (\s -> attr s top (\n -> name ++ "." ++ n ++ ".syn")) syn
  alines = zipWith line (map getName els) (map getName $ tail els) # lc grey
  name = if top then "lhs" else s
  lhs = beside (negateV unitY) (
          beside unitY
          (circle 0.5 # named name # lc grey)
          (if top then (text' 0.9 s === strutY 0.1) else mempty))
        (strutY 0.1 === text' 0.9 name)

text' :: AGBackend b =>
         Double -> String -> AGDiagram b
text' d s = (textSVG_ (TextOpts s lin2 INSIDE_H KERN False d d)) # lw none # fc black # centerX

line :: (IsName n1, IsName n2, AGBackend b) =>
        n1 -> n2 -> AGDiagram b -> AGDiagram b
line a b = connectOutside' (with & arrowHead .~ noHead) a b

getName :: AGDiagram b -> Name
getName = fst . head . names
