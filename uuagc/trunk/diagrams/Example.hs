module Example where

import UU.UUAGC.Diagrams

dia :: AGBackend b => AGDiagram b
dia = production ["count", "level"] "Docs" ["html", "count"]
        [ child ["count", "level"] "hd" ["html", "count"]
        , child ["count", "level"] "tl" ["html", "count"]
        ]
      # agrule shaftL "lhs.count" "hd.count"
      # agrule shaftL "lhs.level" "hd.level"
      # agrule shaftR "lhs.level" "tl.level"
      # agrule shaftL "hd.html" "lhs.html"
      # agrule shaftR "tl.html" "lhs.html"
      # agrule shaftR "tl.count" "lhs.count"
      # agrule shaftT "hd.count" "tl.count"
