module Main where

import RepMinHS

tree = Tree_Bin (Tree_Bin (Tree_Leaf 3) (Tree_Leaf 4)) 
       (Tree_Bin (Tree_Leaf 1) (Tree_Leaf 9))

root = Root_Root tree

main :: IO ()
main = do let wr = (wrap_Root (sem_Root root) Inh_Root)
          print $ min_Syn_Root wr
          print $ tree_Syn_Root wr
          print $ avg_Syn_Root wr

