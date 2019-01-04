module Main where

import Eval
import Syntax

example :: Term
example =
        Let "eff" Inst $
            Int 3


example' = Let "int" Inst $
           WithH (Handler (Var "int") ("x", Var"x") ("x", "k", Var"k" :@: (Int 4 :+: Var"x"))) $
               Perform (Var"int") (Int 3)

main :: IO ()
main = do
         print $ run example
         print $ run example'
