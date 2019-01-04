module Main where

import Eval
import Syntax

example = Let "int" Inst $
           (:+:) (Int 3) $
           WithH (Handler (Var "int")
                   ("x", Var"x" :+: Int (-1))
                   ("x", "k", Var"k" :@: (Int 4 :+: Var"x"))) $
               Perform (Var"int") (Int 7) :*: Int 10 :+: Perform (Var"int") (Int 0)

example' = Let "p" Inst $
           Let "hp" (Handler (Var "p")
                       ("x", Var "x")
                       ("x", "k", Var"k" :@: Var "x")) $
           Let "q" Inst $
           Let "hq" (Handler (Var "q")
                      ("x", Var "x")
                      ("x", "k", Var"k" :@: Var "x")) $
           WithH (Var "hq") $
           WithH (Var "hp") $
               Perform (Var"p") (Int 1) :+: Perform (Var"q") (Int 2)

badExample = Let "p" Inst $
                WithH (Handler (Var "int")
                        ("x", Var "x")
                        ("x", "k", Var "x")) $
                  Perform Inst (Int 0)

main :: IO ()
main = do
         print $ run example
         print $ run example'
         print $ run badExample
