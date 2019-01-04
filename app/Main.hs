module Main where

import Eval
import Syntax

example :: Term
example =
        Let "eff" Inst $
            Int 3

main :: IO ()
main = print $ show $ run example
