module Main where

import Eval
import Parse (parse)
import Syntax
import System.IO
import Text.Trifecta.Result

repl :: IO ()
repl = do
    putStr "λeff=> " >> hFlush stdout
    line <- getLine
    let astOrErr = parse line
    _ <- case astOrErr of
         Success t       -> print $ run t
         err@(Failure _) -> print err
    repl

main :: IO ()
main = repl
