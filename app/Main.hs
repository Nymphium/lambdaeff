module Main where

import Eval
import Parse (parse)
import Syntax
import System.IO
import Text.Trifecta.Result

evalStringM :: (Monad m) => m String -> (String -> m a) -> m a
evalStringM str k = do
        t <- parse <$> str
        case t of
            Success t       -> k $ show $ run t
            err@(Failure _) -> k $ show err

repl :: IO ()
repl = do
    putStr "λeff=> " >> hFlush stdout
    evalStringM getLine putStrLn
    repl

main :: IO ()
main = repl
