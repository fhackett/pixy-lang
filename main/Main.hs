module Main where

import System.Environment
import System.Exit
import Pixy.Eval
import Pixy.Syntax
import Pixy.Parser


main :: IO ()
main = do
    fname <- head <$> getArgs
    contents <- readFile fname
    case runParser program fname contents of
        Left err -> die err
        Right fs -> evalLoop fs (App "main" [])