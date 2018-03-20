module Main where

import System.Environment
import System.Exit
import Pixy.Eval
import Pixy.Syntax
import Pixy.Parser
import Control.Monad
import System.Console.ANSI


main :: IO ()
main = do
    fname <- head <$> getArgs
    contents <- readFile fname
    case runParser program fname contents of
        Left err -> die err
        Right fs -> forM_ (evalLoop fs (App "main" [])) display

display :: Either EvalError Value ->  IO ()
display (Left err) = showErr err
display (Right (VInt k)) = print k 
display (Right (VBool b)) = print b
display (Right (VNil)) = putStrLn "nil"

showErr :: (Show e) => e -> IO ()
showErr err = printErr $ show err ++ "\n"
 
printErr :: String -> IO ()
printErr err = putStr $ prefix ++ err ++ suffix
    where
        prefix = setSGRCode [SetColor Foreground Vivid Red]
        suffix = setSGRCode [Reset]