module Main where

import System.Environment
import System.Exit
import Control.Monad
import System.Console.ANSI
import Options.Applicative
import Data.Semigroup ((<>))

import Pixy.Eval
import Pixy.Delay
import Pixy.Syntax
import Pixy.Parser.Parser
import Pixy.PrettyPrint

data Options = Options 
    { file :: FilePath
    , count :: Maybe Int
    , dumpConstraints :: Bool
    }

options :: Parser Options
options = Options
    <$> argument str
        (metavar "FILENAME"
        <> help "The file to execute"
        )
    <*> (option $ optional auto)
        (short 'n'
        <> metavar "INT"
        <> help "The number of values to compute"
        <> value Nothing
        )
    <*> switch
        (long "constraints"
        <> short 'c'
        <> help "Print the constraints that are generated"
        )


main :: IO ()
main = exec =<< execParser opts
    where
        opts = info (options <**> helper)
            (fullDesc
            <> progDesc "Execute the file FILENAME"
            <> header "Pixy -- A simple dataflow language")

exec :: Options -> IO ()
exec o = do
    contents <- readFile $ file o
    case runParser program (file o) contents of
        Left err -> die err
        Right fs -> 
            if dumpConstraints o then do
                let cs = genConstraints =<< fs
                putStrLn "--[Constraints]--"
                mapM_ (putStrLn . pp) cs
                putStrLn "--[Reduced]--"
                -- either (\e -> printErr ("Constraint Error:\n" ++ pp e ++ "\n")) (mapM_ (putStrLn . pp)) $ reduce $ genConstraints fs
            else forM_ (go $ evalLoop fs (App "main" [])) display
    where go = case count o of
            Just n -> take n
            Nothing -> id

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