module Main where

import System.Environment
import System.Exit
import Control.Monad
import System.Console.ANSI
import Options.Applicative
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Data.List (find)

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

-- import Pixy.Eval
import Pixy.Pass.DelayPass
import Pixy.Pass.RenamePass
import Pixy.Syntax
import Pixy.Error
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
    contents <- T.readFile $ file o
    fs <- hoistErr $ runParser program (file o) contents
    putStrLn "--[Parsed]--"
    mapM_ display fs
    rnFs <- hoistErr $ renamePass fs
    putStrLn "--[Renamed]--"
    mapM_ display rnFs
    putStrLn "--[Delays]--"
    dlFs <- hoistErr $ delayPass rnFs
    mapM_ display dlFs
    -- c@(InferConstraints ecs cs) <- hoistErr $ genConstraints rnFs
    -- putStrLn "--[Equality Constraints]--"
    -- traverse_ (putStrLn . pp) ecs--(apply s cs)
    -- putStrLn "--[Other Constraints]--"
    -- traverse_ (putStrLn . pp) cs--(apply s cs)
    -- delays <- hoistErr $ solveConstraints c
    -- putStrLn $ pp delays

    -- case runParser program (file o) contents of
    --     Left err -> die err
    --     Right fs -> 
    --         if dumpConstraints o then do
    --             let cs = genConstraints fs mainF
    --             putStrLn "--[Constraints]--"
    --             traverse_ print cs
    --             putStrLn "--[Reduced]--"
    --             -- either (\e -> printErr ("Constraint Error:\n" ++ pp e ++ "\n")) (mapM_ (putStrLn . pp)) $ reduce $ genConstraints fs
    --         else forM_ (go $ evalLoop fs mainF) display
    -- where go = case count o of
    --         Just n -> take n
    --         Nothing -> id

display :: (Pretty a) => a -> IO ()
display p = T.putStrLn $ renderStrict $ layoutPretty defaultLayoutOptions $ pretty p

-- mainF :: Expr Rename
-- mainF = Where (Var "main") [("main", (App "main" []))]

-- showErr :: (Show e) => e -> IO ()
-- showErr err = printErr $ show err ++ "\n"

hoistErr :: Either ErrorMessage a -> IO a
hoistErr (Left err) = printErr err
hoistErr (Right a) = return a
 
printErr :: ErrorMessage -> IO a
printErr err = die $ T.unpack $ renderError err
        -- prefix = setSGRCode [SetColor Foreground Vivid Red]
        -- suffix = setSGRCode [Reset]