module Pixy.ParserSpec where

import Test.Hspec
import Pixy.Parser
import System.Directory

spec :: Spec
spec = do
    describe "Pixy.Parser" $ do
        samples

samples :: Spec
samples = do
    it "can parse samples" $ do
        files <- listDirectory "samples"
        sequence_ $ (\f -> sample $ "samples/" ++ f)<$> files

sample :: String -> IO ()
sample fname = do
    putStrLn $ "Parsing File: " ++ fname
    contents <- readFile fname
    putStrLn "File Contents:"
    putStrLn contents
    case runParser expr fname contents of
        Left err -> expectationFailure err
        Right res -> do
            putStrLn "Result:"
            print res

isLeft :: Either e a -> Bool
isLeft (Left _) = True
isLeft (Right _) = False



