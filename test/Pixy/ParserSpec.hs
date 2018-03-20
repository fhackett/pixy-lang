module Pixy.ParserSpec where

import Test.Hspec
import Pixy.Parser
import Pixy.Syntax
import Pixy.PrettyPrint
import System.Directory

spec :: Spec
spec = do
    -- simpleExprs
    samples

-- simpleExprs :: Spec
-- simpleExprs = do
--     describe "Simple Expressions" $ do
--         addExpr
--         arithExpr

-- addExpr :: Spec
-- addExpr = do
--     it "add expression" $ do
--         runParser expr "<stdin>" "x + y" `shouldBe` (Right (BinExpr Plus (Var "x") (Var "y")))

-- arithExpr :: Spec
-- arithExpr = do
--     it "operator precedence" $ do
--         runParser expr "<stdin>" "x + 3*(y - 4)" `shouldBe` 
--             (Right (BinExpr Plus (Var "x") (BinExpr Times (Const 3) (BinExpr Minus (Var "y") (Const 4)))))

samples :: Spec
samples = do
    it "can parse samples" $ do
        files <- listDirectory "samples"
        sequence_ $ (\f -> sample $ "samples/" ++ f)<$> files

sample :: String -> IO ()
sample fname = do
    putStrLn $ "=====[Parsing File: " ++ fname ++ "]====="
    contents <- readFile fname
    putStrLn contents
    case runParser program fname contents of
        Left err -> expectationFailure err
        Right res -> do
            putStrLn "=====[Result]====="
            mapM_ (print) res
            -- mapM_ (putStrLn . pp) res

isLeft :: Either e a -> Bool
isLeft (Left _) = True
isLeft (Right _) = False



