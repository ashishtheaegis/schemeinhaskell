module Main where 
import System.Environment

import Text.ParserCombinators.Parsec hiding (spaces)
main :: IO()
main = do args <- getArgs
          putStrLn("Hello, " ++ (args !! 0) ++ " , and " ++ (args !! 1))
          
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?@^_~#"


readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of 
  Left err -> 