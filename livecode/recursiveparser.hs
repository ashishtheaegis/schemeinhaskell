module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!@#$%^&*+_-=<?>~:/|"

spaces :: Parser ()
spaces = skipMany1 space

-- input is the parameter 
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
  	Left err -> "No match: " ++ show err
	Right val -> "Found value"
-- What values a data type might have, seperated via |           
  -- each constructor defines the construcor name and the value type
data LispVal = Atom String
             | List [LispVal]  
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
parseString :: Parser LispVal               
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x
                 
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of 
                 "#t" -> Bool True
                 "#f" -> Bool False
                 otherwise -> Atom atom
                 
                 
parseNumber :: Parser LispVal               
parseNumber = liftM (Number . read) $ many1 digit



parseList :: Parser LispVal
parseList = liftM List  $ (sepBy  parseExpr spaces)



parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
	    <|> parseQuoted
	    <|> do char '('
			x <- (try parseList <|> parseDottedList)
			char ')'
			return x	

--parseList :: Parser LispVal
--parseList = liftM List $ sepBy  parseExpr spaces 

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail
  
parseQuoted :: Parser LispVal  
parseQuoted = do 
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
  