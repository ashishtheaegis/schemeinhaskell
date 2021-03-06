module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
main :: IO ()
main = getArgs >>= putStrLn . show . eval .  readExpr . (!! 0)

symbol :: Parser Char
symbol = oneOf "!@#$%^&*+_-=<?>~:/|"

spaces :: Parser ()
spaces = skipMany1 space

-- input is the parameter 
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of 
  	Left err -> String  $ "No match: " ++ show err
	Right val -> val 
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
 
parseExpr :: Parser LispVal
parseExpr    = 	parseAtom
            <|> parseString
            <|> parseNumber
	    <|> parseQuoted
	    <|> do 
		char '('
		x <- (try parseList <|> parseDottedList)
		char ')'
		return x	

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal



instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val 
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args)  $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
	    ("-", numericBinop (-)),
	    ("*", numericBinop (*)),
	    ("/", numericBinop div),
	    ("mod", numericBinop mod),
	    ("quotient", numericBinop quot),
	    ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params =  Number  $ foldl1 op $ map  unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n 
unpackNum (String n) = let parsed = reads n in 
			if null parsed 
				then 0
				else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0


import Control.Monad.Error

data LispError = NumArgs Integer [LispVal]
	| TypeMismatch String LispVal	
	| Parser ParseError
	| BadSpecialForm String LispVal
	| NotFunction String String
	| UnboundVar String String
	| Default String

showError :: LispError -> String
showError (Unboundvar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (TypeMismatch expected typeValfound) = "Type mismatch , expected type :" ++ expected ++  "typevalue found "++ show typeValfound
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected foundList) = "Incorrect number of arguments ,expected " ++ show expected ++ " found  " ++ show $ length foundLists
showError (Parser parseError) = "Parse error at : "++ show parseError 

instance Show LispError where show = showError

instance Error LispError where 
	noMsg = Default "An error has occurred"	
	strMsg = Default

type ThrowsError = Either LispError


trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a 
extractValue  (Right val) = val 







