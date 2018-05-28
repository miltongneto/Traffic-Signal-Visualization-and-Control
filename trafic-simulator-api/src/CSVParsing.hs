module CSVParsing 
   (readDataFromCSV, parseData, elemIndex, toChar) where

import Text.ParserCombinators.Parsec
import System.IO

csvFile = endBy line eol
line = sepBy cell (char ';')
cell = many (noneOf ";\r\n")
eol = string "\r\n"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

readDataFromCSV = do
    inh <- openFile "data/semaforos.csv" ReadMode
    l <- readLoop inh []
    hClose inh
    return l


toChar :: [Char] -> Char
toChar [] = 'N'
toChar (x:xs) = x

elemIndex :: [a] -> Int -> a
elemIndex (x:xs) 0 = x
elemIndex (x:xs) i = elemIndex xs (i-1)

concatStrings :: [String] -> String
concatStrings [] = ""
concatStrings (x:xs) = x ++ (concatStrings xs)

parseData :: [String] -> Either ParseError [[String]]
parseData (x:[]) = do parseCSV x
parseData (x:xs) = parseCSV (concatStrings xs)

readLoop :: Handle -> [String] -> IO [String]
readLoop inh l = do
    ineof <- hIsEOF inh
    if ineof
       then return l
       else do
           inStr <- hGetLine inh
           readLoop inh (l ++ [inStr ++ "\r\n"])
