{-# LANGUAGE OverloadedStrings #-}

module Main where
import Prelude hiding (readFile, putStrLn)
import Data.ByteString.Char8 hiding (map)
import Data.Attoparsec.ByteString.Char8
import Data.Scientific (Scientific)
import Parser.TechFile
import Parser.Base
import Serialize
--




-- Parse line "element" and stop at \n
-- parseElement :: Parser [ByteString]
-- parseElement = do
--     string "element" <?> "expect 'element'"
--     skipSpace
--     x <- manyTill parseIdentifier endOfLine
--     -- x <- parseIdentifier `sepBy` skipSpace
--     return x


buildAttributeParser :: ByteString -> Parser [ByteString]
buildAttributeParser name = do
    skipSpace
    stringCI name <?> "expect " ++ unpack name
    skipSpace
    manyTill parseIdentifier endOfLine   <?> "expect line end character"

buildValueParser :: ByteString -> Parser [Scientific]
buildValueParser name = do
    skipSpace
    stringCI name <?> "expect " ++ unpack name
    skipSpace
    manyTill parseDatas endOfLine   <?> "expect line end character"

-- buildLibParser :: Parser [Model]
-- buildLibParser = do
--     x <- many1'


main :: IO ()
main = do
    content <- readFile "./data/test0"
    -- print $ parseOnly (char '.') content
    -- print $ parseOnly parseInt' "!123"
    -- print $ parseOnly parseHaha'' "..!!!...haha,haha,haha,haha"
    -- print $ parseOnly parseScientific "0.1a"
    -- print $ parseOnly (buildValueParser "id") "id      -554.8338n -277.4165n -277.4165n  277.4161n  277.4161n -493.0952n \n"
    -- print $ parseOnly (buildAttributeParser "model") "  model    0:pch.3    0:pch.4    0:pch.4    0:nch.4    0:nch.4    0:pch.3 \n"
    -- print $ parseOnly parseModel content
    let result = parseOnly parseModel content
    case result of
        Left err    -> putStrLn $ pack err
        Right model -> putStrLn $ serialize model
    -- print $ serialize result
    return ()

    -- parseTest parseParams content
