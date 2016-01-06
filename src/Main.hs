{-# LANGUAGE OverloadedStrings #-}

module Main where
import Prelude hiding (readFile)
import Data.ByteString.Char8 hiding (map)
import Data.Attoparsec.ByteString.Char8
import Data.Word
import qualified Data.Scientific as Sci
import Data.Scientific (Scientific)

--
-- parseInt :: Parser Int
-- parseInt = do
--     n <- decimal
--     return $ n
--
-- parseInt' :: Parser Int
-- parseInt' = do
--     char '!'
--     n <- decimal
--     return $ n
--
-- parseBoth :: Parser Int
-- parseBoth = choice [parseInt, parseInt']
--
-- parseHaha :: Parser ByteString
-- parseHaha = string "haha"
--
-- parseHaha' :: Parser [ByteString]
-- parseHaha' = do
--      (string "haha") `sepBy` (char ',')
--
-- parseHaha'' :: Parser [ByteString]   --ignore "..."
-- parseHaha'' = do
--     skipMany (choice [char '.', char '!'])
--     parseHaha'
--
-- eg :: ByteString
-- eg = "14"


-- scientific :: Parser Scientific
-- Sci.scientific :: Integer -> Int -> Scientific
-- Sci.coefficient :: Scientific -> Integer
-- Sci.base10Exponent :: Scientific -> Int
-- parseNano :: Parser Scientific
-- parseNano = do
--     x <- scientific
--     char 'n'
--     return $ adjustBase x (-9)
--
-- parseAtto :: Parser Scientific
-- parseAtto = do
--     x <- scientific
--     char 'a'
--     return $ adjustBase x (-18)
--
-- parseMicro :: Parser Scientific
-- parseMicro = do
--     x <- scientific
--     char 'u'
--     return $ adjustBase x (-6)


buildMetricPrefixParser :: Char -> Int -> Parser Scientific
buildMetricPrefixParser prefix b = do
    x <- scientific
    char prefix
    return $ adjustBase x b

adjustBase :: Scientific -> Int -> Scientific
adjustBase x n = Sci.scientific c (b + n)
    where b = Sci.base10Exponent x
          c = Sci.coefficient x

parseScientific :: Parser Scientific
parseScientific = choice
    [   buildMetricPrefixParser 'a' (-18)
    ,   buildMetricPrefixParser 'f' (-15)
    ,   buildMetricPrefixParser 'p' (-12)
    ,   buildMetricPrefixParser 'n' (-9)
    ,   buildMetricPrefixParser 'u' (-6)
    ,   buildMetricPrefixParser 'm' (-3)
    ,   scientific              --always put this at last|always be placed at last
    ]

-- Parse and get rid of trailing space
parseIdentifier :: Parser ByteString
parseIdentifier = fmap pack parseIdentifierStr
    where   parseIdentifierStr = do
                x <- many1' (notChar ' ')
                skipMany (char ' ')
                return x
-- Parse line "element" and stop at \n
parseElement :: Parser [ByteString]
parseElement = do
    string "element" <?> "expect 'element'"
    skipSpace
    x <- manyTill parseIdentifier endOfLine <?> "banana"
    -- x <- parseIdentifier `sepBy` skipSpace
    return x

main :: IO ()
main = do
    content <- readFile "./data/test0"
    -- print $ parseOnly (char '.') content
    -- print $ parseOnly parseInt' "!123"
    -- print $ parseOnly parseHaha'' "..!!!...haha,haha,haha,haha"
    -- print $ parseOnly parseScientific "-287.11"
    print $ parseOnly parseElement "elenent alks    jj jj \n jdd"
