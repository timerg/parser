-- https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md

{-# LANGUAGE OverloadedStrings #-}

module Parser.Base where

import Prelude hiding (readFile)
import Data.ByteString.Char8 hiding (map)
import Data.Attoparsec.ByteString.Char8     -- https://hackage.haskell.org/package/attoparsec-0.13.0.1/docs/Data-Attoparsec-ByteString-Char8.html
import Data.Word                            -- https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Word.html
import qualified Data.Scientific as Sci
import Data.Scientific (Scientific)         -- https://hackage.haskell.org/package/scientific-0.3.4.4/docs/Data-Scientific.html#t:Scientific

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
    x <- scientific     -- get "<- Parser Scientific" = Scientific
    char prefix         -- get Parser Char
    return $ adjustBase x b
-- When reviewing this function: think b as a fix number. For its number is decide by Char at func:parseScientific

adjustBase :: Scientific -> Int -> Scientific   -- Int is  the value of a,f,p...
adjustBase x n = Sci.scientific c (b + n)
    where b = Sci.base10Exponent x      -- ex: x = 3 * 10^4.  b=4, c = 3
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

parseDatas :: Parser Scientific
parseDatas = do
    x <- parseScientific
    skipMany (char ' ')
    return x

-- Parse and get rid of trailing space
parseIdentifier :: Parser ByteString
parseIdentifier = fmap pack $ do
                x <- many1' (notChar ' ')
                skipMany (char ' ')
                return x

parseComment :: Parser ByteString
parseComment = do
                char '*'
                x <- takeTill $ \c -> c == '\n'
                return x
