-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec
-- http://haddocks.fpcomplete.com/fp/7.8/20140916-162/attoparsec/Data-Attoparsec-Types.html#t:Parser
module Main where
import System.IO
import Data.Char (isSpace)
import Data.List
-- import Data.Attoparsec.Types
import Data.ByteString.Char8   -- pack and unpack [Char]
import Data.Attoparsec.ByteString.Char8
import Data.Word



--
-- isComma :: Char -> Bool
-- isComma x = x == ','
--
-- split :: (a -> Bool) -> [a] -> ([a], [a])
-- split p xs = (first, rest)
--         where first =              takeWhile (not . p) xs
--               rest  = dropWhile p (dropWhile (not . p) xs)
--
--
-- splitAll :: (a -> Bool) -> [a] -> [[a]]
-- splitAll p xs = if null b then [a] else a:splitAll p b
--         where (a, b) = split p xs
--
-- trim :: String -> String
-- trim = reverse . dropSpace . reverse . dropSpace
--     where dropSpace = dropWhile isSpace
--
-- compact :: [String] -> [String]
-- compact xs = filter (not . null) xs
--
-- splitNewline :: String -> [String]
-- splitNewline a = compact $ map trim (splitAll (== '\n') a)
--
-- splitComma :: String -> [String]
-- splitComma a = compact $ map trim (splitAll isComma a)


-- Type for MOS
data MOS = MOS String String String String String String String String String String String String String String String String String String String String String String String deriving (Show)
-- data MOS = MOS name N/P Region Ibs Ibd Vgs Vds Vbs Vth Vdsat Vod Beta Gameff Gm Gds Gmb Cdtot Cgtot Cstot Cbtot Cgs Cgd | PMOS Region Ibs Ibd Vgs Vds Vbs Vth Vdsat Vod Beta Gameff Gm Gds Gmb Cdtot Cgtot Cstot Cbtot Cgs Cgd deriving (Show)
mosmodel :: MOS -> String
mosmodel (MOS _ model _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = model
mosregion :: MOS -> String
mosregion (MOS _ _ region _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = region

-- parseMOS :: Parser MOS
-- parseMOS = do
--     string "element"
--     n1   <- string
--     m1   <- string
--     r1   <- string
--     ibs1 <- string
--     ibd1 <- string
--     vgs1 <- string
--     vds1 <- string
--     vbs1 <- string
--     vth1 <- string
--     vdt1 <- string
--     vod1 <- string
--     bta1 <- string
--     gef1 <- string
--     gm1  <- string
--     gds1 <- string
--     gmb1 <- string
--     cdt1 <- string
--     cgt1 <- string
--     cst1 <- string
--     cbt1 <- string
--     cgs1 <- string
--     cgd1 <- string
--
--     return $ MOS n1 m1 r1 ibs1 ibd1 vgs1 vds1 vbs1 vth1 vdt1 vod1 bta1 gef1 gm1 gds1 gmb1 cdt1 cgt1 cst1 cbt1 cgs1 cgd1

data IP = IP Word8 Word8 Word8 Word8 deriving (Show, Eq)
--
parseIP :: Parser IP
parseIP = do
    d1 <- decimal
    char '.'
    d2 <- decimal
    char '.'
    d3 <- decimal
    char '.'
    d4 <- decimal
    return $ IP d1 d2 d3 d4

--
eg = pack "140.113.210.41"

main :: IO ()
main = do
    print $ parseOnly parseIP eg

-- line' :: String -> [String]
-- line' "" = []
-- line' a =
-- line' a = head' : (line' tail')
--     where   head' = takeWhile (/= ',') a
--             tail' = dropWhile (== ',') a
-- main :: IO ()
-- main = do
--     content <- readFile "data/test0"
    -- dropWhile (/= )
    -- print $ splitNewline content >>= splitComma >>= words
    -- print $ concat $ map splitComma $ splitNewline content
