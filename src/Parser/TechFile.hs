{-# LANGUAGE OverloadedStrings #-}

module Parser.TechFile where
import Data.ByteString.Char8 hiding (map)
import Data.Attoparsec.ByteString.Char8
import Parser.Base
import Data.Scientific (Scientific)

--
----
data Model = Model ByteString ByteString [(ByteString, Parameter)]
    deriving (Show)

modelName :: Model -> ByteString
modelName (Model name _ _) = name

modelType :: Model -> ByteString
modelType (Model _ mtype _) = mtype

modelContent :: Model -> [(ByteString, Parameter)]
modelContent (Model _ _ content) = content
----
data Parameter = Numerical Scientific | Expression ByteString
    deriving (Show)

--
parseParamName :: Parser ByteString
parseParamName = fmap pack $ do
                x <- many1' (satisfy $ \c -> (not . isSpace) c && c /= '=')
                skipMany (char ' ')
                return x

parseParamLine :: Parser [(ByteString, Parameter)]
parseParamLine = do
    stringCI ".param"    <?> "expected '.param'"
    x <- parseParams
    return x

parseParams :: Parser [(ByteString, Parameter)]
parseParams = many1' $ do
        skipMany (satisfy $ \c -> isSpace c || c == '\n' || c == '+')
        param <- parseParam
        return param

parseParam :: Parser (ByteString, Parameter)
parseParam = do
    x <- parseParamName
    char '='
    skipSpace
    y <- choice
        [   fmap Numerical parseScientific
        ,   fmap Expression parseParamName
        ]
    return (x, y)

parseModel :: Parser Model
parseModel = do
    stringCI ".model"   <?> ".model"
    skipSpace
    mName <- parseIdentifier    <?> "expected model Name"
    skipSpace
    mType <- parseIdentifier    <?> "expected model Type"
    skipSpace
    char '('                    <?> "expected a opening bracket"
    skipSpace
    mContent <- parseParams     <?> "expected a lot of Parameters"
    skipSpace
    char ')'                    <?> "expected a closing bracket"
    return $ Model mName mType mContent
