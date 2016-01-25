{-# LANGUAGE OverloadedStrings #-}

module Serialize where

import Parser.TechFile
import Data.ByteString.Char8 hiding (map)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
class Serialize a where
    serialize :: a -> ByteString

instance Serialize Int where
    serialize n = "haha"

instance Serialize Value where
    serialize (Numerical a) = pack $ show a
    serialize (Expression b) = b

instance Serialize Model where
    serialize (Model a b c) = a <> "\n" <> b <> "\n" <>
        intercalate "\n" (map serialize c)
--         where   serializeParam = pack . show

instance Serialize Parameter where
    serialize (Parameter name value) = name <> " = " <> serialize value

class Julialize a where
    julialize :: a -> ByteString

instance Julialize Parameter where
    julialize (Parameter name value) = name <> " = " <> serialize value

instance Julialize Model where
    julialize (Model mname mtype mcontent) =
        let repl '.' = '_'
            repl a   =  a
        in   (BS.map repl mname) <> " = DataFrame" <> "(" <>
        "Type = " <> mtype <>
        "\n," <> intercalate "\n," (map serialize mcontent) <> ")"
