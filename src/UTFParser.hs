module UTFParser where

import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8, 
                                              decodeUtf16LE, decodeUtf16BE, 
                                              decodeUtf32LE, decodeUtf32BE)
import           Data.ByteString             (ByteString)
import           Data.ByteString             as ByteString (readFile)
import           Text.Parsec                 (ParseError, parse)
import           Text.Parsec.Text            (Parser)

import           Control.Monad               (liftM)

type Decoder = ByteString -> Text

parseFromUTFFile :: Decoder -> Parser a -> FilePath -> IO (Either ParseError a)
parseFromUTFFile d p fn = return . parse p "" . d =<< ByteString.readFile fn 
{-
             do bs <- BS.readFile fn
                let t = d bs 
                return $ parse p "" t
-}

parseFromUTF8File = parseFromUTFFile decodeUtf8 
parseFromUTF16LEFile = parseFromUTFFile decodeUtf16LE
parseFromUTF16BEFile = parseFromUTFFile decodeUtf16BE
parseFromUTF32LEFile = parseFromUTFFile decodeUtf32LE
parseFromUTF32BEFile = parseFromUTFFile decodeUtf32BE
