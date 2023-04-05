{- |
Module      : Main
Description : Essai de lecture de fichier au format UTF8
Copyright   : (c) Benoit Fraikin @ Solutions Lambda, 2015
License     : GPL-3
Maintainer  : solutions.lambda@gmail.com
Stability   : experimental
-}
module Main where

import           Control.Monad          (liftM)
--
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BSC (pack, unpack)
import qualified Data.ByteString.UTF8   as BSU (fromString, toString)
--
import           Text.Parsec.Char       (char, noneOf)
import           Text.Parsec.Combinator (many1, sepBy)
import           Text.Parsec.Text       (Parser)
--
import           UTFParser              (parseFromUTF8File)

fromString = BSU.fromString
toString = BSU.toString
{-
fromString = BSC.pack
toString = BSC.unpack
-}

oldSep :: Char
oldSep = '.'

newSep :: Char
newSep = '-'

text :: Parser [ByteString]
text =  word `sepBy` separator

word :: Parser ByteString
word =  liftM fromString $ many1 $ noneOf $ pure oldSep

separator :: Parser Char
separator = char oldSep

main :: IO ()
main = convert "texte.txt"

convert :: String -> IO ()
convert filename = do file <- parseFromUTF8File text filename
                      print file
                      printResult file
  where printResult (Left err) = print err
        printResult (Right bs) = putStrLn $ concat $ map convert bs
        pre = return newSep
        convert = (pre++) . toString
