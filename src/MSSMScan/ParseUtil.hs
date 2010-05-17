{-# OPTIONS_GHC -fglasgow-exts #-}

module MSSMScan.ParseUtil where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)


import Data.Function
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B

myint :: Parser Int 
myint = do rest <- many1 (oneOf "0123456789")
           return $ read rest

myroughfloat :: Parser Double
myroughfloat = do result <- many1 (oneOf "+-0123456789.e")
                  let result' = correct_double_str result 
                                --if last result == '.' 
                                --then result ++ "0"
                                --else result
                  return $ read result'

correct_double_str :: String -> String 
correct_double_str str = let (x,y) = break ( == '.') str
                         in if y == []  
                            then str
                            else if y == ['.']
                                 then x ++ ".0"
                                 else if y !! 1 == 'e'
                                      then x ++ ".0" ++ tail y
                                      else str 

empty :: Parser [Char]
empty = many1 (oneOf emptyletters)

emptyletters :: [Char]
emptyletters = [' ']
