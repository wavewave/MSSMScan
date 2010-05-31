{-# OPTIONS_GHC -fglasgow-exts #-}

module MSSMScan.ParseUtil where

import Data.Function
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B


correct_double_str :: String -> String 
correct_double_str str = let (x,y) = break ( == '.') str
                         in if y == []  
                            then str
                            else if y == ['.']
                                 then x ++ ".0"
                                 else if y !! 1 == 'e'
                                      then x ++ ".0" ++ tail y
                                      else str 


emptyletters :: [Char]
emptyletters = [' ']
