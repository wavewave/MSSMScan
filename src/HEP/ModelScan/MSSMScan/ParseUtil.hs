{-# OPTIONS_GHC -fglasgow-exts #-}

module HEP.ModelScan.MSSMScan.ParseUtil where


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
