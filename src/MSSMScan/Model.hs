{-# OPTIONS_GHC -fglasgow-exts #-}

module MSSMScan.Model where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator


data DMM    = DMM
data MSUGRA = MSUGRA

class (Show (ModelInput a)) => Model a where
    data ModelInput a  
    lineInput :: a -> Parser (ModelInput a) 

