{-# OPTIONS_GHC -fglasgow-exts #-}

module MSSMScan.Model where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator

import qualified Data.ByteString.Lazy.Char8 as B

import MSSMScan.OutputPhys

data DMM    = DMM
data MSUGRA = MSUGRA

class (Show (ModelInput a)) => Model a where
    data ModelInput a  
    lineInput :: a -> Parser (ModelInput a) 
    parseInput :: B.ByteString -> (ModelInput a)
    tanbeta :: (ModelInput a) -> Double

data (Model a) => FullModel a = FullModel {
      idnum      :: Int
    , inputparam :: (ModelInput a) 
    , outputphys :: OutputPhys 
    , fullsort   :: [MassType] 
    , roddsort   :: [MassType] 
    , nonsmsort  :: [MassType]
    }

