{-# OPTIONS_GHC -fglasgow-exts #-}

module MSSMScan.Model where

import Data.Typeable

import qualified Data.ByteString.Lazy.Char8 as B

import HEP.Physics.MSSM.Type

data DMM    = DMM
              deriving Typeable
data MSUGRA = MSUGRA
            deriving Typeable

class (Show (ModelInput a), Typeable a) => Model a where
    data ModelInput a  
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

