{-# OPTIONS_GHC -fglasgow-exts #-}

module MSSMScan.Model.MSUGRA where

import MSSMScan.Model
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import MSSMScan.ParseUtil

import qualified Data.ByteString.Lazy as L
--import qualified Text.Show.ByteString as S

--- | InputMSUGRA = m0, m1/2, a0, tanb
-- newtype InputMSUGRA = 
--class ByteShow a where
--  byteshow :: a -> L.ByteString


instance Model MSUGRA where
    data ModelInput MSUGRA = IMSUGRA (Double,Double,Double,Double)
    lineInput MSUGRA  = 
        do m0   <- myroughfloat  
           empty
           m12  <- myroughfloat 
           empty
           a0   <- myroughfloat
           empty
           tanb <- myroughfloat
                
           many (noneOf "\n\r") 
           return $ IMSUGRA (m0,m12,a0,tanb)

type InputMSUGRA = ModelInput MSUGRA


instance Show InputMSUGRA where 
  show (IMSUGRA (m0,m12,a0,tanb)) =     show m0 ++ " " 
                                     ++ show m12 ++ " " 
                                     ++ show a0  ++ " " 
                                     ++ show tanb 

--instance ByteShow InputMSUGRA where
--  byteshow (IMSUGRA (m0,m12,a0,tanb)) = L.concat [S.show m0, S.show m12, S.show a0, S.show tanb]

