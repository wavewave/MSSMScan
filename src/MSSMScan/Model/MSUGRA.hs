{-# OPTIONS_GHC -fglasgow-exts #-}

module MSSMScan.Model.MSUGRA where

import MSSMScan.Model
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import MSSMScan.ParseUtil

--import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Lazy.Char8 as B
--import Data.ByteString.Internal
import Data.ByteString.Lex.Lazy.Double 

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
    parseInput oneline = let chunks = B.split ' ' oneline
                             a1:a2:a3:a4:[] = take 6 $ filter (not. B.null) chunks
                             m0   = maybe 0 fst $ readDouble a1
                             m12  = maybe 0 fst $ readDouble a2
                             a0   = maybe 0 fst $ readDouble a3 
                             tanb = maybe 0 fst $ readDouble a4
                         in IMSUGRA (m0,m12,a0,tanb)


    tanbeta (IMSUGRA (_,_,_,tanb)) = tanb
type InputMSUGRA = ModelInput MSUGRA


instance Show InputMSUGRA where 
  show (IMSUGRA (m0,m12,a0,tanb)) =     show m0 ++ " " 
                                     ++ show m12 ++ " " 
                                     ++ show a0  ++ " " 
                                     ++ show tanb 

--instance ByteShow InputMSUGRA where
--  byteshow (IMSUGRA (m0,m12,a0,tanb)) = L.concat [S.show m0, S.show m12, S.show a0, S.show tanb]

