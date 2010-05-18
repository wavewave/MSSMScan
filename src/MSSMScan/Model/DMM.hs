{-# OPTIONS_GHC -fglasgow-exts #-}

module MSSMScan.Model.DMM where

import MSSMScan.Model

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P 
import Text.Parsec.Language (haskellDef)

import MSSMScan.ParseUtil

import qualified Data.ByteString.Lazy.Char8 as B
--import Data.ByteString.Internal
import Data.ByteString.Lex.Lazy.Double 

--import qualified Text.Show.ByteString as S

import Data.Binary 


   
-- | InputDMM = NMess, MMess, M0, alpham, alphag, tanb
instance Model DMM where
    data ModelInput DMM =  IDMM (Double,Double,Double,Double,Double,Double)
    lineInput DMM = 
        do nmess  <- myroughfloat  
           empty
           mmess  <- myroughfloat 
           empty
           m0     <- myroughfloat
           empty
           alpham <- myroughfloat
           empty
           alphag <- myroughfloat
           empty
           tanb <- myroughfloat
                
           many (noneOf "\n\r") 
           return $ IDMM (nmess,mmess,m0,alpham,alphag,tanb)
    parseInput oneline = let chunks = B.split ' ' oneline
                             a1:a2:a3:a4:a5:a6:[] = take 6 $ filter (not. B.null) chunks
                             nmess  = maybe 0 fst $ readDouble a1
                             mmess  = maybe 0 fst $ readDouble a2
                             m0     = maybe 0 fst $ readDouble a3 
                             alpham = maybe 0 fst $ readDouble a4
                             alphag = maybe 0 fst $ readDouble a5
                             tanb   = maybe 0 fst $ readDouble a6
                         in IDMM (nmess,mmess,m0,alpham,alphag,tanb)

    tanbeta (IDMM (_,_,_,_,_,tanb)) = tanb

type InputDMM = ModelInput DMM

instance Show InputDMM where 
  show (IDMM (nmess,mmess,m0,alpham,alphag,tanb)) 
      =    "Nmess = " ++ show nmess ++ ", " 
        ++ "Mmess = " ++ show mmess ++ ", " 
        ++ "m0 = " ++ show m0 ++ ", " 
        ++ "alpham = " ++ show alpham ++ ", "
        ++ "alphag = " ++ show alphag ++ ", " 
        ++ "tanb = " ++ show tanb 

instance Binary InputDMM where
  put (IDMM (nmess,mmess,m0,alpham,alphag,tanb)) 
      = put nmess >> put mmess >> put m0 
        >> put alpham >> put alphag >> put tanb
  get = do nmess  <- get
           mmess  <- get
           m0     <- get
           alpham <- get 
           alphag <- get
           tanb   <- get
           return (IDMM (nmess,mmess,m0,alpham,alphag,tanb))
  
