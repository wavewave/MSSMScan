{-# OPTIONS_GHC -fglasgow-exts #-}

module MSSMScan.DMM where

import qualified Data.ByteString.Lazy as L
--import qualified Text.Show.ByteString as S

import Data.Binary 

-- | InputDMM = NMess, MMess, M0, alpham, alphag, tanb
newtype InputDMM = IDMM (Double,Double,Double,Double,Double,Double)

instance Show InputDMM where 
  show (IDMM (nmess,mmess,m0,alpham,alphag,tanb)) 
      = show nmess     ++ " " 
        ++ show mmess  ++ " " 
        ++ show m0     ++ " " 
        ++ show alpham ++ " "
        ++ show alphag ++ " " 
        ++ show tanb 

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
  
