{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts,  
             DeriveDataTypeable, TypeSynonymInstances  #-}

module HEP.ModelScan.MSSMScan.Model.MSUGRA where

import HEP.ModelScan.MSSMScan.Model

import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lex.Lazy.Double 


instance Model MSUGRA where
    data ModelInput MSUGRA = IMSUGRA (Double,Double,Double,Double)
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
  show (IMSUGRA (m0,m12,a0,tanb)) =     "m0="   ++ show m0 ++ ", " 
                                     ++ "m12="  ++ show m12 ++ ", " 
                                     ++ "a0="   ++ show a0  ++ ", " 
                                     ++ "tanb=" ++ show tanb 


