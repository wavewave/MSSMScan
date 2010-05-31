{-# LANGUAGE BangPatterns #-}

module MSSMScan.OutputPhys where

import qualified Data.ByteString.Lazy as L

import Data.Binary 
import Data.List
import qualified Data.Map as M

import Control.Parallel.Strategies



-- | OutputPhys : output of SoftSUSY and MicrOmegas.                 
data OutputPhys = OutputPhys {
  mass_Mh   :: Double, 
  mass_MHH  :: Double,
  mass_MH3  :: Double,
  mass_MHc  :: Double,
  mass_MNE1 :: Double,
  mass_MNE2 :: Double,
  mass_MNE3 :: Double,
  mass_MNE4 :: Double,
  mass_MC1  :: Double,
  mass_MC2  :: Double,
  mass_MSG  :: Double,
  mass_MSuL :: Double,
  mass_MSdL :: Double,
  mass_MSeL :: Double,
  mass_MSne :: Double,
  mass_MSuR :: Double,
  mass_MSdR :: Double,
  mass_MSeR :: Double,
  mass_MScL :: Double,
  mass_MSsL :: Double,
  mass_MSmL :: Double,
  mass_MSnm :: Double,
  mass_MScR :: Double,
  mass_MSsR :: Double,
  mass_MSmR :: Double,
  mass_MSt1 :: Double,
  mass_MSb1 :: Double,
  mass_MSl1 :: Double,
  mass_MSn1 :: Double,
  mass_MSt2 :: Double,
  mass_MSb2 :: Double,
  mass_MSl2 :: Double,
  deltarho :: Double,
  gmuon     :: Double,
  bsgnlo    :: Double,
  bsmumu    :: Double,
  bino      :: Double,
  wino      :: Double,
  higgsino1 :: Double,
  higgsino2 :: Double, 
  micro_Xf  :: Double,
  micro_Omega :: Double
} deriving Eq

data MassType = Higgs | HeavyHiggs | AHiggs | CHiggs 
              | Neutralino1 | Neutralino2 | Neutralino3 | Neutralino4 
              | Chargino1 | Chargino2 
              | Gluino 
              | SupL | SdownL | SelectronL | SeneutrinoL
              | SupR | SdownR | SelectronR 
              | ScharmL | SstrangeL | SmuonL | SmuneutrinoL 
              | ScharmR | SstrangeR | SmuonR
              | Stop1 | Sbottom1 | Stau1 | Sneutrino3
              | Stop2 | Sbottom2 | Stau2 
              deriving (Show,Eq,Ord,Enum)

instance NFData MassType 

nulloutput = OutputPhys 0 0 0 0 0 0 0 0 0 0 
                        0 0 0 0 0 0 0 0 0 0 
                        0 0 0 0 0 0 0 0 0 0 
                        0 0 0 0 0 0 0 0 0 0 
                        0 0 
                        
instance Show OutputPhys where
  show x =    "Mh = "  ++ show (mass_Mh x) ++ ", "    
           ++ "MHH = " ++ show (mass_MHH x) ++ ", " 
           ++ "MH3 = " ++ show (mass_MH3 x) ++ ", " 
           ++ "MHc = " ++ show (mass_MHc x) ++ ", " 
           ++ "MNE1 = " ++ show (mass_MNE1 x) ++ ", " 
           ++ "MNE2 = " ++ show (mass_MNE2 x) ++ ", " 
           ++ "MNE3 = " ++ show (mass_MNE3 x) ++ ", " 
           ++ "MNE4 = " ++ show (mass_MNE4 x) ++ ", " 
           ++ "MC1 = " ++ show (mass_MC1  x) ++ ", " 
           ++ "MC2 = " ++ show (mass_MC2  x) ++ ", " 
           ++ "MSG = " ++ show (mass_MSG  x) ++ ", " 
           ++ "MSuL = " ++ show (mass_MSuL x) ++ ", " 
           ++ show (mass_MSdL x) ++ " " 
           ++ show (mass_MSeL x) ++ " " 
           ++ show (mass_MSne x) ++ " " 
           ++ show (mass_MSuR x) ++ " " 
           ++ show (mass_MSdR x) ++ " " 
           ++ show (mass_MSeR x) ++ " " 
           ++ show (mass_MScL x) ++ " " 
           ++ show (mass_MSsL x) ++ " " 
           ++ show (mass_MSmL x) ++ " " 
           ++ show (mass_MSnm x) ++ " " 
           ++ show (mass_MScR x) ++ " " 
           ++ show (mass_MSsR x) ++ " " 
           ++ show (mass_MSmR x) ++ " " 
           ++ "mass_MSt1=" ++ show (mass_MSt1 x) ++ ", " 
           ++ show (mass_MSb1 x) ++ " " 
           ++ "mass_MSl1=" ++ show (mass_MSl1 x) ++ ", " 
           ++ show (mass_MSn1 x) ++ " " 
           ++ show (mass_MSt2 x) ++ " " 
           ++ show (mass_MSb2 x) ++ " " 
           ++ show (mass_MSl2 x) ++ ", " 
           ++ "deltarho =" ++ show (deltarho  x) ++ ", " 
           ++ "gmuon =" ++ show (gmuon     x) ++ ", " 
           ++ "bsgnlo =" ++ show (bsgnlo    x) ++ ", " 
           ++ "bsmumu =" ++ show (bsmumu    x) ++ ", " 
           ++ "bino = " ++ show (bino      x) ++ ", " 
           ++ "wino = " ++ show (wino      x) ++ ", " 
           ++ "higgsino = " ++ show (higgsino1 x) ++ ", " 
           ++ "higgsino = " ++ show (higgsino2 x) ++ ", "  
           ++ "micro_Xf = " ++ show (micro_Xf  x) ++ ", " 
           ++ "micro_Omega = " ++ show (micro_Omega x)  

instance Binary OutputPhys where
  put x =     put (mass_Mh x) >> put (mass_MHH x) >> put (mass_MH3 x) >> put (mass_MHc x) 
              >> put (mass_MNE1 x) >> put (mass_MNE2 x) >> put (mass_MNE3 x) >> put (mass_MNE4 x) 
              >> put (mass_MC1  x) >> put (mass_MC2  x) >> put (mass_MSG  x) >> put (mass_MSuL x) 
              >> put (mass_MSdL x) >> put (mass_MSeL x) >> put (mass_MSne x) >> put (mass_MSuR x) 
              >> put (mass_MSdR x) >> put (mass_MSeR x) >> put (mass_MScL x) >> put (mass_MSsL x) 
              >> put (mass_MSmL x) >> put (mass_MSnm x) >> put (mass_MScR x) >> put (mass_MSsR x) 
              >> put (mass_MSmR x) >> put (mass_MSt1 x) >> put (mass_MSb1 x) >> put (mass_MSl1 x) 
              >> put (mass_MSn1 x) >> put (mass_MSt2 x) >> put (mass_MSb2 x) >> put (mass_MSl2 x)
              >> put (deltarho  x) >> put (gmuon     x) >> put (bsgnlo    x) >> put (bsmumu    x) 
              >> put (bino      x) >> put (wino      x) >> put (higgsino1 x) >> put (higgsino2 x)
              >> put (micro_Xf  x) >> put (micro_Omega x) 
  get   = do  { mass_Mh'   <- get ; mass_MHH'  <- get ; mass_MH3'  <- get ; mass_MHc'  <- get  
              ; mass_MNE1' <- get ; mass_MNE2' <- get ; mass_MNE3' <- get ; mass_MNE4' <- get 
              ; mass_MC1'  <- get ; mass_MC2'  <- get ; mass_MSG'  <- get ; mass_MSuL' <- get 
              ; mass_MSdL' <- get ; mass_MSeL' <- get ; mass_MSne' <- get ; mass_MSuR' <- get
              ; mass_MSdR' <- get ; mass_MSeR' <- get ; mass_MScL' <- get ; mass_MSsL' <- get
              ; mass_MSmL' <- get ; mass_MSnm' <- get ; mass_MScR' <- get ; mass_MSsR' <- get
              ; mass_MSmR' <- get ; mass_MSt1' <- get ; mass_MSb1' <- get ; mass_MSl1' <- get
              ; mass_MSn1' <- get ; mass_MSt2' <- get ; mass_MSb2' <- get ; mass_MSl2' <- get
              ; deltarho'  <- get ; gmuon'     <- get ; bsgnlo'    <- get ; bsmumu'    <- get    
              ; bino'      <- get ; wino'      <- get ; higgsino1' <- get ; higgsino2' <- get
              ; micro_Xf'  <- get ; micro_Omega' <- get 
              ; return $ OutputPhys  mass_Mh'   mass_MHH'  mass_MH3'  mass_MHc'     
                                     mass_MNE1' mass_MNE2' mass_MNE3' mass_MNE4'   
                                     mass_MC1'  mass_MC2'  mass_MSG'  mass_MSuL'   
                                     mass_MSdL' mass_MSeL' mass_MSne' mass_MSuR'  
                                     mass_MSdR' mass_MSeR' mass_MScL' mass_MSsL'  
                                     mass_MSmL' mass_MSnm' mass_MScR' mass_MSsR'  
                                     mass_MSmR' mass_MSt1' mass_MSb1' mass_MSl1'  
                                     mass_MSn1' mass_MSt2' mass_MSb2' mass_MSl2'  
                                     deltarho'  gmuon'     bsgnlo'    bsmumu'         
                                     bino'      wino'      higgsino1' higgsino2'  
                                     micro_Xf'  micro_Omega'  }
    


makeassocmass :: OutputPhys -> [(MassType,Double)]
makeassocmass x =   ( Higgs      , mass_Mh   x ) 
                  : ( HeavyHiggs , mass_MHH  x )
                  : ( AHiggs     , mass_MH3  x )
                  : ( CHiggs     , mass_MHc  x )
                  : ( Neutralino1, mass_MNE1 x )
                  : ( Neutralino2, mass_MNE2 x )
                  : ( Neutralino3, mass_MNE3 x )
                  : ( Neutralino4, mass_MNE4 x )
                  : ( Chargino1  , mass_MC1  x ) 
                  : ( Chargino2  , mass_MC2  x )
                  : ( Gluino     , mass_MSG  x )
                  : ( SupL       , mass_MSuL x )
                  : ( SdownL     , mass_MSdL x )
                  : ( SelectronL , mass_MSeL x )
                  : ( SeneutrinoL, mass_MSne x )
                  : ( SupR       , mass_MSuR x )
                  : ( SdownR     , mass_MSdR x )
                  : ( SelectronR , mass_MSeR x )
                  : ( ScharmL    , mass_MScL x )
                  : ( SstrangeL  , mass_MSsL x )
                  : ( SmuonL     , mass_MSmL x ) 
                  : ( SmuneutrinoL, mass_MSnm x )
                  : ( ScharmR    , mass_MScR x )
                  : ( SstrangeR  , mass_MSsR x )
                  : ( SmuonR     , mass_MSmR x )
                  : ( Stop1      , mass_MSt1 x )
                  : ( Sbottom1   , mass_MSb1 x )
                  : ( Stau1      , mass_MSl1 x ) 
                  : ( Sneutrino3 , mass_MSn1 x ) 
                  : ( Stop2      , mass_MSt2 x ) 
                  : ( Sbottom2   , mass_MSb2 x )
                  : ( Stau2      , mass_MSl2 x )
                  : []

sortmassassoc :: [(MassType,Double)] -> [(MassType,Double)]
sortmassassoc lst = sortBy sortfun lst 
    where sortfun (_,x) (_,y) = compare x y

rparityodd_massassoc :: [(MassType,Double)] -> [(MassType,Double)]
rparityodd_massassoc = filter (isrparityodd.fst) 

nonSM_massassoc :: [(MassType,Double)] -> [(MassType,Double)]
nonSM_massassoc = filter (isNonSM.fst) 

isNonSM :: MassType -> Bool 
isNonSM Higgs = False
isNonSM _     = True


isrparityodd :: MassType -> Bool 
isrparityodd Higgs      = False
isrparityodd HeavyHiggs = False
isrparityodd AHiggs     = False
isrparityodd CHiggs     = False
isrparityodd _          = True

