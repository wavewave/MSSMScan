{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE BangPatterns #-}

module MSSMScan.Read where

import Debug.Trace

import System.IO
import System.Environment

import Data.Int
import Data.List

import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B
import qualified Data.Binary.Get as G


import Control.Parallel
import Control.DeepSeq

import MSSMScan.Model
import MSSMScan.Model.MSUGRA
import MSSMScan.Model.DMM
import MSSMScan.OutputPhys
import MSSMScan.Parse
import MSSMScan.Print 

import MSSMScan.Pattern


import Data.Function
import qualified Data.Map as M

import Control.Monad.State.Strict


data PatternSwitch = ROdd4 | NonSM7 | NonSM4




class PrettyPrintable a where
    pretty_print :: a -> IO ()

instance PrettyPrintable PatternOccurrenceList where
    pretty_print (PO lst) = mapM_ putStrLn lst' 
        where lst' = map formatting lst
              formatting x = show (fst x) ++ ":" ++ show (snd x)


type PatternHandleMap = M.Map Pattern Handle


data Count a = Count { totalcount :: Int
                     , pattcount  :: PatternCountMap
                     , pattlist   :: (PatternModelMap a) 
                     , handlelist :: PatternHandleMap
                     , fileprefix :: String
                     }

type CountState a = StateT (Count a) IO

--type CountState a= State (Count a)

pattType :: (Model a) => PatternSwitch -> FullModel a -> Pattern
pattType sw fm = case sw of 
                   ROdd4  -> take 4 $ tidyup_1st_2nd_gen $ roddsort fm
                   NonSM7 -> take 7 $ tidyup_1st_2nd_gen $ nonsmsort fm 
                   NonSM4 -> take 4 $ tidyup_1st_2nd_gen $ nonsmsort fm



classifyPattern :: (Model a) => Pattern -> (FullModel a) -> CountState a ()
classifyPattern patt fullmodel = 
    do stat <- get

       let prefix = fileprefix stat
           hdl    = handlelist stat
       let hdllkup = M.lookup patt hdl 

       case hdllkup of 
         Nothing -> do newhdl <- liftIO $ 
                                 do putStrLn ("new pattern " ++ show patt)
                                    let fn = prefix ++ show patt ++ ".dat" 
                                             
                                    h <- openFile fn WriteMode
                                    hPutStrLn h $ print_fullmodel fullmodel
                                    return $ M.insert patt h hdl
                       put $ stat {handlelist = newhdl}
                       
                                

         Just h  -> liftIO $ hPutStrLn h $ print_fullmodel fullmodel 

print_fullmodel fullmodel = show (idnum fullmodel) ++ " : " ++ 
                            show (inputparam fullmodel) ++ " | " ++ 
                            show (outputphys fullmodel)



addPatternModelMap :: (Model a) => PatternSwitch -> PatternModelMap a -> FullModel a -> PatternModelMap a
addPatternModelMap sw pmm fm = addPatternModel (pattType sw fm) fm pmm


feed_single_fullmodel_list :: (Model a) => PatternSwitch -> [FullModel a] -> CountState a ()
feed_single_fullmodel_list sw fmlst = 
    do mapM_  onefilestep fmlst 
    where onefilestep fullmodel 
              = do stat <- get 
                   let acclen    = totalcount stat
                       accmap    = pattcount  stat 
--                       hdl       = handlelist stat

                       patt      = pattType sw fullmodel
                       accmap'   = addPattern patt accmap 
                       acclen'   = acclen + 1 


                   classifyPattern patt fullmodel
                   stat' <- get 

                   acclen' `seq`  accmap'  `seq`  -- accmdllst' `seq`
                        put $ stat' { totalcount = acclen' ,
                                      pattcount  = accmap' } 


prettyprint :: (Model a) => FullModel a -> IO ()
prettyprint x = putStrLn $ "id =" ++ show (idnum x) ++ " : " 
                           ++ show (inputparam x) 
                           ++ " : " ++ show ( take 7 $ (sortmassassoc.makeassocmass.outputphys) x )


assoc_sort :: (Model a) => (Int,(ModelInput a,OutputPhys)) -> FullModel a  
assoc_sort (id1,(input1,output1)) = FullModel id1 input1 output1 
                                              fullassoc rparityassoc nonsmassoc
    where fullassoc    =  map fst $ (sortmassassoc.makeassocmass) output1
          rparityassoc =  filter isrparityodd fullassoc
          nonsmassoc   =  filter isNonSM fullassoc


--- tidyup sort 
tidyup_1st_2nd_gen list = 
    let (lst1,lst2) = break isFstOrSndGen list
    in if lst2 == [] 
       then list
       else let (headtype, headtypelst) = headcheck $ head lst2 
                (lst1',lst2') = span (flip elem headtypelst) lst2
            in  lst1 ++ headtype : tidyup_1st_2nd_gen lst2'

headcheck x | x `elem` kind_SupL   = (SupL  , kind_SupL)
            | x `elem` kind_SdownL = (SdownL, kind_SdownL)
            | x `elem` kind_SupR   = (SupR  , kind_SupR)
            | x `elem` kind_SdownR = (SdownR, kind_SdownR)
            | x `elem` kind_SleptonL = (SelectronL, kind_SleptonL)
            | x `elem` kind_SleptonR = (SelectronR, kind_SleptonR)
            | x `elem` kind_Sneutrino = (SeneutrinoL, kind_Sneutrino)
            | otherwise = (undefined,undefined)

fstOrSndGen = [SupL,SupR,SdownL,SdownR,SstrangeL,SstrangeR,ScharmL,ScharmR
              ,SelectronL,SelectronR,SmuonL,SmuonR,SeneutrinoL,SmuneutrinoL]

isFstOrSndGen x = elem x fstOrSndGen 

kind_SupL   = [SupL,ScharmL]
kind_SdownL = [SdownL,SstrangeL]
kind_SupR   = [SupR,ScharmR]
kind_SdownR = [SdownR,SstrangeR]
kind_SleptonL = [SelectronL,SmuonL]
kind_SleptonR = [SelectronR,SmuonR]
kind_Sneutrino = [SeneutrinoL,SmuneutrinoL]

 


--- cut functions.


applycut :: (Model a) => [(OutputPhys -> Bool)] 
         -> [(FullModel a-> Bool) ]
         -> FullModel a
         -> Bool
applycut cuts compcuts x = let boollst1 = map (\f->f ph) cuts
                               boollst2 = map (\f->f x) compcuts 
                           in     and boollst1
                               && and boollst2
    where ph   = outputphys x  


---- pattern ordering 
priority :: M.Map MassType Int 
priority = M.fromList [ (Neutralino1,1), (Neutralino2,2), (Chargino1,3)
                      , (Stau1,4), (Gluino,5), (HeavyHiggs,6), (AHiggs,7)
                      , (CHiggs,8), (Stop1,9), (SelectronR,9)  ] 

massorder x y = let lookupresult = (M.lookup x priority, M.lookup y priority)
                in case lookupresult of 
                     (Nothing,Nothing) -> compare x y 
                     (Nothing,Just _)  -> LT
                     (Just _, Nothing) -> GT
                     (Just a, Just b)  -> compare b a 


patternorder (x1:x2:xs) (y1:y2:ys) = let x1y1order = flip massorder x1 y1
                                         x2y2order = flip massorder x2 y2
                                     in  case x1y1order of
                                           EQ        -> x2y2order
                                           otherwise -> x1y1order 

patternorder _ _ = EQ

patternoccorder (patt1,occ1) (patt2,occ2) = 
    let patorder = patternorder patt1 patt2
    in case patorder of 
         EQ        -> compare occ2 occ1
         otherwise -> patorder

