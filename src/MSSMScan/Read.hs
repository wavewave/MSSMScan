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

import MSSMScan.DMM
import MSSMScan.OutputPhys
import MSSMScan.Parse
import MSSMScan.Print 
import MSSMScan.Cuts

import Data.Function
import qualified Data.Map as M

import Control.Monad.State.Lazy

data FullModel = FullModel {
      idnum      :: Int
    , inputparam :: InputDMM 
    , outputphys :: OutputPhys 
    , fullsort   :: [MassType] 
    , roddsort   :: [MassType] 
    , nonsmsort  :: [MassType]
    }

data PatternSwitch = ROdd4 | NonSM7



processonefile :: PatternSwitch -> String -> String -> CountState ()   
processonefile sw str1 str2 = 

    do (count,patmap) <- get

       let simplelst = parsestr str1 str2
           fulllst   = map assoc_sort simplelst 

            -- Baris cut
--           cutfilter = applycut masscut_baris indirect_baris {-bogus_cut-}
           
           -- Wisc mass cut with m_h > 100 and no indirect
           cutfilter = applycut bogus_cut
                       
                                {-- (\x -> higgs100cut x && 
                                       masscut_wisc_other_than_higgs x) --}
                                bogus_cut 
           cutlst    = filter cutfilter fulllst

       trace ("length simplelst = " ++ show (length simplelst)) $ feed_single_fullmodel_list sw cutlst


mai' = do arglist <- getArgs
          let swstr = arglist !! 0 
              outfilename = arglist !! 1
              sw = case swstr of
                     "R4"   -> ROdd4
                     "NSM7" -> NonSM7 
                   
--              (num :: Int) = read $ arglist !! 1



          let scanfile x = "scan" ++ show x ++ ".dat"
              microfile x = "micro" ++ show x ++ ".dat"
            
          handle_out <- openFile outfilename WriteMode

          handle_scan  <- mapM (\x->openFile (scanfile x)  ReadMode) [1,2]
          handle_micro <- mapM (\x->openFile (microfile x) ReadMode) [1,2]

          content_scan'  <- mapM hGetContents handle_scan
          content_micro' <- mapM hGetContents handle_micro

          let contents = zip content_scan' content_micro' 

{--
          let contents = zip (map (take 100000)  content_scan') 
                             (map (take 1000000) content_micro') --}


          let test ctt = processonefile sw (fst ctt) (snd ctt)
              totalaction = mapM_ test contents
              (lencut10,patmap10) = execState totalaction (0,M.empty)

          hPutStrLn handle_out $ latexprint $ PO $ sortBy patternoccorder (M.toList patmap10)
          hPutStrLn handle_out $ show lencut10 

          mapM_ hClose handle_scan
          mapM_ hClose handle_micro
          hClose handle_out



addPatternFromFullModel :: PatternSwitch -> PatternCountMap -> FullModel -> PatternCountMap
addPatternFromFullModel sw pcm fm = addPattern patt pcm
    where patt = case sw of 
                   ROdd4  -> take 4 $ tidyup_1st_2nd_gen $ roddsort fm
                   NonSM7 -> take 7 $ tidyup_1st_2nd_gen $ nonsmsort fm 



class PrettyPrintable a where
    pretty_print :: a -> IO ()

instance PrettyPrintable PatternOccurrenceList where
    pretty_print (PO lst) = mapM_ putStrLn lst' 
        where lst' = map formatting lst
              formatting x = show (fst x) ++ ":" ++ show (snd x)

type CountState = State (Int,PatternCountMap) 

feed_single_fullmodel_list :: PatternSwitch -> [FullModel] -> CountState ()
feed_single_fullmodel_list sw fmlst = do stat <- get 
                                         put $ foldl' onefilestep stat fmlst 
    where onefilestep (acclen, accmap) item 
              = let accmap' = addPatternFromFullModel sw accmap item
                    acclen' = acclen + 1 
                in  acclen' `seq` accmap'   `seq`  (acclen', accmap')


prettyprint :: FullModel -> IO ()
prettyprint x = putStrLn $ "id =" ++ show (idnum x) ++ " : " 
                           ++ show (inputparam x) 
                           ++ " : " ++ show ( take 7 $ (sortmassassoc.makeassocmass.outputphys) x )


assoc_sort :: (Int,(InputDMM,OutputPhys)) -> FullModel  
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


applycut :: (OutputPhys -> Bool) -> (OutputPhys -> Bool) -> FullModel 
            -> Bool
applycut f g x = (head rodd == Neutralino1) && (f ph) && (g ph) 
    where ph   = outputphys x  
          rodd = roddsort x



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

