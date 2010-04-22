{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE BangPatterns #-}

module MSSMScan.Print where


import Debug.Trace

import MSSMScan.DMM
import MSSMScan.OutputPhys
import MSSMScan.Parse

import Data.Function
import qualified Data.Map as M

import Control.Monad.State.Lazy


---- latex print 

type LaTeX = String 

latexname :: M.Map MassType LaTeX 
latexname = M.fromList [ (Higgs,"h")
                       , (HeavyHiggs,"H")
                       , (AHiggs, "A") 
                       , (CHiggs, "H^\\pm")
                       , (Neutralino1, "\\chi^0_1")
                       , (Neutralino2, "\\chi^0_2")
                       , (Neutralino3, "\\chi^0_3")
                       , (Neutralino4, "\\chi^0_4")
                       , (Chargino1, "\\chi^\\pm_1")
                       , (Chargino2, "\\chi^\\pm_2")
                       , (Gluino, "\\tilde{g}")
                       , (SupL, "\\tilde{u}_L")
                       , (SdownL, "\\tilde{d}_L")
                       , (SelectronL, "\\tilde{e}_L")
                       , (SeneutrinoL, "\\tilde{\\nu}_{eL}")
                       , (SupR, "\\tilde{u}_R")
                       , (SdownR, "\\tilde{d}_R")
                       , (SelectronR, "\\tilde{e}_R")
                       , (ScharmL, "\\tilde{c}_L")
                       , (SstrangeL, "\\tilde{s}_L")
                       , (SmuonL, "\\tilde{\\mu}_L")
                       , (SmuneutrinoL, "\\tilde{\\nu}_{\\mu L}")
                       , (ScharmR, "\\tilde{c}_R")
                       , (SstrangeR, "\\tilde{s}_R")
                       , (SmuonR, "\\tilde{\\mu}_R")
                       , (Stop1, "\\tilde{t}_1")
                       , (Sbottom1, "\\tilde{b}_1")
                       , (Stau1, "\\tilde{\\tau}_1")
                       , (Sneutrino3, "\\tilde{\\nu}_3")
                       , (Stop2, "\\tilde{t}_2")
                       , (Sbottom2, "\\tilde{b}_2")
                       , (Stau2, "\\tilde{\\tau}_2") ]

class LaTeXPrint a where
    latexprint :: a -> String 

instance LaTeXPrint MassType where
    latexprint ty = let name = M.lookup ty latexname  
                    in  maybe "" id name 


latexprintlist :: (LaTeXPrint a) => String -> [a] -> String 
latexprintlist splitter []  = ""
latexprintlist splitter [x] = latexprint x
latexprintlist splitter (x:xs) = latexprint x ++ splitter 
                                  ++ latexprintlist splitter xs 

instance LaTeXPrint Pattern where
    latexprint patt = "$" ++ latexprintlist "<" patt ++ "$"


instance LaTeXPrint (Pattern,Int) where
    latexprint (p,i) = latexprint p ++ "&" ++ show i 

instance LaTeXPrint PatternOccurrenceList where
    latexprint (PO lst) = "\\begin{tabular}{|c|c|} \n \\hline \n"
                           ++ latexprintlist "\\\\\n" lst  
                           ++ "\\\\\n\\hline \n \\end{tabular} \n"
