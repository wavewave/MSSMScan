module HEP.ModelScan.MSSMScan.Pattern where

import qualified Data.Map as M
import HEP.ModelScan.MSSMScan.Model
import HEP.Physics.MSSM.OutputPhys


type Pattern = [MassType] 
type PatternCountMap = M.Map Pattern Int 
type PatternModelMap a = M.Map Pattern [FullModel a]


addPattern :: Pattern -> PatternCountMap -> PatternCountMap 
addPattern patt pcm = M.insertWith' (+) patt 1 pcm 

addPatternModel :: (Model a) => Pattern -> FullModel a -> PatternModelMap a -> PatternModelMap a
addPatternModel patt fm pmm = M.insertWith' (++) patt [fm] pmm 

newtype PatternOccurrenceList = PO [(Pattern,Int)] 
