module MSSMScan.Pattern where

import qualified Data.Map as M
import MSSMScan.Model
import HEP.Physics.MSSM.Type


type Pattern = [MassType] 
type PatternCountMap = M.Map Pattern Int 
type PatternModelMap a = M.Map Pattern [FullModel a]


addPattern :: Pattern -> PatternCountMap -> PatternCountMap 
addPattern patt pcm = M.insertWith' (+) patt 1 pcm 

addPatternModel :: (Model a) => Pattern -> FullModel a -> PatternModelMap a -> PatternModelMap a
addPatternModel patt fm pmm = M.insertWith' (++) patt [fm] pmm 

newtype PatternOccurrenceList = PO [(Pattern,Int)] 
