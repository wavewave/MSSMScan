module MSSMScan.Pattern where

import qualified Data.Map as M
import MSSMScan.OutputPhys

type Pattern = [MassType] 
type PatternCountMap = M.Map Pattern Int 


addPattern :: Pattern -> PatternCountMap -> PatternCountMap 
addPattern patt pcm = M.insertWith' (+) patt 1 pcm 

newtype PatternOccurrenceList = PO [(Pattern,Int)] 
