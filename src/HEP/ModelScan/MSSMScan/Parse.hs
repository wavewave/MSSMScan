module HEP.ModelScan.MSSMScan.Parse where

import Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lex.Lazy.Double 


import HEP.ModelScan.MSSMScan.Model

import HEP.Physics.MSSM.OutputPhys



isRight (Left x) = False 
isRight (Right x) = True

unRight (Right x) = x
unRight (Left x) = undefined

isJust Nothing = False
isJust _       = True
unJust (Just x) = x
unJust Nothing  = undefined

 
mergeresult :: [(Int,a)] -> [(Int,b)]
               -> [(Int,(a,b))]
mergeresult [] _ = []
mergeresult _ [] = []
mergeresult (x@(idx,restx):xs) (y@(idy,resty):ys) = 
  if idx == idy 
    then restx `seq` resty `seq` (idx,(restx,resty)) : mergeresult xs ys
    else if idx < idy 
           then mergeresult xs (y:ys)
           else [] 



parseOutput :: B.ByteString -> Maybe (Int,OutputPhys)
parseOutput ostr = do let chunks = B.split ' ' ostr
                          (a1:a2:a3:a4:a5:a6:a7:a8:a9:a10
                                :a11:a12:a13:a14:a15:a16:a17:a18:a19:a20
                                :a21:a22:a23:a24:a25:a26:a27:a28:a29:a30
                                :a31:a32:a33:a34:a35:a36:a37:a38:a39:a40
                                :a41:a42:a43:a44) = filter (not. B.null) chunks
                          myint    x = do y <- B.readInt x
                                          return (fst y)
                          mydouble x = do y <- readDouble x
                                          if (not. B.null .snd) y
                                            then do y' <- readDouble (tempsol x)
                                                    return (fst y')
                                            else return (fst y)
                          tempsol str = let (fore,rear) = B.break (=='.') str
                                            str' = fore `B.append` B.pack ".0" `B.append` B.tail rear
                                        in str'

                      id' <- myint a1
                      let id = if (id' `mod` 10000 == 0) 
                                 then trace ("id = " ++ show id') id'
                                 else id' 
                      data_Mh   <- mydouble a2
                      data_MHH  <- mydouble a3
                      data_MH3  <- mydouble a4
                      data_MHc  <- mydouble a5
                      data_MNE1 <- mydouble a6
                      data_MNE2 <- mydouble a7
                      data_MNE3 <- mydouble a8
                      data_MNE4 <- mydouble a9
                      data_MC1  <- mydouble a10
                      data_MC2  <- mydouble a11
                      data_MSG  <- mydouble a12
                      data_MSuL <- mydouble a13
                      data_MSdL <- mydouble a14
                      data_MSeL <- mydouble a15
                      data_MSne <- mydouble a16
                      data_MSuR <- mydouble a17
                      data_MSdR <- mydouble a18
                      data_MSeR <- mydouble a19
                      data_MScL <- mydouble a20
                      data_MSsL <- mydouble a21
                      data_MSmL <- mydouble a22
                      data_MSnm <- mydouble a23
                      data_MScR <- mydouble a24 
                      data_MSsR <- mydouble a25
                      data_MSmR <- mydouble a26
                      data_MSt1 <- mydouble a27
                      data_MSb1 <- mydouble a28
                      data_MSl1 <- mydouble a29
                      data_MSn1 <- mydouble a30
                      data_MSt2 <- mydouble a31
                      data_MSb2 <- mydouble a32
                      data_MSl2 <- mydouble a33
                      data_deltarho <- mydouble a34
                      data_gmuon  <- mydouble a35
                      data_bsgnlo <- mydouble a36
                      data_bsmumu <- mydouble a37
                      data_bino   <- mydouble a38
                      data_wino   <- mydouble a39
                      data_higgsino1 <- mydouble a40
                      data_higgsino2 <- mydouble a41
                      data_micro_Xf <- mydouble a42
                      data_micro_Omega <- mydouble a43 
                         
                       
                      let output = OutputPhys data_Mh   data_MHH  data_MH3  data_MHc
                                              data_MNE1 data_MNE2 data_MNE3 data_MNE4
                                              data_MC1  data_MC2  data_MSG  data_MSuL
                                              data_MSdL data_MSeL data_MSne data_MSuR
                                              data_MSdR data_MSeR data_MScL data_MSsL
                                              data_MSmL data_MSnm data_MScR data_MSsR
                                              data_MSmR data_MSt1 data_MSb1 data_MSl1
                                              data_MSn1 data_MSt2 data_MSb2 data_MSl2
                                              data_deltarho data_gmuon data_bsgnlo data_bsmumu
                                              data_bino data_wino data_higgsino1 data_higgsino2
                                              data_micro_Xf data_micro_Omega


                      return (id, output)

newparsestr :: (Model a) => a -> B.ByteString -> B.ByteString 
         -> [(Int,(ModelInput a,OutputPhys))] 
newparsestr mdl str1 str2 = 
          let strlines1 = B.lines str1 
              inputresult = {-# SCC "inputresult" #-} zip [1..] $ map parseInput strlines1
    
             
              strlines2 = B.lines str2 
                                

              outputresult'   = map (parseOutput) strlines2

              outputresult''  = filter isJust outputresult'
              outputresult    = map unJust outputresult''
                                
              combinedresult  = mergeresult inputresult outputresult 
          in  combinedresult


