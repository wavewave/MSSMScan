{-# OPTIONS_GHC -fglasgow-exts #-}

module MSSMScan.Parse where

import Debug.Trace


import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import MSSMScan.ParseUtil

import Data.Function
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lex.Lazy.Double 


import Control.Monad


import System.IO
import System.Environment

import MSSMScan.Model
import MSSMScan.Model.MSUGRA
import MSSMScan.Model.DMM
import MSSMScan.OutputPhys

{--main = do arglist <- getArgs

          let inputdmm  = arglist !! 0 
              outputdmm = arglist !! 1
              outbyte      = arglist !! 2

          inh1 <- openFile inputdmm ReadMode
          inh2 <- openFile outputdmm ReadMode
          outh <- openFile outbyte WriteMode

          str1 <- hGetContents inh1
          str2 <- hGetContents inh2 --}

parsestr :: (Model a) => a -> String -> String 
         -> [(Int,(ModelInput a, OutputPhys))]

parsestr mdl str1 str2 =
          let strlines1 = lines str1
              inputresult'  = zip [1..] $ map (run (lineInput mdl)) strlines1 
              inputresult'' = filter (isRight.snd) inputresult' 
              inputresult   = map (\x->(fst x, (unRight.snd) x)) 
                                  inputresult''
              
             
              strlines2 = lines str2 
              outputresult'   = map (run lineOutput) strlines2
              outputresult''  = filter isRight outputresult'
              outputresult''' = map unRight outputresult''
              outputresult''''= filter isJust outputresult'''
              outputresult    = map unJust outputresult''''
                                
              combinedresult =  mergeresult inputresult outputresult
          in  combinedresult


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

run :: Parser a -> String -> Either String a 
run p input = 
  case (parse p "" input) of 
    Left err -> Left  $ "parse error at " ++  show err ++ "\n"
    Right x  -> Right x


--lineInput :: (Model a) => a -> Parser (ModelInput a)


lineOutput :: Parser (Maybe (Int,OutputPhys))
lineOutput = do id <- myint
                empty 
                data_Mh <- myroughfloat
                empty
                data_MHH <- myroughfloat
                empty
                data_MH3 <- myroughfloat
                empty
                data_MHc <- myroughfloat
                empty
                data_MNE1 <- myroughfloat
                empty
                data_MNE2 <- myroughfloat
                empty
                data_MNE3 <- myroughfloat
                empty
                data_MNE4 <- myroughfloat
                empty
                data_MC1 <- myroughfloat
                empty
                data_MC2 <- myroughfloat
                empty
                data_MSG <- myroughfloat
                empty
                data_MSuL <- myroughfloat
                empty
                data_MSdL <- myroughfloat
                empty
                data_MSeL <- myroughfloat
                empty
                data_MSne <- myroughfloat
                empty
                data_MSuR <- myroughfloat
                empty
                data_MSdR <- myroughfloat
                empty
                data_MSeR <- myroughfloat
                empty
                data_MScL <- myroughfloat
                empty
                data_MSsL <- myroughfloat
                empty
                data_MSmL <- myroughfloat
                empty
                data_MSnm <- myroughfloat
                empty
                data_MScR <- myroughfloat
                empty
                data_MSsR <- myroughfloat
                empty
                data_MSmR <- myroughfloat
                empty
                data_MSt1 <- myroughfloat
                empty
                data_MSb1 <- myroughfloat
                empty
                data_MSl1 <- myroughfloat
                empty
                data_MSn1 <- myroughfloat
                empty
                data_MSt2 <- myroughfloat
                empty
                data_MSb2 <- myroughfloat
                empty
                data_MSl2 <- myroughfloat
                empty
                data_deltarho <- myroughfloat
                empty
                data_gmuon <- myroughfloat
                empty
                data_bsgnlo <- myroughfloat
                empty
                data_bsmumu <- myroughfloat
                empty
                data_bino   <- myroughfloat
                empty
                data_wino   <- myroughfloat
                empty
                data_higgsino1 <- myroughfloat
                empty
                data_higgsino2 <- myroughfloat
                empty
                checkend <- do try (myroughfloat >>= \data_micro_Xf -> 
                                    empty >> 
                                    myroughfloat >>= \data_micro_Omega -> 
                                    return $ Just (data_micro_Xf, data_micro_Omega)) 
                               <|> return Nothing
                 

                many (noneOf "\n\r")
  --              eol
                case checkend of 
                  Nothing    -> return Nothing 
                  Just (x,y) ->  
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
                                             x y
                     in return $ Just (id, output) 
                                                              


-- common routines 

--eol = try (string "\n\r")
--      <|> (string "\r\n")
--      <|> string "\n"
--      <|> string "\r"


parseOutput :: B.ByteString -> Maybe (Int,OutputPhys)
parseOutput ostr = do let chunks = B.split ' ' ostr
                          ~(a1:a2:a3:a4:a5:a6:a7:a8:a9:a10
                                :a11:a12:a13:a14:a15:a16:a17:a18:a19:a20
                                :a21:a22:a23:a24:a25:a26:a27:a28:a29:a30
                                :a31:a32:a33:a34:a35:a36:a37:a38:a39:a40
                                :a41:a42:a43:a44) = filter (not. B.null) chunks
                          myint    x = do y <- B.readInt x
                                          return (fst y)
                          mydouble x = do y <- readDouble x
                                          if (not. B.null .snd) y
                                            then {- trace ("x = " ++ show x ++ "\ntempsol :" ++ (show $ tempsol x) ++ "\n") -}
                                                 do y' <- readDouble (tempsol x)
                                                    {- trace ("y'= " ++ show y') -}
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
         -> [(Int,(ModelInput a,OutputPhys))] --- [(Int,(ModelInput a, OutputPhys))]

newparsestr mdl str1 str2 =
          let strlines1 = B.lines str1

              inputresult = zip [1..] $ map parseInput strlines1
   --            inputresult'' = filter (isRight.snd) inputresult' 
   --           inputresult   = map (\x->(fst x, (unRight.snd) x)) 
    --                              inputresult'' --}
              
             
              strlines2 = B.lines str2 
              parseOutput' x = let output = parseOutput x 
                               in if output == Nothing 
                                    then trace ("wrong output = " ++ (show x)) Nothing 
                                    else output 
                                  {--  else if (fst.unJust) output == 81767 
                                            then trace ("special 81767 = " ++ (show $ (snd.unJust) output) ) output
                                            else output --}
                                  

              outputresult'   = map (parseOutput') strlines2

              outputresult''''= filter isJust outputresult'
              outputresult    = map unJust outputresult''''
                                
              combinedresult =  mergeresult inputresult outputresult 
          in  combinedresult -- inputresult' -- combinedresult



