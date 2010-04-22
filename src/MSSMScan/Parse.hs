module MSSMScan.Parse where

import Debug.Trace


import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import Data.Function
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B



import System.IO
import System.Environment

import MSSMScan.DMM
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

parsestr str1 str2 =
          let strlines1 = lines str1
              inputresult'  = zip [1..] $ map (run lineInput) strlines1 
              inputresult'' = filter (isRight.snd) inputresult' 
              inputresult   = map (\x->(fst x, (unRight.snd) x)) 
                                  inputresult''
              
             

              strlines2 = lines str2 
              outputresult'   = map (run lineOutput) strlines2
              outputresult''  = filter isRight outputresult'
              outputresult''' = map unRight outputresult''
              outputresult''''= filter isJust outputresult'''
              outputresult    = map unJust outputresult''''

              combinedresult = mergeresult inputresult outputresult
          in  combinedresult

{--
          print $ length combinedresult

--          let encodedbs = map B.encode combinedresult
          
--        mapM_ (L.hPut outh) encodedbs 


          hClose inh1
          hClose inh2
          hClose outh --}

isRight (Left x) = False 
isRight (Right x) = True

unRight (Right x) = x
unRight (Left x) = undefined

isJust Nothing = False
isJust _       = True
unJust (Just x) = x
unJust Nothing  = undefined

mergeresult :: [(Int,InputDMM)] -> [(Int,OutputPhys)] 
               -> [(Int,(InputDMM,OutputPhys))]
mergeresult [] _ = []
mergeresult _ [] = []
mergeresult (x@(idx,restx):xs) (y@(idy,resty):ys) = 
  if idx == idy 
  then (idx,(restx,resty)) : mergeresult xs ys
  else if idx < idy 
       then mergeresult xs (y:ys)
       else [] 

run :: Parser a -> String -> Either String a 
run p input = 
  case (parse p "" input) of 
    Left err -> Left  $ "parse error at " ++  show err ++ "\n"
    Right x  -> Right x


lineInput :: Parser InputDMM
lineInput  = do nmess  <- myroughfloat  
                empty
                mmess  <- myroughfloat 
                empty
                m0     <- myroughfloat
                empty
                alpham <- myroughfloat
                empty
                alphag <- myroughfloat
                empty
                tanb <- myroughfloat
                
                many (noneOf "\n\r") 
                return $ IDMM (nmess,mmess,m0,alpham,alphag,tanb)

lineOutput :: Parser (Maybe (Int,OutputPhys))
lineOutput = do id' <- myint
                let id = if (id' `mod` 100 == 0) 
                         then trace ("id = " ++ show id') id'
                         else id'
                --id <- myint
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


myint :: Parser Int 
myint = do rest <- many1 (oneOf "0123456789")
           return $ read rest

myroughfloat :: Parser Double
myroughfloat = do result <- many1 (oneOf "+-0123456789.e")
                  let result' = correct_double_str result 
                                --if last result == '.' 
                                --then result ++ "0"
                                --else result
                  return $ read result'

correct_double_str :: String -> String 
correct_double_str str = let (x,y) = break ( == '.') str
                         in if y == []  
                            then str
                            else if y == ['.']
                                 then x ++ ".0"
                                 else if y !! 1 == 'e'
                                      then x ++ ".0" ++ tail y
                                      else str 

empty = many1 (oneOf emptyletters)

emptyletters = [' ']

