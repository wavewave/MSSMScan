module MSSMScan.Cuts where

import MSSMScan.OutputPhys

-- direct collider bounds
masscut_baris :: OutputPhys -> Bool   
masscut_baris x =    mass_Mh x   > 100 
                  && mass_MC1 x  > 104.5
                  && mass_MSt1 x > 101.5
                  && mass_MSl1 x > 98.8

higgs100cut x = mass_Mh x > 100 

higgs110cut x = mass_Mh x > 110

higgs114cut x = mass_Mh x > 114

charginocut x = let mch    = mass_MC1  x 
                    mneut1 = mass_MNE1 x
                in  if abs (mch - mneut1) > 3.0 
                    then mch > 104.5
                    else True

masscut_wisc_other_than_higgs :: OutputPhys -> Bool   
masscut_wisc_other_than_higgs x = charginocut x 
                                  && mass_MSt1 x > 101.5
                                  && mass_MSl1 x > 98.8
                                  && mass_MSG  x > 309.0
                                  && mass_MH3  x > 85.0
                                  && mass_MHc  x > 79.3



-- indirect bounds
indirect_baris :: OutputPhys -> Bool 
indirect_baris x =    micro_Omega x > 0.0855
                   && micro_Omega x < 0.1189
                   && bsgnlo x      > 229e-6
                   && bsgnlo x      < 481e-6
                   && bsmumu x      < 9e-6
                   && gmuon x       > -11.4e-10
                   && gmuon x       < 9.4e-9

indirect_wisc x =  bsgnlo x      > 229e-6
                   && bsgnlo x      < 481e-6
                   && bsmumu x      < 5.8e-8
                   && gmuon x       > -5.7e-10
                   && gmuon x       < 4.7e-9

wmap_3year x =     micro_Omega x > 0.0855
                   && micro_Omega x < 0.1189

wmap_7year x =     micro_Omega x > 0.0997
                   && micro_Omega x < 0.1221

wmap_preferred x = micro_Omega x > 0.07
                   && micro_Omega x < 0.14

bogus_cut _ = True

