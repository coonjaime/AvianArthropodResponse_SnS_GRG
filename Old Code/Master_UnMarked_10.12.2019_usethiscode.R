library('unmarked')
library(ggplot2)
library(ggthemes)
library(ggpubr)
#library (dplyr)
library(gridExtra)
library(AICcmodavg)
library(tidyverse)
library(wesanderson)
library(RColorBrewer)
library(cowplot)

##to do##
#-fix coefs and CIs - do NOT need to be exponentiated -DONE
#-drop uninformative parameters!! - DONE

#***************************************************************************####
#1.IMPORTING----
#***************************************************************************

    DICK_MAC_DATA = "/Users/rozzer/Dropbox/_Manuscripts/Dissertation/Ch. 1/Program UnMarked/MasterDataFiles/DICK_Post2014.csv"
    DICK_all=read.csv(DICK_MAC_DATA,header=T)
   # View(DICK_all)
    
    BOBO_MAC_DATA = "/Users/rozzer/Dropbox/_Manuscripts/Dissertation/Ch. 1/Program UnMarked/MasterDataFiles/BOBO_Post2014.csv"
    BOBO_all=read.csv(BOBO_MAC_DATA,header=T)
    
    GRSP_MAC_DATA = "/Users/rozzer/Dropbox/_Manuscripts/Dissertation/Ch. 1/Program UnMarked/MasterDataFiles/GRSP_Post2014.csv"
    GRSP_all=read.csv(GRSP_MAC_DATA,header=T)

    EAME_MAC_DATA = "/Users/rozzer/Dropbox/_Manuscripts/Dissertation/Ch. 1/Program UnMarked/MasterDataFiles/EAME_Post2014.csv"
    EAME_all=read.csv(EAME_MAC_DATA,header=T)
    
    RWBL_MAC_DATA = "/Users/rozzer/Dropbox/_Manuscripts/Dissertation/Ch. 1/Program UnMarked/MasterDataFiles/RWBL_Post2014.csv"
    RWBL_all=read.csv(RWBL_MAC_DATA,header=T) 

    BHCO_MAC_DATA = "/Users/rozzer/Dropbox/_Manuscripts/Dissertation/Ch. 1/Program UnMarked/MasterDataFiles/BHCO_Post2014.csv"
    BHCO_all=read.csv(BHCO_MAC_DATA,header=T)  
    
    FISP_MAC_DATA = "/Users/rozzer/Dropbox/_Manuscripts/Dissertation/Ch. 1/Program UnMarked/MasterDataFiles/FISP_Post2014.csv"
    FISP_all=read.csv(FISP_MAC_DATA,header=T)  
    
    HESP_MAC_DATA = "/Users/rozzer/Dropbox/_Manuscripts/Dissertation/Ch. 1/Program UnMarked/MasterDataFiles/HESP_Post2014.csv"
    HESP_all=read.csv(HESP_MAC_DATA,header=T)  
    
    SEWR_MAC_DATA = "/Users/rozzer/Dropbox/_Manuscripts/Dissertation/Ch. 1/Program UnMarked/MasterDataFiles/SEWR_Post2014.csv"
    SEWR_all=read.csv(SEWR_MAC_DATA,header=T)  
    
#***************************************************************************####
#2 [HERB] CLEANING + FILTERING----
#***************************************************************************

Data_Cleaning = function (df) {
  
  #setting NAs from visits to 0
  df$Visit_1[is.na(df$Visit_1)] <- 0
  df$Visit_2[is.na(df$Visit_2)] <- 0
  df$Visit_3[is.na(df$Visit_3)] <- 0
  df$Visit_4[is.na(df$Visit_4)] <- 0
  df$Visit_5[is.na(df$Visit_5)] <- 0
  
  #replacing missing covariates to the mean
  df$Clouds_1[is.na(df$Clouds_1)] <- mean(df$Clouds_1,na.rm=T)
  df$Clouds_2[is.na(df$Clouds_2)] <- mean(df$Clouds_2,na.rm=T)
  df$Clouds_3[is.na(df$Clouds_3)] <- mean(df$Clouds_3,na.rm=T)
  df$Clouds_4[is.na(df$Clouds_4)] <- mean(df$Clouds_4,na.rm=T)
  df$Clouds_5[is.na(df$Clouds_5)] <- mean(df$Clouds_5,na.rm=T)
  
  df$Date_1_Ord[is.na(df$Date_1_Ord)] <- mean(df$Date_1_Ord,na.rm=T)
  df$Date_2_Ord[is.na(df$Date_2_Ord)] <- mean(df$Date_2_Ord,na.rm=T)
  df$Date_3_Ord[is.na(df$Date_3_Ord)] <- mean(df$Date_3_Ord,na.rm=T)
  df$Date_4_Ord[is.na(df$Date_4_Ord)] <- mean(df$Date_4_Ord,na.rm=T)
  df$Date_5_Ord[is.na(df$Date_5_Ord)] <- mean(df$Date_5_Ord,na.rm=T)
  
  df$StartTime_1[is.na(df$StartTime_1)] <- mean(df$StartTime_1,na.rm=T)
  df$StartTime_2[is.na(df$StartTime_2)] <- mean(df$StartTime_2,na.rm=T)
  df$StartTime_3[is.na(df$StartTime_3)] <- mean(df$StartTime_3,na.rm=T)
  df$StartTime_4[is.na(df$StartTime_4)] <- mean(df$StartTime_4,na.rm=T)
  df$StartTime_5[is.na(df$StartTime_5)] <- mean(df$StartTime_5,na.rm=T)
  
  df$Winds_1[is.na(df$Winds_1)] <- mean(df$Winds_1,na.rm=T)
  df$Winds_2[is.na(df$Winds_2)] <- mean(df$Winds_2,na.rm=T)
  df$Winds_3[is.na(df$Winds_3)] <- mean(df$Winds_3,na.rm=T)
  df$Winds_4[is.na(df$Winds_4)] <- mean(df$Winds_4,na.rm=T)
  df$Winds_5[is.na(df$Winds_5)] <- mean(df$Winds_5,na.rm=T)
  
  #rounding and and rescaling large numbers by dividing by 100
  df$Clouds_1 <- round((df$Clouds_1/100),2)
  df$Clouds_2 <- round((df$Clouds_2/100),2)
  df$Clouds_3 <- round((df$Clouds_3/100),2)
  df$Clouds_4 <- round((df$Clouds_4/100),2)
  df$Clouds_5 <- round((df$Clouds_5/100),2)
  
  df$Date_1_Ord <- round(df$Date_1_Ord/100,2)
  df$Date_2_Ord <- round(df$Date_2_Ord/100,2)
  df$Date_3_Ord <- round(df$Date_3_Ord/100,2)
  df$Date_4_Ord <- round(df$Date_4_Ord/100,2)
  df$Date_5_Ord <- round(df$Date_5_Ord/100,2)
  
  df$FirstOfSumOf2010_Length <- round(df$FirstOfSumOf2010_Length/100,0)
  
  df$StartTime_1 <- round(df$StartTime_1,4)
  df$StartTime_2 <- round(df$StartTime_2,4)
  df$StartTime_3 <- round(df$StartTime_3,4)
  df$StartTime_4 <- round(df$StartTime_4,4)
  df$StartTime_5 <- round(df$StartTime_5,4)
  
  df$Winds_1 <- round(df$Winds_1,0)
  df$Winds_2 <- round(df$Winds_2,0)
  df$Winds_3 <- round(df$Winds_3,0)
  df$Winds_4 <- round(df$Winds_4,0)
  df$Winds_5 <- round(df$Winds_5,0)
  
  df$TSH[df$TSH==0.5]='a'
  df$TSH[df$TSH==1.5]='b'
  df$TSH[df$TSH==2.5]='c'
  df$TSH[df$TSH==3.5]='d'
  
  df$Year[df$Year==2015]='a'
  df$Year[df$Year==2016]='b'
  df$Year[df$Year==2017]='c'
  df$Year[df$Year==2018]='d'
  
  df$GrazingTreat[df$GrazingTreat=='Hay']='None' #Haying is technically no grazing, so this code combines those two categories in a new column
  df$GrazingTreat[df$GrazingTreat=='IES']='IES'
  df$GrazingTreat[df$GrazingTreat=='SLS']='SLS'
  df$GrazingTreat[df$GrazingTreat=='None']='None'
  df$GrazingTreat=factor(df$GrazingTreat)
  
  df
  
  df%>%
    filter(!(Pasture=="235"))%>% 
    filter(!(Pasture=="KLN"))%>% 
    filter(!(Pasture=="KLT"))%>%
    filter(!(Pasture=="PAW"))%>%
    filter(!(Pasture=="PYN"))%>%
    filter(!(Pasture=="PYS"))%>%
    filter(!(Pasture=="RCH2014"))%>%
    filter(!(Pasture=="RIE"))%>%
    filter(!(Pasture=="RIS"))%>%
    filter(!(Pasture=="RNR"))%>%
    filter(!(Pasture=="RIN"))%>%
    filter(!(Pasture=="STE"& Year=="a"))%>%
    filter(!(Pasture_patch_year=="STE_N2_2016"))%>%
    filter(!(Pasture_patch_year=="STE_N2_2017"))%>%
    filter(!(Pasture_patch_year=="STE_N2_2018"))
} #data cleaning and filtering function

    DICK_Herb= Data_Cleaning(DICK_all) #>>Dickcissel----
    DICK_Herb$HerbTreat_14.18=factor(DICK_Herb$HerbTreat_14.18)
    #View(DICK_Herb)

    BOBO_Herb= Data_Cleaning(BOBO_all) #>>Bobolink----
    BOBO_Herb$HerbTreat_14.18=factor(BOBO_Herb$HerbTreat_14.18)

    GRSP_Herb= Data_Cleaning(GRSP_all) #>>Grasshopper Sparrow----
    GRSP_Herb$HerbTreat_14.18=factor(GRSP_Herb$HerbTreat_14.18)
    
    EAME_Herb= Data_Cleaning(EAME_all) #>>Meadowlarks----
    EAME_Herb$HerbTreat_14.18=factor(EAME_Herb$HerbTreat_14.18)
    
    RWBL_Herb= Data_Cleaning(RWBL_all) #>>Red-winged Blackbird----
    RWBL_Herb$HerbTreat_14.18=factor(RWBL_Herb$HerbTreat_14.18)
    
    BHCO_Herb= Data_Cleaning(BHCO_all) #>>Cowbirds----
    BHCO_Herb$HerbTreat_14.18=factor(BHCO_Herb$HerbTreat_14.18) 
    
    FISP_Herb= Data_Cleaning(FISP_all) #>>Field Sparrow----
    FISP_Herb$HerbTreat_14.18=factor(FISP_Herb$HerbTreat_14.18) 
    
    HESP_Herb= Data_Cleaning(HESP_all) #>>Henslow's Sparrow----
    HESP_Herb$HerbTreat_14.18=factor(HESP_Herb$HerbTreat_14.18)  
    
    SEWR_Herb= Data_Cleaning(SEWR_all) #>>Sedge Wren----
    SEWR_Herb$HerbTreat_14.18=factor(SEWR_Herb$HerbTreat_14.18) 
    
Data_Cleaning_Occu= function (df) {
  df$Visit_1[df$Visit_1>0]=1
  df$Visit_2[df$Visit_2>0]=1
  df$Visit_3[df$Visit_3>0]=1
  df$Visit_4[df$Visit_4>0]=1
  df$Visit_5[df$Visit_5>0]=1
  df
  }

HESP_Presence=Data_Cleaning_Occu(HESP_Herb) 
SEWR_Presence=Data_Cleaning_Occu(SEWR_Herb)

#How many detections?----
DICK_Det=sum(DICK_Herb$Visit_1)+sum(DICK_Herb$Visit_2)+sum(DICK_Herb$Visit_3)+sum(DICK_Herb$Visit_4)
BOBO_Det=sum(BOBO_Herb$Visit_1)+sum(BOBO_Herb$Visit_2)+sum(BOBO_Herb$Visit_3)+sum(BOBO_Herb$Visit_4)
GRSP_Det=sum(GRSP_Herb$Visit_1)+sum(GRSP_Herb$Visit_2)+sum(GRSP_Herb$Visit_3)+sum(GRSP_Herb$Visit_4)
EAME_Det=sum(EAME_Herb$Visit_1)+sum(EAME_Herb$Visit_2)+sum(EAME_Herb$Visit_3)+sum(EAME_Herb$Visit_4)
RWBL_Det=sum(RWBL_Herb$Visit_1)+sum(RWBL_Herb$Visit_2)+sum(RWBL_Herb$Visit_3)+sum(RWBL_Herb$Visit_4)
BHCO_Det=sum(BHCO_Herb$Visit_1)+sum(BHCO_Herb$Visit_2)+sum(BHCO_Herb$Visit_3)+sum(BHCO_Herb$Visit_4) 
FISP_Det=sum(FISP_Herb$Visit_1)+sum(FISP_Herb$Visit_2)+sum(FISP_Herb$Visit_3)+sum(FISP_Herb$Visit_4) #probably need to do occu
HESP_Det=sum(HESP_Herb$Visit_1)+sum(HESP_Herb$Visit_2)+sum(HESP_Herb$Visit_3)+sum(HESP_Herb$Visit_4) #probably need to do occu
SEWR_Det=sum(SEWR_Herb$Visit_1)+sum(SEWR_Herb$Visit_2)+sum(SEWR_Herb$Visit_3)+sum(SEWR_Herb$Visit_4) #probably need to do occu

Det_List=c(DICK_Det,BOBO_Det,GRSP_Det,EAME_Det, RWBL_Det, FISP_Det,BHCO_Det,HESP_Det, SEWR_Det)
Det_List
#***************************************************************************####
#3 [HERB] UNMARKED SETUP----
#***************************************************************************

unmarked_data = function(df) {
  abundance_by_visit=df[,4:8] #sets the columns that will be used for abundance
  siteCovs=data.frame(list(Past_Pat_Year=df[,1], #site covariates
                           Year=df[,2], 
                           Pasture=df[,3], 
                           FireTreat=df[,34], 
                           TSH=df[,36],
                           HerbTreat=df[,37],
                           GrazingYesNo=df[,38],
                           HerbYesNo=df[,39],
                           GrazingTreat=df[,40],
                           Robel=df[,41],
                           Area_ha=df[,42], 
                           Tree_kmha=df[,44],
                           Herb_Prop=df[,45],
                           Crop_Prop=df[,46],
                           Tree_Prop=df[,47]
  ))
  obsCovs=list(Wind=df[,24:28],  #sets the columns that will be used for observation covariates (should match the number of columns for abundance)
               DOY=df[,14:18],
               Clouds=df[,9:13],
               StartTime=df[,19:23],
               Obs=df[,29:33])
  unmarkedFramePCount(abundance_by_visit,siteCovs,obsCovs)
} #setting up unmarked 'pcount'

    DICK_PCount=unmarked_data(DICK_Herb) #>>Dickcissels----
    BOBO_PCount=unmarked_data(BOBO_Herb) #>>Bobolink----
    GRSP_PCount=unmarked_data(GRSP_Herb) #>>Grasshopper Sparrow----
    EAME_PCount=unmarked_data(EAME_Herb) #>>Meadowlarks----
    RWBL_PCount=unmarked_data(RWBL_Herb) #>>Red-winged Blackbirds----
    BHCO_PCount=unmarked_data(BHCO_Herb) #>>Cowbirds----
    FISP_PCount=unmarked_data(FISP_Herb) #>>Field Sparrows----
    #HESP_PCount=unmarked_data(HESP_Herb) 
    #SEWR_PCount=unmarked_data(SEWR_Herb) 

occu_unmarked_data = function(df) {
      occu_by_visit=df[,4:8] #sets the columns that will be used for abundance
      siteCovs=data.frame(list(Past_Pat_Year=df[,1], #site covariates
                               Year=df[,2], 
                               Pasture=df[,3], 
                               FireTreat=df[,34], 
                               TSH=df[,36],
                               HerbTreat=df[,37],
                               GrazingYesNo=df[,38],
                               HerbYesNo=df[,39],
                               GrazingTreat=df[,40],
                               Robel=df[,41],
                               Area_ha=df[,42], 
                               Tree_kmha=df[,44],
                               Herb_Prop=df[,45],
                               Crop_Prop=df[,46],
                               Tree_Prop=df[,47]
      ))
      obsCovs=list(Wind=df[,24:28],  #sets the columns that will be used for observation covariates (should match the number of columns for abundance)
                   DOY=df[,14:18],
                   Clouds=df[,9:13],
                   StartTime=df[,19:23],
                   Obs=df[,29:33])
      unmarkedFrameOccu(occu_by_visit,siteCovs,obsCovs)
  }
    
    HESP_Occu=occu_unmarked_data(HESP_Presence)#>>Henslow's Sparrow---
    SEWR_Occu=occu_unmarked_data(SEWR_Presence)#>>Sedge Wren----
#***************************************************************************####
#4 [HERB] DISTRIBUTIONS----
#***************************************************************************#
Distribution_mods = function (df) {
  null_ZIP      =pcount(~1 ~1, data=df, mixture="ZIP",K=100,)
  null_poisson  =pcount(~1 ~1, data=df, mixture="P",K=100,)
  
  mods=list(null_ZIP,null_poisson)
  names=c("null_ZIP","null_poisson")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} #picking between ZIP and P


Distribution_mods(DICK_PCount) #>>Dickcissel----
    #              K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 1364.634    0.0000     1      1 -679.3172
    # null_poisson 2 1426.679   62.0447     0      1 -711.3395
  #Chosen: ZIP
Distribution_mods(BOBO_PCount)#>>Bobolink----
    #              K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 1449.807     0.000     1      1 -721.9034
    # null_poisson 2 1644.236   194.429     0      1 -820.1179
  #Chosen:ZIP
Distribution_mods(GRSP_PCount)#>>Grasshopper Sparrow----
    #              K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 780.2308    0.0000     1      1 -387.1154
    # null_poisson 2 853.4659   73.2351     0      1 -424.7329

Distribution_mods(EAME_PCount)#>>Meadowlarks----
    #              K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # null_ZIP     3 790.0357    0.0000 0.9964 0.9964 -392.0179
    # null_poisson 2 801.2749   11.2391 0.0036 1.0000 -398.6374
Distribution_mods(RWBL_PCount)#>>Red-winged Blackbird----
#              K      AIC Delta_AIC AICWt Cum.Wt        LL
# null_ZIP     3 1277.961    0.0000     1      1 -635.9805
# null_poisson 2 1396.478  118.5167     0      1 -696.2388

Distribution_mods(BHCO_PCount)#>>Cowbirds----
    #            K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 727.8665     0.000     1      1 -360.9332
    # null_poisson 2 788.0675    60.201     0      1 -392.0338
Distribution_mods(FISP_PCount)#>>Field Sparrow----
    #             K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 575.9652    0.0000     1      1 -284.9826
    # null_poisson 2 634.1127   58.1475     0      1 -315.0563


#Distribution_mods_occu(HESP_Occu)#>>Henslow's Sparrow----
    #              K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 546.1615    0.0000     1      1 -270.0807
    # null_poisson 2 579.5776   33.4162     0      1 -287.7888

#Distribution_mods(SEWR_PCount)#>>Sedge Wren----
    #             K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 510.7426    0.0000     1      1 -252.3713
    # null_poisson 2 572.4041   61.6616     0      1 -284.2021


#***************************************************************************####
#5 [HERB] DETECTION----
#***************************************************************************
Detection_mods=function(df) {
  # For detection variables, all single-variable models are included, plus each of the following groups in all subsets:
  #Weather: Wind,Clouds
  # Timing: StartTime, DOY
  # Observer: Obs
  # Visibility: Robel
  Null                      =pcount(~Robel ~1, data=df, mixture="ZIP",K=100,)
  Obs                       =pcount(~Obs+Robel ~1, data=df, mixture="ZIP",K=100)
  StartTime                 =pcount(~StartTime+Robel ~1, data=df, mixture="ZIP",K=100)
  DOY                       =pcount(~DOY+Robel ~1, data=df, mixture="ZIP",K=100)
  Winds                     =pcount(~Wind+Robel ~1, data=df, mixture="ZIP",K=100)
  Clouds                    =pcount(~Clouds+Robel ~1, data=df, mixture="ZIP",K=100)
  Winds_Clouds              =pcount(~Wind+Clouds+Robel ~1, data=df, mixture="ZIP",K=100)
  StartTime_DOY             =pcount(~StartTime+DOY+Robel ~1, data=df, mixture="ZIP",K=100)
  Obs_StartTime_DOY         =pcount(~Obs+DOY+StartTime+Robel ~1, data=df, mixture="ZIP",K=100)
  Obs_Winds_Clouds          =pcount(~Obs+Wind+Clouds+Robel ~1, data=df, mixture="ZIP",K=100)
  Global                    =pcount(~Obs+DOY+StartTime+Robel+Wind+Clouds ~1, data=df, mixture="ZIP",K=100)
  
  mods=list(Null, Obs,   StartTime,  DOY,   Winds,   Clouds,   Winds_Clouds,   StartTime_DOY,   Obs_StartTime_DOY,   Obs_Winds_Clouds,   Global)
  names=c("Null","Obs", "StartTime","DOY", "Winds", "Clouds", "Winds_Clouds", "StartTime_DOY", "Obs_StartTime_DOY", "Obs_Winds_Clouds", "Global")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}
Detection_mods_occu=function(df) {
  # For detection variables, all single-variable models are included, plus each of the following groups in all subsets:
  #Weather: Wind,Clouds
  # Timing: StartTime, DOY
  # Observer: Obs
  # Visibility: Robel
  Null                      =occu(~Robel ~1, data=df,)
  Obs                       =occu(~Obs+Robel ~1, data=df)
  StartTime                 =occu(~StartTime+Robel ~1, data=df)
  DOY                       =occu(~DOY+Robel ~1, data=df)
  Winds                     =occu(~Wind+Robel ~1, data=df)
  Clouds                    =occu(~Clouds+Robel ~1, data=df)
  Winds_Clouds              =occu(~Wind+Clouds+Robel ~1, data=df)
  StartTime_DOY             =occu(~StartTime+DOY+Robel ~1, data=df)
  Obs_StartTime_DOY         =occu(~Obs+DOY+StartTime+Robel ~1, data=df)
  Obs_Winds_Clouds          =occu(~Obs+Wind+Clouds+Robel ~1, data=df)
  Global                    =occu(~Obs+DOY+StartTime+Robel+Wind+Clouds ~1, data=df)
  
  mods=list(Null, Obs,   StartTime,  DOY,   Winds,   Clouds,   Winds_Clouds,   StartTime_DOY,   Obs_StartTime_DOY,   Obs_Winds_Clouds,   Global)
  names=c("Null","Obs", "StartTime","DOY", "Winds", "Clouds", "Winds_Clouds", "StartTime_DOY", "Obs_StartTime_DOY", "Obs_Winds_Clouds", "Global")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}

Detection_mods(DICK_PCount) #>>Dickcissel: Obs+Starttime+DOY (no Robel)----
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Obs_StartTime_DOY 14 1317.266    0.0000 0.4477 0.4477 -644.6329
      # Global            16 1317.436    0.1700 0.4112 0.8588 -642.7179
      # Obs               12 1320.413    3.1474 0.0928 0.9516 -648.2066
      # Obs_Winds_Clouds  14 1321.715    4.4494 0.0484 1.0000 -646.8576
      # Winds_Clouds       6 1359.293   42.0266 0.0000 1.0000 -673.6462
      # StartTime          5 1359.726   42.4607 0.0000 1.0000 -674.8633
      # Winds              5 1361.376   44.1096 0.0000 1.0000 -675.6877
      # StartTime_DOY      6 1361.679   44.4134 0.0000 1.0000 -674.8396
      # Clouds             5 1363.266   45.9997 0.0000 1.0000 -676.6328
      # Null               4 1363.508   46.2424 0.0000 1.0000 -677.7541
      # DOY                5 1422.800  105.5339 0.0000 1.0000 -706.3999
          #CHOSEN: Obs_StartTime_DOY (+Robel)
  DICK_det_mod   =pcount(~Obs+DOY+StartTime+Robel ~1, data=DICK_PCount, mixture="ZIP",K=100)
  confint(DICK_det_mod, type="det", level=0.85) #indicates Robel is uninformative parameter

Detection_mods(BOBO_PCount) #>>Bobolink: Obs+DOY+Robel+Clouds---- 
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Global            16 1329.780    0.0000 0.9351 0.9351 -648.8899
      # Obs_StartTime_DOY 14 1335.116    5.3358 0.0649 1.0000 -653.5577
      # Obs_Winds_Clouds  14 1361.143   31.3637 0.0000 1.0000 -666.5717
      # Obs               12 1365.997   36.2177 0.0000 1.0000 -670.9987
      # DOY                5 1408.312   78.5326 0.0000 1.0000 -699.1562
      # StartTime_DOY      6 1409.029   79.2496 0.0000 1.0000 -698.5146
      # Clouds             5 1442.163  112.3830 0.0000 1.0000 -716.0813
      # Winds_Clouds       6 1444.141  114.3614 0.0000 1.0000 -716.0706
      # Null               4 1446.290  116.5098 0.0000 1.0000 -719.1448
      # Winds              5 1447.652  117.8727 0.0000 1.0000 -718.8262
      # StartTime          5 1448.202  118.4220 0.0000 1.0000 -719.1008
      #CHOSEN: GLOBAL

    BOBO_det_mod=pcount(~Obs+DOY+StartTime+Robel+Wind+Clouds ~1, data=BOBO_PCount, mixture="ZIP",K=100)
    confint(BOBO_det_mod, type="det", level=0.85) #indicates Wind and StartTime are uninformative parameters

Detection_mods(GRSP_PCount) #>>Grasshopper Sparrow: Obs+Robel---- 
        #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # Obs               12 720.0560    0.0000 0.6158 0.6158 -348.0280
        # Obs_StartTime_DOY 14 721.8257    1.7697 0.2542 0.8699 -346.9128
        # Obs_Winds_Clouds  14 723.8927    3.8367 0.0904 0.9604 -347.9464
        # Global            16 725.5424    5.4864 0.0396 1.0000 -346.7712
        # Winds              5 746.0638   26.0078 0.0000 1.0000 -368.0319
        # Null               4 746.0741   26.0181 0.0000 1.0000 -369.0371
        # DOY                5 746.8642   26.8082 0.0000 1.0000 -368.4321
        # Winds_Clouds       6 747.3154   27.2594 0.0000 1.0000 -367.6577
        # Clouds             5 747.6940   27.6380 0.0000 1.0000 -368.8470
        # StartTime          5 748.0169   27.9609 0.0000 1.0000 -369.0085
        # StartTime_DOY      6 748.8359   28.7799 0.0000 1.0000 -368.4180
    GRSP_det_mod   =pcount(~Obs+Robel ~1, data=GRSP_PCount, mixture="ZIP",K=100)
    confint(GRSP_det_mod, type="det", level=0.85) #all good


Detection_mods(EAME_PCount) #>>Meadowlarks: Robel---- 
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Null               4 775.0810    0.0000 0.2354 0.2354 -383.5405
      # DOY                5 776.0492    0.9682 0.1450 0.3804 -383.0246
      # Clouds             5 776.8582    1.7772 0.0968 0.4772 -383.4291
      # Winds              5 776.8789    1.7978 0.0958 0.5730 -383.4394
      # Obs               12 776.8800    1.7989 0.0957 0.6687 -376.4400
      # StartTime          5 777.0759    1.9949 0.0868 0.7555 -383.5379
      # Obs_Winds_Clouds  14 777.3864    2.3053 0.0743 0.8298 -374.6932
      # StartTime_DOY      6 778.0202    2.9392 0.0541 0.8840 -383.0101
      # Obs_StartTime_DOY 14 778.2957    3.2147 0.0472 0.9311 -375.1478
      # Winds_Clouds       6 778.7280    3.6469 0.0380 0.9691 -383.3640
      # Global            16 779.1439    4.0629 0.0309 1.0000 -373.5719

    EAME_det_mod   =pcount(~Robel ~1, data=EAME_PCount, mixture="ZIP",K=100)
    confint(EAME_det_mod, type="det", level=0.85) #all good

Detection_mods(RWBL_PCount) #>>Red-winged Blackbird: Obs+Robel+DOY---- 
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Obs_StartTime_DOY 14 1211.829    0.0000 0.6862 0.6862 -591.9146
      # Global            16 1213.394    1.5649 0.3138 1.0000 -590.6970
      # DOY                5 1232.848   21.0184 0.0000 1.0000 -611.4237
      # StartTime_DOY      6 1234.612   22.7829 0.0000 1.0000 -611.3060
      # Obs               12 1247.562   35.7334 0.0000 1.0000 -611.7812
      # Obs_Winds_Clouds  14 1249.908   38.0789 0.0000 1.0000 -610.9540
      # Clouds             5 1268.220   56.3910 0.0000 1.0000 -629.1101
      # Null               4 1268.332   56.5031 0.0000 1.0000 -630.1661
      # Winds              5 1268.692   56.8628 0.0000 1.0000 -629.3460
      # Winds_Clouds       6 1269.336   57.5070 0.0000 1.0000 -628.6681
      # StartTime          5 1269.797   57.9677 0.0000 1.0000 -629.8984
    RWBL_det_mod   =pcount(~Obs+Robel+StartTime+DOY ~1, data=RWBL_PCount, mixture="ZIP",K=100)
    confint(RWBL_det_mod, type="det", level=0.85) #StartTime is uninformative

Detection_mods(BHCO_PCount) #>>Cowbirds: Obs+DOY+Clouds---- 
    #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # Global            16 694.2173    0.0000 0.3827 0.3827 -331.1086
    # Obs_StartTime_DOY 14 694.6120    0.3947 0.3142 0.6969 -333.3060
    # Obs               12 695.5165    1.2993 0.1999 0.8968 -335.7583
    # Obs_Winds_Clouds  14 696.8376    2.6203 0.1032 1.0000 -334.4188
    # DOY                5 728.7753   34.5580 0.0000 1.0000 -359.3876
    # Null               4 729.8465   35.6293 0.0000 1.0000 -360.9233
    # Clouds             5 730.5519   36.3346 0.0000 1.0000 -360.2760
    # StartTime_DOY      6 730.6887   36.4714 0.0000 1.0000 -359.3443
    # Winds              5 731.5161   37.2988 0.0000 1.0000 -360.7580
    # StartTime          5 731.8211   37.6038 0.0000 1.0000 -360.9106
    # Winds_Clouds       6 731.9883   37.7710 0.0000 1.0000 -359.9941
    BHCO_det_mod   =pcount(~Obs+Robel+StartTime+DOY+Clouds+Wind ~1, data=BHCO_PCount, mixture="ZIP",K=100)
    confint(BHCO_det_mod, type="det", level=0.85) #StartTime,Wind,Robel is uninformative
    
    
Detection_mods_occu(HESP_Occu) #>>Henslow's Sparrow:Clouds+Robel---- 
    #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # Clouds             4 340.8620    0.0000 0.3913 0.3913 -166.4310
    # Winds_Clouds       5 341.4825    0.6205 0.2869 0.6782 -165.7413
    # Obs_Winds_Clouds  13 342.0455    1.1835 0.2165 0.8947 -158.0227
    # Winds              4 345.9980    5.1360 0.0300 0.9247 -168.9990
    # Global            15 346.0368    5.1748 0.0294 0.9542 -158.0184
    # Null               3 347.1670    6.3050 0.0167 0.9709 -170.5835
    # Obs               11 347.8154    6.9534 0.0121 0.9830 -162.9077
    # DOY                4 349.0184    8.1564 0.0066 0.9896 -170.5092
    # StartTime          4 349.1669    8.3049 0.0062 0.9958 -170.5835
    # StartTime_DOY      5 351.0094   10.1474 0.0024 0.9982 -170.5047
    # Obs_StartTime_DOY 13 351.6514   10.7894 0.0018 1.0000 -162.8257            

    HESP_det_mod   =occu(~Clouds+Robel ~1, data=HESP_Occu)
    confint(HESP_det_mod, type="det", level=0.85) #all good
  
Detection_mods_occu(SEWR_Occu) #>>Sedge Wren:Obs+StartTime+DOY+Robel---- 
      #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Obs_StartTime_DOY 13 268.3983    0.0000 0.6950 0.6950 -121.1991
      # Global            15 270.0556    1.6573 0.3035 0.9985 -120.0278
      # StartTime_DOY      5 280.9264   12.5281 0.0013 0.9999 -135.4632
      # Obs               11 285.8965   17.4982 0.0001 1.0000 -131.9483
      # DOY                4 289.4626   21.0643 0.0000 1.0000 -140.7313
      # Obs_Winds_Clouds  13 289.6618   21.2635 0.0000 1.0000 -131.8309
      # StartTime          4 293.4036   25.0053 0.0000 1.0000 -142.7018
      # Null               3 297.0716   28.6733 0.0000 1.0000 -145.5358
      # Winds              4 298.3880   29.9897 0.0000 1.0000 -145.1940
      # Clouds             4 299.0134   30.6151 0.0000 1.0000 -145.5067
      # Winds_Clouds       5 300.3763   31.9781 0.0000 1.0000 -145.1882

    SEWR_det_mod   =occu(~Obs+StartTime+DOY+Robel ~1, data=SEWR_Occu)
    confint(SEWR_det_mod, type="det", level=0.85) #

    
#**************************************************************************#####
#6 [HERB] LANDSCAPE----
#***************************************************************************

DICK_Landscape_mods = function(df) {
  Null               =pcount(~Obs+StartTime+DOY  ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp           =pcount(~Obs+StartTime+DOY  ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp  =pcount(~Obs+StartTime+DOY  ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_Tree_kmha =pcount(~Obs+StartTime+DOY  ~Herb_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp           =pcount(~Obs+StartTime+DOY  ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp  =pcount(~Obs+StartTime+DOY  ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreekmHa  =pcount(~Obs+StartTime+DOY  ~Crop_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp           =pcount(~Obs+StartTime+DOY  ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  Tree_kmHa          =pcount(~Obs+StartTime+DOY  ~Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   HerbProp_Tree_kmha,   CropProp,   CropProp_TreeProp,   CropProp_TreekmHa,   TreeProp,   Tree_kmHa)
  names=c( "Null", "HerbProp", "HerbProp_TreeProp", "HerbProp_Tree_kmha", "CropProp", "CropProp_TreeProp", "CropProp_TreekmHa", "TreeProp", "Tree_kmHa")
  
 print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 
BOBO_Landscape_mods = function(df) {
  Null               =pcount(~Obs+DOY+Robel+Clouds   ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp           =pcount(~Obs+DOY+Robel+Clouds   ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp  =pcount(~Obs+DOY+Robel+Clouds   ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_Tree_kmha =pcount(~Obs+DOY+Robel+Clouds   ~Herb_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp           =pcount(~Obs+DOY+Robel+Clouds   ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp  =pcount(~Obs+DOY+Robel+Clouds   ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreekmHa  =pcount(~Obs+DOY+Robel+Clouds   ~Crop_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp           =pcount(~Obs+DOY+Robel+Clouds   ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  Tree_kmHa          =pcount(~Obs+DOY+Robel+Clouds   ~Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   HerbProp_Tree_kmha,   CropProp,   CropProp_TreeProp,   CropProp_TreekmHa,   TreeProp,   Tree_kmHa)
  names=c( "Null", "HerbProp", "HerbProp_TreeProp", "HerbProp_Tree_kmha", "CropProp", "CropProp_TreeProp", "CropProp_TreekmHa", "TreeProp", "Tree_kmHa")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 
GRSP_Landscape_mods = function(df) {
  Null               =pcount(~Obs+Robel   ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp           =pcount(~Obs+Robel   ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp  =pcount(~Obs+Robel   ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_Tree_kmha =pcount(~Obs+Robel   ~Herb_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp           =pcount(~Obs+Robel   ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp  =pcount(~Obs+Robel   ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreekmHa  =pcount(~Obs+Robel   ~Crop_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp           =pcount(~Obs+Robel   ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  Tree_kmHa          =pcount(~Obs+Robel   ~Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   HerbProp_Tree_kmha,   CropProp,   CropProp_TreeProp,   CropProp_TreekmHa,   TreeProp,   Tree_kmHa)
  names=c( "Null", "HerbProp", "HerbProp_TreeProp", "HerbProp_Tree_kmha", "CropProp", "CropProp_TreeProp", "CropProp_TreekmHa", "TreeProp", "Tree_kmHa")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 
EAME_Landscape_mods = function(df) {
  Null               =pcount(~Robel   ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp           =pcount(~Robel   ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp  =pcount(~Robel   ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_Tree_kmha =pcount(~Robel   ~Herb_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp           =pcount(~Robel   ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp  =pcount(~Robel   ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreekmHa  =pcount(~Robel   ~Crop_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp           =pcount(~Robel   ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  Tree_kmHa          =pcount(~Robel   ~Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   HerbProp_Tree_kmha,   CropProp,   CropProp_TreeProp,   CropProp_TreekmHa,   TreeProp,   Tree_kmHa)
  names=c( "Null", "HerbProp", "HerbProp_TreeProp", "HerbProp_Tree_kmha", "CropProp", "CropProp_TreeProp", "CropProp_TreekmHa", "TreeProp", "Tree_kmHa")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 
RWBL_Landscape_mods = function(df) {
  Null               =pcount(~Robel+Obs+DOY    ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp           =pcount(~Robel+Obs+DOY    ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp  =pcount(~Robel+Obs+DOY    ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_Tree_kmha =pcount(~Robel+Obs+DOY    ~Herb_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp           =pcount(~Robel+Obs+DOY    ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp  =pcount(~Robel+Obs+DOY    ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreekmHa  =pcount(~Robel+Obs+DOY    ~Crop_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp           =pcount(~Robel+Obs+DOY    ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  Tree_kmHa          =pcount(~Robel+Obs+DOY    ~Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   HerbProp_Tree_kmha,   CropProp,   CropProp_TreeProp,   CropProp_TreekmHa,   TreeProp,   Tree_kmHa)
  names=c( "Null", "HerbProp", "HerbProp_TreeProp", "HerbProp_Tree_kmha", "CropProp", "CropProp_TreeProp", "CropProp_TreekmHa", "TreeProp", "Tree_kmHa")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 
BHCO_Landscape_mods = function(df) {
  Null               =pcount(~Obs+DOY+Clouds     ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp           =pcount(~Obs+DOY+Clouds     ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp  =pcount(~Obs+DOY+Clouds     ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_Tree_kmha =pcount(~Obs+DOY+Clouds     ~Herb_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp           =pcount(~Obs+DOY+Clouds     ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp  =pcount(~Obs+DOY+Clouds     ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreekmHa  =pcount(~Obs+DOY+Clouds     ~Crop_Prop+Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp           =pcount(~Obs+DOY+Clouds     ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  Tree_kmHa          =pcount(~Obs+DOY+Clouds     ~Tree_kmha+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   HerbProp_Tree_kmha,   CropProp,   CropProp_TreeProp,   CropProp_TreekmHa,   TreeProp,   Tree_kmHa)
  names=c( "Null", "HerbProp", "HerbProp_TreeProp", "HerbProp_Tree_kmha", "CropProp", "CropProp_TreeProp", "CropProp_TreekmHa", "TreeProp", "Tree_kmHa")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 
HESP_Landscape_mods = function(df) {
  Null               =occu(~Robel+Clouds   ~1, data=df)
  HerbProp           =occu(~Robel+Clouds   ~Herb_Prop, data=df)
  HerbProp_TreeProp  =occu(~Robel+Clouds   ~Herb_Prop+Tree_Prop, data=df)
  HerbProp_Tree_kmha =occu(~Robel+Clouds   ~Herb_Prop+Tree_kmha, data=df)
  CropProp           =occu(~Robel+Clouds   ~Crop_Prop, data=df)
  CropProp_TreeProp  =occu(~Robel+Clouds   ~Crop_Prop+Tree_Prop, data=df)
  CropProp_TreekmHa  =occu(~Robel+Clouds   ~Crop_Prop+Tree_kmha, data=df)
  TreeProp           =occu(~Robel+Clouds   ~Tree_Prop, data=df)
  Tree_kmHa          =occu(~Robel+Clouds   ~Tree_kmha, data=df)
  
  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   HerbProp_Tree_kmha,   CropProp,   CropProp_TreeProp,   CropProp_TreekmHa,   TreeProp,   Tree_kmHa)
  names=c( "Null", "HerbProp", "HerbProp_TreeProp", "HerbProp_Tree_kmha", "CropProp", "CropProp_TreeProp", "CropProp_TreekmHa", "TreeProp", "Tree_kmHa")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 
SEWR_Landscape_mods = function(df) {
  Null               =occu(~Robel+Obs+StartTime+DOY   ~offset(log(Area_ha)), data=df)
  HerbProp           =occu(~Robel+Obs+StartTime+DOY   ~Herb_Prop+offset(log(Area_ha)), data=df)
  HerbProp_TreeProp  =occu(~Robel+Obs+StartTime+DOY   ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df)
  HerbProp_Tree_kmha =occu(~Robel+Obs+StartTime+DOY   ~Herb_Prop+Tree_kmha+offset(log(Area_ha)), data=df)
  CropProp           =occu(~Robel+Obs+StartTime+DOY   ~Crop_Prop+offset(log(Area_ha)), data=df)
  CropProp_TreeProp  =occu(~Robel+Obs+StartTime+DOY   ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df)
  CropProp_TreekmHa  =occu(~Robel+Obs+StartTime+DOY   ~Crop_Prop+Tree_kmha+offset(log(Area_ha)), data=df)
  TreeProp           =occu(~Robel+Obs+StartTime+DOY   ~Tree_Prop+offset(log(Area_ha)), data=df)
  Tree_kmHa          =occu(~Robel+Obs+StartTime+DOY   ~Tree_kmha+offset(log(Area_ha)), data=df)
  
  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   HerbProp_Tree_kmha,   CropProp,   CropProp_TreeProp,   CropProp_TreekmHa,   TreeProp,   Tree_kmHa)
  names=c( "Null", "HerbProp", "HerbProp_TreeProp", "HerbProp_Tree_kmha", "CropProp", "CropProp_TreeProp", "CropProp_TreekmHa", "TreeProp", "Tree_kmHa")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 

DICK_Landscape_mods(DICK_PCount) #>>Dickcissels: no landscape  ----
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # TreeProp           14 1321.734    0.0000 0.3778 0.3778 -646.8668
      # CropProp_TreeProp  15 1322.232    0.4987 0.2945 0.6723 -646.1162
      # HerbProp_TreeProp  15 1322.368    0.6339 0.2752 0.9475 -646.1838
      # Tree_kmHa          14 1327.197    5.4634 0.0246 0.9721 -649.5986
      # HerbProp_Tree_kmha 15 1327.979    6.2456 0.0166 0.9887 -648.9896
      # CropProp_TreekmHa  15 1329.177    7.4429 0.0091 0.9979 -649.5883
      # HerbProp           14 1332.490   10.7566 0.0017 0.9996 -652.2452
      # CropProp           14 1336.761   15.0273 0.0002 0.9998 -654.3805
      # Null               13 1336.976   15.2426 0.0002 1.0000 -655.4882
        
    DICK_landscape_mod   =pcount(~Obs+DOY+StartTime+Robel ~Tree_Prop+Crop_Prop, data=DICK_PCount, mixture="ZIP",K=100)
    confint(DICK_landscape_mod, type="state", level=0.85) #indicates Robel is uninformative parameter
        
BOBO_Landscape_mods(BOBO_PCount) #>>Bobolinks: crop_prop+tree_prop----
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # CropProp_TreeProp  16 1113.195    0.0000 0.5686 0.5686 -540.5975
      # HerbProp_TreeProp  16 1113.777    0.5817 0.4251 0.9937 -540.8884
      # HerbProp_Tree_kmha 16 1122.210    9.0154 0.0063 1.0000 -545.1052
      # CropProp_TreekmHa  16 1135.672   22.4772 0.0000 1.0000 -551.8361
      # Tree_kmHa          15 1166.119   52.9240 0.0000 1.0000 -568.0595
      # TreeProp           15 1181.493   68.2979 0.0000 1.0000 -575.7465
      # HerbProp           15 1188.024   74.8286 0.0000 1.0000 -579.0118
      # CropProp           15 1319.391  206.1963 0.0000 1.0000 -644.6957
      # Null               14 1320.918  207.7231 0.0000 1.0000 -646.4591

    BOBO_landscape_mod=pcount(~Obs+DOY+StartTime+Robel+Wind+Clouds ~Crop_Prop+Tree_Prop, data=BOBO_PCount, mixture="ZIP",K=100)
    confint(BOBO_landscape_mod, type="state", level=0.85) #indicates Wind and StartTime are uninformative parameters


GRSP_Landscape_mods(GRSP_PCount) #>>Grasshoopper Sparrow:no landscape----
        #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # HerbProp           13 703.8379    0.0000 0.3091 0.3091 -338.9190
        # HerbProp_Tree_kmha 14 705.4318    1.5939 0.1393 0.4484 -338.7159
        # Null               12 705.5563    1.7183 0.1309 0.5794 -340.7781
        # HerbProp_TreeProp  14 705.8264    1.9885 0.1144 0.6937 -338.9132
        # CropProp_TreeProp  14 706.1548    2.3169 0.0971 0.7908 -339.0774
        # CropProp           13 706.7796    2.9416 0.0710 0.8618 -340.3898
        # TreeProp           13 707.3448    3.5069 0.0535 0.9154 -340.6724
        # Tree_kmHa          13 707.5514    3.7135 0.0483 0.9636 -340.7757
        # CropProp_TreekmHa  14 708.1180    4.2800 0.0364 1.0000 -340.0590
    GRSP_landscape_mod   =pcount(~Obs+Robel ~Herb_Prop, data=GRSP_PCount, mixture="ZIP",K=100)
    confint(GRSP_landscape_mod, type="state", level=0.85) #all good

EAME_Landscape_mods(EAME_PCount) #>>Meadowlarks:Crop_Prop+Tree_Prop----
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # CropProp_TreeProp  6 729.6449    0.0000 0.3322 0.3322 -358.8225
      # HerbProp_TreeProp  6 730.3531    0.7082 0.2331 0.5653 -359.1766
      # HerbProp_Tree_kmha 6 730.8052    1.1603 0.1859 0.7512 -359.4026
      # CropProp_TreekmHa  6 731.6829    2.0380 0.1199 0.8711 -359.8415
      # Tree_kmHa          5 732.5221    2.8772 0.0788 0.9499 -361.2610
      # TreeProp           5 733.7784    4.1335 0.0421 0.9920 -361.8892
      # HerbProp           5 737.2575    7.6126 0.0074 0.9994 -363.6287
      # Null               4 742.8222   13.1773 0.0005 0.9998 -367.4111
      # CropProp           5 744.8154   15.1705 0.0002 1.0000 -367.4077
  EAME_landscape_mod   =pcount(~Robel ~Crop_Prop+Tree_Prop, data=EAME_PCount, mixture="ZIP",K=100)
  confint(EAME_landscape_mod, type="state", level=0.85) #all good

RWBL_Landscape_mods(RWBL_PCount) #>>Red-winged Blackbird: Herb_Prop----
      #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbProp           14 1155.488    0.0000 0.3696 0.3696 -563.7440
      # CropProp_TreeProp  15 1157.016    1.5277 0.1722 0.5417 -563.5078
      # CropProp_TreekmHa  15 1157.135    1.6474 0.1622 0.7039 -563.5677
      # HerbProp_Tree_kmha 15 1157.220    1.7323 0.1554 0.8593 -563.6101
      # HerbProp_TreeProp  15 1157.421    1.9332 0.1406 0.9999 -563.7106
      # CropProp           14 1172.003   16.5146 0.0001 1.0000 -572.0013
      # Tree_kmHa          14 1177.588   22.1004 0.0000 1.0000 -574.7942
      # TreeProp           14 1180.701   25.2132 0.0000 1.0000 -576.3506
      # Null               13 1182.320   26.8315 0.0000 1.0000 -578.1597

  RWBL_landscape_mod   =pcount(~Obs+Robel+StartTime+DOY ~Herb_Prop, data=RWBL_PCount, mixture="ZIP",K=100)
  confint(RWBL_landscape_mod, type="state", level=0.85) 
  
BHCO_Landscape_mods(BHCO_PCount) #>>Cowbird: Herb_Prop----
  #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
  # HerbProp           14 1155.488    0.0000 0.3696 0.3696 -563.7440
  # CropProp_TreeProp  15 1157.016    1.5277 0.1722 0.5417 -563.5078
  # CropProp_TreekmHa  15 1157.135    1.6474 0.1622 0.7039 -563.5677
  # HerbProp_Tree_kmha 15 1157.220    1.7323 0.1554 0.8593 -563.6101
  # HerbProp_TreeProp  15 1157.421    1.9332 0.1406 0.9999 -563.7106
  # CropProp           14 1172.003   16.5146 0.0001 1.0000 -572.0013
  # Tree_kmHa          14 1177.588   22.1004 0.0000 1.0000 -574.7942
  # TreeProp           14 1180.701   25.2132 0.0000 1.0000 -576.3506
  # Null               13 1182.320   26.8315 0.0000 1.0000 -578.1597
  
  BHCO_landscape_mod   =pcount(~Obs+DOY+Clouds ~Herb_Prop, data=BHCO_PCount, mixture="ZIP",K=100)
  confint(BHCO_landscape_mod, type="state", level=0.85) 
  
  
HESP_Landscape_mods(HESP_Occu) #>>Henslow's Sparrow:Herb_Prop----
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbProp           5 325.6404    0.0000 0.4402 0.4402 -157.8202
      # HerbProp_Tree_kmha 6 327.4423    1.8019 0.1788 0.6190 -157.7212
      # HerbProp_TreeProp  6 327.5794    1.9390 0.1670 0.7860 -157.7897
      # CropProp_TreeProp  6 328.1220    2.4816 0.1273 0.9133 -158.0610
      # CropProp_TreekmHa  6 329.7162    4.0759 0.0574 0.9707 -158.8581
      # CropProp           5 331.0860    5.4456 0.0289 0.9996 -160.5430
      # Null               4 340.8620   15.2216 0.0002 0.9998 -166.4310
      # Tree_kmHa          5 342.1166   16.4763 0.0001 0.9999 -166.0583
      # TreeProp           5 342.7773   17.1369 0.0001 1.0000 -166.3887

    HESP_landscape_mod   =occu(~Clouds+Robel ~Herb_Prop, data=HESP_Occu)
    confint(HESP_landscape_mod, type="state", level=0.85) #all good

SEWR_Landscape_mods(SEWR_Occu) #>>Sedge Wren: Tree_kmha----
      #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbProp_Tree_kmha 15 257.9442    0.0000 0.2526 0.2526 -113.9721
      # CropProp_TreekmHa  15 258.1312    0.1871 0.2300 0.4826 -114.0656
      # Tree_kmHa          14 258.8910    0.9468 0.1573 0.6399 -115.4455
      # CropProp_TreeProp  15 259.1410    1.1969 0.1388 0.7787 -114.5705
      # HerbProp_TreeProp  15 259.1641    1.2199 0.1372 0.9159 -114.5821
      # TreeProp           14 260.1718    2.2276 0.0829 0.9989 -116.0859
      # HerbProp           14 270.2262   12.2820 0.0005 0.9994 -121.1131
      # Null               13 270.6668   12.7226 0.0004 0.9998 -122.3334
      # CropProp           14 272.5618   14.6176 0.0002 1.0000 -122.2809


    SEWR_landscape_mod   =occu(~Obs+StartTime+DOY+Robel ~Herb_Prop+Tree_kmha, data=SEWR_Occu)
    confint(SEWR_landscape_mod, type="state", level=0.85) #drop Herb_Prop as uninformative
    

#**************************************************************************#####
#7 [HERB] TREATMENT----
#***************************************************************************

DICK_Abundance_mods = function(df) {
    Null                       =pcount(~Obs+StartTime+DOY   ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
    TSH                        =pcount(~Obs+StartTime+DOY   ~TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
    TSH_GrazingTreat           =pcount(~Obs+StartTime+DOY   ~TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
    HerbTreat                  =pcount(~Obs+StartTime+DOY   ~HerbTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
    HerbTreat_GrazingTreat     =pcount(~Obs+StartTime+DOY   ~HerbTreat+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
    HerbTreat_TSH              =pcount(~Obs+StartTime+DOY   ~HerbTreat*TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
    HerbTreat_TSH_GrazingTreat =pcount(~Obs+StartTime+DOY   ~HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
    GrazingTreat               =pcount(~Obs+StartTime+DOY   ~GrazingTreat+offset(log(Area_ha)), mixture="ZIP",data=df, K=100)
    
    mods=list(Null,TSH,TSH_GrazingTreat,HerbTreat, HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
    names=c("Null","TSH","TSH+GrazingTreat","HerbTreat", "HerbTreat+GrazingTreat","HerbTreat*TSH","HerbTreat*TSH+GrazingTreat","GrazingTreat")
    
    print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)

}
BOBO_Abundance_mods = function(df) {
  null                       =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  TSH                        =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~Crop_Prop+Tree_Prop+TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TSH_GrazingTreat           =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~Crop_Prop+Tree_Prop+TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat                  =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~Crop_Prop+Tree_Prop+HerbTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_GrazingTreat     =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~Crop_Prop+Tree_Prop+HerbTreat+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_TSH              =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~Crop_Prop+Tree_Prop+HerbTreat*TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_TSH_GrazingTreat =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~Crop_Prop+Tree_Prop+HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat               =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~Crop_Prop+Tree_Prop+GrazingTreat+offset(log(Area_ha)), mixture="ZIP",data=df, K=100)
  
  mods=list(null,TSH,TSH_GrazingTreat,HerbTreat, HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  names=c("null","TSH","TSH+GrazingTreat","HerbTreat", "HerbTreat+GrazingTreat","HerbTreat*TSH","HerbTreat*TSH+GrazingTreat","GrazingTreat")
  
    print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
}
GRSP_Abundance_mods = function(df) {
  null                       =pcount(~Obs+Robel   ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  TSH                        =pcount(~Obs+Robel   ~TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TSH_GrazingTreat           =pcount(~Obs+Robel   ~TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat                  =pcount(~Obs+Robel   ~HerbTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_GrazingTreat     =pcount(~Obs+Robel   ~HerbTreat+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_TSH              =pcount(~Obs+Robel   ~HerbTreat*TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_TSH_GrazingTreat =pcount(~Obs+Robel   ~HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat               =pcount(~Obs+Robel   ~GrazingTreat+offset(log(Area_ha)), mixture="ZIP",data=df, K=100)
  
  mods=list(null,TSH,TSH_GrazingTreat,HerbTreat, HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  names=c("null","TSH","TSH+GrazingTreat","HerbTreat", "HerbTreat+GrazingTreat","HerbTreat*TSH","HerbTreat*TSH+GrazingTreat","GrazingTreat")
  
    print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
}
EAME_Abundance_mods = function(df) {
  null                       =pcount(~Robel   ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  TSH                        =pcount(~Robel   ~Crop_Prop+Tree_Prop+TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TSH_GrazingTreat           =pcount(~Robel   ~Crop_Prop+Tree_Prop+TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat                  =pcount(~Robel   ~Crop_Prop+Tree_Prop+HerbTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_GrazingTreat     =pcount(~Robel   ~Crop_Prop+Tree_Prop+HerbTreat+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_TSH              =pcount(~Robel   ~Crop_Prop+Tree_Prop+HerbTreat*TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_TSH_GrazingTreat =pcount(~Robel   ~Crop_Prop+Tree_Prop+HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat               =pcount(~Robel   ~Crop_Prop+Tree_Prop+GrazingTreat+offset(log(Area_ha)), mixture="ZIP",data=df, K=100)
  
  mods=list(null,TSH,TSH_GrazingTreat,HerbTreat, HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  names=c("null","TSH","TSH+GrazingTreat","HerbTreat", "HerbTreat+GrazingTreat","HerbTreat*TSH","HerbTreat*TSH+GrazingTreat","GrazingTreat")
  
  #mods=list(null,TSH,TSH_GrazingTreat,HerbTreat,HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  #names=c("null","TSH","TSH_GrazingTreat","HerbTreat","HerbTreat_GrazingTreat","HerbTreat_TSH","HerbTreat_TSH_GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
}
RWBL_Abundance_mods = function(df) {
  null                       =pcount(~Robel+Obs+DOY   ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  TSH                        =pcount(~Robel+Obs+DOY   ~Herb_Prop+TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TSH_GrazingTreat           =pcount(~Robel+Obs+DOY   ~Herb_Prop+TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat                  =pcount(~Robel+Obs+DOY   ~Herb_Prop+HerbTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_GrazingTreat     =pcount(~Robel+Obs+DOY   ~Herb_Prop+HerbTreat+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_TSH              =pcount(~Robel+Obs+DOY   ~Herb_Prop+HerbTreat*TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_TSH_GrazingTreat =pcount(~Robel+Obs+DOY   ~Herb_Prop+HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat               =pcount(~Robel+Obs+DOY   ~Herb_Prop+GrazingTreat+offset(log(Area_ha)), mixture="ZIP",data=df, K=100)
  
  mods=list(null,TSH,TSH_GrazingTreat,HerbTreat, HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  names=c("null","TSH","TSH+GrazingTreat","HerbTreat", "HerbTreat+GrazingTreat","HerbTreat*TSH","HerbTreat*TSH+GrazingTreat","GrazingTreat")
  
  #mods=list(null,TSH,TSH_GrazingTreat,HerbTreat,HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  #names=c("null","TSH","TSH_GrazingTreat","HerbTreat","HerbTreat_GrazingTreat","HerbTreat_TSH","HerbTreat_TSH_GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
}
BHCO_Abundance_mods = function(df) {
  null                       =pcount(~Obs+DOY+Clouds   ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  TSH                        =pcount(~Obs+DOY+Clouds   ~Herb_Prop+TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TSH_GrazingTreat           =pcount(~Obs+DOY+Clouds   ~Herb_Prop+TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat                  =pcount(~Obs+DOY+Clouds   ~Herb_Prop+HerbTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_GrazingTreat     =pcount(~Obs+DOY+Clouds   ~Herb_Prop+HerbTreat+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_TSH              =pcount(~Obs+DOY+Clouds   ~Herb_Prop+HerbTreat*TSH+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbTreat_TSH_GrazingTreat =pcount(~Obs+DOY+Clouds   ~Herb_Prop+HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat               =pcount(~Obs+DOY+Clouds   ~Herb_Prop+GrazingTreat+offset(log(Area_ha)), mixture="ZIP",data=df, K=100)
  
  mods=list(null,TSH,TSH_GrazingTreat,HerbTreat, HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  names=c("null","TSH","TSH+GrazingTreat","HerbTreat", "HerbTreat+GrazingTreat","HerbTreat*TSH","HerbTreat*TSH+GrazingTreat","GrazingTreat")
  
  #mods=list(null,TSH,TSH_GrazingTreat,HerbTreat,HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  #names=c("null","TSH","TSH_GrazingTreat","HerbTreat","HerbTreat_GrazingTreat","HerbTreat_TSH","HerbTreat_TSH_GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
}
HESP_Occu_mods = function(df) {
  null                       =occu(~Robel+Clouds   ~Herb_Prop, data=df)
  TSH                        =occu(~Robel+Clouds   ~Herb_Prop+TSH, data=df)
  TSH_GrazingTreat           =occu(~Robel+Clouds   ~Herb_Prop+TSH+GrazingTreat, data=df)
  HerbTreat                  =occu(~Robel+Clouds   ~Herb_Prop+HerbTreat, data=df)
  HerbTreat_GrazingTreat     =occu(~Robel+Clouds   ~Herb_Prop+HerbTreat+GrazingTreat, data=df)
  HerbTreat_TSH              =occu(~Robel+Clouds   ~Herb_Prop+HerbTreat*TSH, data=df)
  HerbTreat_TSH_GrazingTreat =occu(~Robel+Clouds   ~Herb_Prop+HerbTreat*TSH+GrazingTreat, data=df)
  GrazingTreat               =occu(~Robel+Clouds   ~Herb_Prop+GrazingTreat,data=df)
  
  mods=list(null,TSH,TSH_GrazingTreat,HerbTreat, HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  names=c("null","TSH","TSH+GrazingTreat","HerbTreat", "HerbTreat+GrazingTreat","HerbTreat*TSH","HerbTreat*TSH+GrazingTreat","GrazingTreat")
  
  #mods=list(null,TSH,TSH_GrazingTreat,HerbTreat,HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  #names=c("null","TSH","TSH_GrazingTreat","HerbTreat","HerbTreat_GrazingTreat","HerbTreat_TSH","HerbTreat_TSH_GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
}
SEWR_Occu_mods = function(df) {
  null                       =occu(~Obs+StartTime+DOY+Robel   ~Tree_kmha+offset(log(Area_ha)), data=df)
  TSH                        =occu(~Obs+StartTime+DOY+Robel   ~Tree_kmha+TSH+offset(log(Area_ha)), data=df)
  TSH_GrazingTreat           =occu(~Obs+StartTime+DOY+Robel   ~Tree_kmha+TSH+GrazingTreat+offset(log(Area_ha)), data=df)
  HerbTreat                  =occu(~Obs+StartTime+DOY+Robel   ~Tree_kmha+HerbTreat+offset(log(Area_ha)), data=df)
  HerbTreat_GrazingTreat     =occu(~Obs+StartTime+DOY+Robel   ~Tree_kmha+HerbTreat+GrazingTreat+offset(log(Area_ha)), data=df)
  HerbTreat_TSH              =occu(~Obs+StartTime+DOY+Robel   ~Tree_kmha+HerbTreat*TSH+offset(log(Area_ha)), data=df)
  HerbTreat_TSH_GrazingTreat =occu(~Obs+StartTime+DOY+Robel   ~Tree_kmha+HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=df)
  GrazingTreat               =occu(~Obs+StartTime+DOY+Robel   ~Tree_kmha+GrazingTreat+offset(log(Area_ha)),data=df)
  
  mods=list(null,TSH,TSH_GrazingTreat,HerbTreat, HerbTreat_GrazingTreat,HerbTreat_TSH,HerbTreat_TSH_GrazingTreat,GrazingTreat)
  names=c("null","TSH","TSH+GrazingTreat","HerbTreat", "HerbTreat+GrazingTreat","HerbTreat*TSH","HerbTreat*TSH+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}

DICK_Abundance_mods(DICK_PCount) #>>Dickcissels: HerbTreat*TSH+GrazingTreat----
        #                             K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # HerbTreat*TSH+GrazingTreat 26 1269.460    0.0000 0.9997 0.9997 -608.7300
        # HerbTreat+GrazingTreat     17 1285.968   16.5083 0.0003 1.0000 -625.9841
        # HerbTreat*TSH              24 1299.533   30.0733 0.0000 1.0000 -625.7666
        # TSH+GrazingTreat           18 1308.833   39.3732 0.0000 1.0000 -636.4166
        # HerbTreat                  15 1309.005   39.5446 0.0000 1.0000 -639.5023
        # GrazingTreat               15 1314.918   45.4576 0.0000 1.0000 -642.4588
        # TSH                        16 1335.103   65.6431 0.0000 1.0000 -651.5516
        # Null                       13 1336.976   67.5164 0.0000 1.0000 -655.4882


BOBO_Abundance_mods(BOBO_PCount) #>>Bobolinks: HerbTreat*TSH+GrazingTreat----
        #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # HerbTreat*TSH+GrazingTreat 30 1095.940    0.0000 0.8913 0.8913 -517.9700
        # GrazingTreat               19 1101.115    5.1748 0.0670 0.9584 -531.5574
        # HerbTreat+GrazingTreat     21 1102.982    7.0417 0.0264 0.9847 -530.4908
        # TSH+GrazingTreat           22 1104.412    8.4723 0.0129 0.9976 -530.2062
        # HerbTreat*TSH              28 1108.914   12.9744 0.0014 0.9990 -526.4572
        # null                       17 1110.068   14.1278 0.0008 0.9997 -538.0339
        # HerbTreat                  19 1113.120   17.1796 0.0002 0.9999 -537.5598
        # TSH                        20 1114.030   18.0904 0.0001 1.0000 -537.0152

GRSP_Abundance_mods(GRSP_PCount) #>>Grasshopper Sparrow: null ----
        #                             K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # null                       12 705.5563    0.0000 0.4357 0.4357 -340.7781
        # HerbTreat                  14 706.5439    0.9877 0.2659 0.7016 -339.2720
        # HerbTreat+GrazingTreat     16 708.0443    2.4881 0.1256 0.8271 -338.0222
        # GrazingTreat               14 708.2514    2.6952 0.1132 0.9403 -340.1257
        # TSH                        15 710.7486    5.1923 0.0325 0.9728 -340.3743
        # HerbTreat*TSH              23 713.0159    7.4596 0.0105 0.9833 -333.5079
        # TSH+GrazingTreat           17 713.3546    7.7984 0.0088 0.9921 -339.6773
        # HerbTreat*TSH+GrazingTreat 25 713.5797    8.0234 0.0079 1.0000 -331.7899

EAME_Abundance_mods(EAME_PCount) #>>Meadowlarks:HerbTreat*TSH+GrazingTreat Kinda top----
        #                              K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # GrazingTreat                8 726.1389    0.0000 0.4142 0.4142 -355.0694
        # HerbTreat*TSH+GrazingTreat 19 726.8314    0.6925 0.2929 0.7071 -344.4157
        # HerbTreat+GrazingTreat     10 729.1554    3.0165 0.0917 0.7988 -354.5777
        # null                        6 729.6449    3.5060 0.0718 0.8705 -358.8225
        # HerbTreat*TSH              17 729.7224    3.5835 0.0690 0.9395 -347.8612
        # TSH+GrazingTreat           11 730.9845    4.8456 0.0367 0.9763 -354.4923
        # HerbTreat                   8 732.4083    6.2694 0.0180 0.9943 -358.2041
        # TSH                         9 734.7070    8.5681 0.0057 1.0000 -358.3535 #Probably grazing is the whole deal - but examine HerbTreat*TSH + GrazingTreat

RWBL_Abundance_mods(RWBL_PCount) #>>Red-winged Blackbird: HerbTreat*TSH ----
      #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbTreat*TSH              25 1142.886    0.0000 0.5171 0.5171 -546.4429
      # HerbTreat*TSH+GrazingTreat 27 1143.107    0.2210 0.4630 0.9802 -544.5534
      # TSH                        17 1150.625    7.7393 0.0108 0.9910 -558.3126
      # TSH+GrazingTreat           19 1151.911    9.0249 0.0057 0.9967 -556.9554
      # null                       14 1155.488   12.6021 0.0009 0.9976 -563.7440
      # HerbTreat                  16 1155.513   12.6268 0.0009 0.9985 -561.7564
      # GrazingTreat               16 1155.560   12.6739 0.0009 0.9995 -561.7799
      # HerbTreat+GrazingTreat     18 1156.617   13.7312 0.0005 1.0000 -560.3085


BHCO_Abundance_mods(BHCO_PCount) #>>Cowbirds: TSH+GrazingTreat----
      #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # TSH+GrazingTreat           19 674.2758    0.0000 0.4907 0.4907 -318.1379
      # GrazingTreat               16 675.5018    1.2260 0.2659 0.7566 -321.7509
      # HerbTreat+GrazingTreat     18 677.0198    2.7440 0.1245 0.8811 -320.5099
      # HerbTreat                  16 678.0723    3.7965 0.0735 0.9546 -323.0361
      # null                       14 679.7879    5.5120 0.0312 0.9858 -325.8939
      # HerbTreat*TSH+GrazingTreat 27 682.6680    8.3922 0.0074 0.9932 -314.3340
      # TSH                        17 683.2655    8.9897 0.0055 0.9986 -324.6328
      # HerbTreat*TSH              25 686.0451   11.7692 0.0014 1.0000 -318.0225

HESP_Occu_mods(HESP_Occu) #>>Henslow's Sparrow:HerbTreat*TSH ----
      #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbTreat*TSH              16 312.0452    0.0000 0.8258 0.8258 -140.0226
      # TSH+GrazingTreat           10 316.4121    4.3669 0.0930 0.9188 -148.2060
      # HerbTreat*TSH+GrazingTreat 18 317.5407    5.4955 0.0529 0.9717 -140.7703
      # TSH                         8 319.2027    7.1575 0.0230 0.9948 -151.6014
      # GrazingTreat                7 323.9134   11.8682 0.0022 0.9969 -154.9567
      # HerbTreat+GrazingTreat      9 324.7219   12.6767 0.0015 0.9984 -153.3610
      # null                        5 325.6404   13.5952 0.0009 0.9993 -157.8202
      # HerbTreat                   7 326.2648   14.2196 0.0007 1.0000 -156.1324

SEWR_Occu_mods(SEWR_Occu) #>>Sedge Wren: HerbTreat+GrazingTreat (kind of)----
      #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # GrazingTreat               16 244.0548    0.0000 0.5042 0.5042 -106.0274
      # HerbTreat+GrazingTreat     18 244.3886    0.3338 0.4267 0.9309 -104.1943
      # TSH+GrazingTreat           19 248.1710    4.1162 0.0644 0.9953 -105.0855
      # HerbTreat*TSH+GrazingTreat 27 253.8059    9.7510 0.0038 0.9991  -99.9029
      # HerbTreat                  16 257.6437   13.5889 0.0006 0.9997 -112.8219
      # null                       14 258.8910   14.8361 0.0003 1.0000 -115.4455
      # TSH                        17 263.9447   19.8899 0.0000 1.0000 -114.9724
      # HerbTreat*TSH              25 267.2178   23.1630 0.0000 1.0000 -108.6089
#***************************************************************************####
#8 [HERB] CHOSEN MODELS----
#***************************************************************************
Coefficients =function(model) {
  coefs=c(coef(model, type = "state"))
  coefs=data.frame(coefs)
}

ConfidenceIntervals = function(model,coefs) {
  CIs = as.data.frame(confint(model, type = "state", level = 0.85))
  colnames(CIs)=c("LCL", "UCL") #renames columns
  CIs
  coefs$LCL=CIs$LCL
  coefs$UCL=CIs$UCL
  coefs
} #confidence intervals

#Top dickcissel model
DICK_Top=pcount(~Obs+StartTime+DOY   ~HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=DICK_PCount, mixture="ZIP",K=100)
  #summary(DICK_Top) 
  DICK_Abund_coef_df=Coefficients(DICK_Top) 

#Top bobolink model
BOBO_Top=pcount(~Obs+DOY+Robel+Clouds   ~Crop_Prop+Tree_Prop+HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=BOBO_PCount, mixture="ZIP",K=100)
  #summary(BOBO_Top)
  BOBO_Abund_coef_df=Coefficients(BOBO_Top) 
  
#Top grasshopper sparrow model
GRSP_KindaTop=pcount(~Robel+Obs ~offset(log(Area_ha)), data=GRSP_PCount, mixture="ZIP",K=100)
#summary(GRSP_KindaTop)
GRSP_Abund_coef_df=Coefficients(GRSP_KindaTop) 

#Top meadowlarks model
EAME_KindaTop=pcount(~Robel   ~Crop_Prop+Tree_Prop+HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=EAME_PCount, mixture="ZIP",K=100)
  #summary(EAME_Top)
  EAME_Abund_coef_df=Coefficients(EAME_KindaTop) 
  
#Top red-winged blackbird model
RWBL_Top=pcount(~Robel+Obs+DOY   ~Herb_Prop+HerbTreat*TSH+offset(log(Area_ha)), data=RWBL_PCount, mixture="ZIP",K=100)
  #summary(RWBL_Top)
  RWBL_Abund_coef_df=Coefficients(RWBL_Top) 
  
#Top Cowbird models
BHCO_Top=pcount(~Robel+Obs+DOY   ~Herb_Prop+GrazingTreat+TSH+offset(log(Area_ha)), data=RWBL_PCount, mixture="ZIP",K=100)
  #summary(BHCO_Top)
  BHCO_Abund_coef_df=Coefficients(BHCO_Top) 
  
#Top Henslow's sparrow model
HESP_Top=occu(~Robel+Clouds   ~Herb_Prop+HerbTreat*TSH+GrazingTreat, data=HESP_Occu)
  #summary(HESP_Top)
  HESP_Abund_coef_df=Coefficients(HESP_Top) 

#Top Sedge Wren model
SEWR_Top=occu(~Obs+StartTime+DOY+Robel   ~Tree_kmha+HerbTreat+GrazingTreat+offset(log(Area_ha)), data=SEWR_Occu)
#summary(SEWR_Top)
SEWR_Abund_coef_df=Coefficients(SEWR_Top)  

#Coefs and CIs
DICK_Coefs_CIs=ConfidenceIntervals(DICK_Top,DICK_Abund_coef_df) #>>Dickcissels----
print(DICK_Coefs_CIs)
        #                              coefs         LCL        UCL
        # lam(Int)                0.532466098  0.01262322 1.05230898
        # lam(HerbTreatSnS)      -0.453542602 -0.97383017 0.06674496
        # lam(HerbTreatSpr)       0.563409031  0.12475163 1.00206643
        # lam(TSHb)               0.002879951 -0.46472726 0.47048716
        # lam(TSHc)               0.098157376 -0.38280452 0.57911927
        # lam(TSHd)              -0.079207347 -0.66758868 0.50917399
        # lam(GrazingTreatNone)   0.651204925  0.46955812 0.83285173
        # lam(GrazingTreatSLS)    0.693438903  0.48800766 0.89887015
        # lam(HerbTreatSnS:TSHb)  1.459356058  0.83936642 2.07934570
        # lam(HerbTreatSpr:TSHb)  0.280487955 -0.28097320 0.84194911
        # lam(HerbTreatSnS:TSHc)  1.078848598  0.44805863 1.70963857
        # lam(HerbTreatSpr:TSHc)  0.001230180 -0.56815275 0.57061311
        # lam(HerbTreatSnS:TSHd)  1.768619648  1.05341203 2.48382727
        # lam(HerbTreatSpr:TSHd)  0.512315233 -0.15114623 1.17577670

BOBO_Coefs_CIs=ConfidenceIntervals(BOBO_Top,BOBO_Abund_coef_df) #>>Bobolinks----
print(BOBO_Coefs_CIs)
        #                             coefs        LCL         UCL
        # lam(Int)                1.62267650  0.9130294  2.33232357
        # lam(Crop_Prop)         -3.15616139 -3.8732346 -2.43908816
        # lam(Tree_Prop)         -5.47936671 -6.6567204 -4.30201301
        # lam(HerbTreatSnS)      -0.60802976 -0.9876566 -0.22840289
        # lam(HerbTreatSpr)      -0.27269766 -0.5823375  0.03694215
        # lam(TSHb)              -0.01678953 -0.3326970  0.29911791
        # lam(TSHc)              -0.53439884 -0.9170410 -0.15175669
        # lam(TSHd)              -0.29163181 -0.7686750  0.18541143
        # lam(GrazingTreatNone)   2.27105090  1.6790930  2.86300876
        # lam(GrazingTreatSLS)    1.98532678  1.4455881  2.52506550
        # lam(HerbTreatSnS:TSHb)  0.18293623 -0.3094161  0.67528853
        # lam(HerbTreatSpr:TSHb) -0.16461270 -0.6174663  0.28824089
        # lam(HerbTreatSnS:TSHc)  1.25755404  0.7543155  1.76079262
        # lam(HerbTreatSpr:TSHc)  0.17234034 -0.3480667  0.69274736
        # lam(HerbTreatSnS:TSHd)  1.03180409  0.4980863  1.56552184
        # lam(HerbTreatSpr:TSHd)  0.34678466 -0.1773445  0.87091380

EAME_Coefs_CIs=ConfidenceIntervals(EAME_KindaTop,EAME_Abund_coef_df) #>>Meadowlarks----
print(EAME_Coefs_CIs)
      #                              coefs         LCL         UCL
      # lam(Int)                3.417822980  2.62762045  4.20802551
      # lam(Crop_Prop)         -2.662190266 -4.06823452 -1.25614601
      # lam(Tree_Prop)         -4.447757254 -6.02078229 -2.87473222
      # lam(HerbTreatSnS)       0.397097183 -0.07396503  0.86815940
      # lam(HerbTreatSpr)       0.008407978 -0.57059598  0.58741193
      # lam(TSHb)               0.295403285 -0.14770387  0.73851044
      # lam(TSHc)               0.282907128 -0.15318541  0.71899967
      # lam(TSHd)              -0.789959433 -1.49865156 -0.08126730
      # lam(GrazingTreatNone)  -0.550289890 -1.01837804 -0.08220174
      # lam(GrazingTreatSLS)    0.131696498 -0.16675962  0.43015262
      # lam(HerbTreatSnS:TSHb) -1.157136829 -1.85273101 -0.46154265
      # lam(HerbTreatSpr:TSHb)  0.081203338 -0.62386530  0.78627198
      # lam(HerbTreatSnS:TSHc) -1.188993237 -1.89681326 -0.48117321
      # lam(HerbTreatSpr:TSHc) -0.215086569 -0.96252028  0.53234715
      # lam(HerbTreatSnS:TSHd)  0.745729783 -0.09606154  1.58752110
      # lam(HerbTreatSpr:TSHd)  0.691906632 -0.27288662  1.65669988

RWBL_Coefs_CIs=ConfidenceIntervals(RWBL_Top,RWBL_Abund_coef_df) #>>Red-winged blackbirds---
print(RWBL_Coefs_CIs)
      #                             coefs        LCL        UCL
      # lam(Int)                0.15411555 -0.4237856  0.7320167
      # lam(Herb_Prop)          2.95380837  2.2997784  3.6078383
      # lam(HerbTreatSnS)      -1.42478362 -1.9402742 -0.9092930
      # lam(HerbTreatSpr)      -0.64605137 -0.9908646 -0.3012382
      # lam(TSHb)              -0.08607556 -0.4450311  0.2728799
      # lam(TSHc)              -0.58801439 -1.0155820 -0.1604468
      # lam(TSHd)              -0.95952507 -1.5086251 -0.4104251
      # lam(HerbTreatSnS:TSHb)  1.53625308  0.9144819  2.1580242
      # lam(HerbTreatSpr:TSHb)  0.57917447  0.1080458  1.0503032
      # lam(HerbTreatSnS:TSHc)  1.49840208  0.8455434  2.1512608
      # lam(HerbTreatSpr:TSHc)  1.01610693  0.4899541  1.5422597
      # lam(HerbTreatSnS:TSHd)  1.53255097  0.7978606  2.2672413
      # lam(HerbTreatSpr:TSHd)  0.87957495  0.2653434  1.4938065

HESP_Coefs_CIs=ConfidenceIntervals(HESP_Top,HESP_Abund_coef_df) #>>Henslow's Sparrow----
print(HESP_Coefs_CIs)
      #                              coefs         LCL        UCL
      # psi(Int)               -15.4948586 -24.2108975 -6.7788197
      # psi(Herb_Prop)          21.1909005   7.9573591 34.4244418
      # psi(HerbTreatSnS)       -2.9854929  -6.5785749  0.6075891
      # psi(HerbTreatSpr)       -3.0956499  -7.1463280  0.9550282
      # psi(TSHb)               12.9862281 -36.4963984 62.4688546
      # psi(TSHc)                3.3421878  -0.2889698  6.9733455
      # psi(TSHd)                0.4608787  -2.7003511  3.6221085
      # psi(GrazingTreatNone)    2.9611337   0.8948493  5.0274182
      # psi(GrazingTreatSLS)     2.3649724   0.1229802  4.6069646
      # psi(HerbTreatSnS:TSHb)  -7.5949212 -57.0735255 41.8836831
      # psi(HerbTreatSpr:TSHb)  -8.9130831 -58.4521023 40.6259361
      # psi(HerbTreatSnS:TSHc)  -1.0795718  -5.7242120  3.5650684
      # psi(HerbTreatSpr:TSHc)   4.0193684  -1.8671161  9.9058529
      # psi(HerbTreatSnS:TSHd)   5.4602413  -0.1512573 11.0717399
      # psi(HerbTreatSpr:TSHd)   4.0574921  -1.0859835  9.2009676
SEWR_Coefs_CIs=ConfidenceIntervals(SEWR_Top,SEWR_Abund_coef_df) #>>Henslow's Sparrow----
print(SEWR_Coefs_CIs)
      #                           coefs        LCL        UCL
      # psi(Int)              -6.8352834 -9.8841632 -3.7864037
      # psi(Tree_kmha)         4.4480822  0.8155239  8.0806406
      # psi(HerbTreatSnS)     -1.6296901 -3.1161466 -0.1432337
      # psi(HerbTreatSpr)      0.1183383 -1.2521957  1.4888723
      # psi(GrazingTreatNone)  4.0769826  2.3164027  5.8375624
      # psi(GrazingTreatSLS)   2.0769794  0.2507249  3.9032338

#mean detection rate at all surveys for all observers
mean(DICK_Herb$Date_1_Ord)+mean(DICK_Herb$Date_2_Ord)+mean(DICK_Herb$Date_3_Ord)+mean(DICK_Herb$Date_4_Ord)+mean(DICK_Herb$Date_5_Ord)/5 # 1.765923
mean(DICK_Herb$StartTime_1)+mean(DICK_Herb$StartTime_2)+mean(DICK_Herb$StartTime_3)+mean(DICK_Herb$StartTime_4)+mean(DICK_Herb$StartTime_5)/5 # 7.48
DICK_det_newdata = data.frame(Obs=c("ACW","ALL","BJE","BSV","JD","JJC","SBN","SR","TMS"),
                          StartTime = 7.48,
                          DOY = 1.77)
DICK_det_overall=as.data.frame(predict(DICK_Top, newdata=DICK_det_newdata,type = "det"))
mean(DICK_det_overall$Predicted)
mean((predict(DICK_Top, type = "det"))[,1])

mean((predict(BOBO_Top, type = "det"))[,1])
mean((predict(EAME_Top, type = "det"))[,1])
mean((predict(RWBL_Top, type = "det"))[,1])
mean((predict(HESP_Top, type = "det"))[,1])
mean((predict(SEWR_Top, type = "det"))[,1])


#***************************************************************************####
#9 [HERB] PREDICTION + GRAPHS----
#***************************************************************************

LandCoverMeans=matrix(c(mean(DICK_Herb$AvgOfHerbProp),mean(DICK_Herb$AvgOfCropProp),mean(DICK_Herb$AvgOfTreeProp),mean(DICK_Herb$AvgOfTree_kmHa)))

#  0.5892627  - Herbaceus percent cover
#  0.2016037  - Crop percent cover
#  0.1858188  - Tree percent cover
#  0.7172600  - Tree edge km/ha  

dodge <- position_dodge(width=0.9) #ggplot prep

#produce predicted values for abundance "state" variables across values in newdata
PredictedValues=function(model,newdata) {
  abundance_estimates = as.data.frame(predict(model, type = "state", newdata = newdata, level=0.85,appendData = T))
  abundance_estimates
}

#>>Dickcissel Graph----
DICK_newdata = data.frame(TSH=c("a","a","a","a","a","a","a","a","a","b","b","b","b","b","b","b","b","b","c","c","c","c","c","c","c","c","c","d","d","d","d","d","d","d","d","d"),
                     GrazingTreat = c("None","None","None","SLS","SLS","SLS","IES","IES","IES"),
                     HerbTreat = c("Con", "SnS", "Spr"),
                     Area_ha = 1,
                     Crop_Prop=0.2)

DICK_Predicted=PredictedValues(DICK_Top,DICK_newdata)
DICK_Predicted_Sum= DICK_Predicted %>% 
  group_by(TSH, HerbTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean) #averages out the grazing treatments so we can focus on herbicide treatments only
#View(DICK_Predicted_Sum) 

DICK_Predicted_Sum$HerbTreat=factor(DICK_Predicted_Sum$HerbTreat,levels=c("Con","Spr","SnS")) #ordering

DICK.plot <- ggplot(data = DICK_Predicted_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=TSH, 
               fill=HerbTreat), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,17), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"),labels=c("Control","Spray","Spray-and-Seed"))+
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12,color="black",face="bold"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  
  scale_x_discrete(breaks=c("a","b","c","d"),labels=c("1","2","3","4"))+
  geom_errorbar(aes(x = TSH, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbTreat), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Time Since Herbicide (y)", 
       fill = "Herbicide Treatment") + #sets label for whatever you used to "fill" the bars
  ggtitle("Dickcissels")  #can use this to set a main title above the plot
DICK.plot


#>>Bobolink Graph----
BOBO_newdata = data.frame(TSH=c("a","a","a","a","a","a","a","a","a","b","b","b","b","b","b","b","b","b","c","c","c","c","c","c","c","c","c","d","d","d","d","d","d","d","d","d"),
                          GrazingTreat = c("None","None","None","SLS","SLS","SLS","IES","IES","IES"),
                          HerbTreat = c("Con", "SnS", "Spr"),
                          Area_ha = 1,
                          Crop_Prop = 0.2,
                          Tree_Prop = 0.19)


BOBO_Predicted=PredictedValues(BOBO_Top,BOBO_newdata)
BOBO_Predicted_Sum=BOBO_Predicted %>% 
  group_by(TSH, HerbTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on herbicide treatments only
#View(BOBO_Predicted_Sum) 

BOBO_Predicted_Sum$HerbTreat=factor(BOBO_Predicted_Sum$HerbTreat,levels=c("Con","Spr","SnS")) #ordering

BOBO.plot <- ggplot(data = BOBO_Predicted_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=TSH, 
               fill=HerbTreat), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,8), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  
  scale_x_discrete(breaks=c("a","b","c","d"),labels=c("1","2","3","4"))+
  geom_errorbar(aes(x = TSH, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbTreat), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Time Since Herbicide (y)", 
       fill = "Herbicide Treatment") + #sets label for whatever you used to "fill" the bars
  ggtitle("Bobolinks")  #can use this to set a main title above the plot
BOBO.plot


#>>Meadowlark Graph----
EAME_newdata = data.frame(TSH=c("a","a","a","a","a","a","a","a","a","b","b","b","b","b","b","b","b","b","c","c","c","c","c","c","c","c","c","d","d","d","d","d","d","d","d","d"),
                          GrazingTreat = c("None","None","None","SLS","SLS","SLS","IES","IES","IES"),
                          HerbTreat = c("Con", "SnS", "Spr"),
                          Area_ha = 1,
                          Crop_Prop = 0.2,
                          Tree_Prop = 0.19)


EAME_Predicted=PredictedValues(EAME_KindaTop,EAME_newdata)
EAME_Predicted_Sum=EAME_Predicted %>% 
  group_by(TSH, HerbTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on herbicide treatments only
#View(EAME_Predicted_Sum) 

EAME_Predicted_Sum$HerbTreat=factor(EAME_Predicted_Sum$HerbTreat,levels=c("Con","Spr","SnS")) #ordering

EAME.plot <- ggplot(data = EAME_Predicted_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=TSH, 
               fill=HerbTreat), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,17), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  
  scale_x_discrete(breaks=c("a","b","c","d"),labels=c("1","2","3","4"))+
  geom_errorbar(aes(x = TSH, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbTreat), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Time Since Herbicide (y)", 
       fill = "Herbicide Treatment") + #sets label for whatever you used to "fill" the bars
  ggtitle("Meadowlarks")  #can use this to set a main title above the plot
EAME.plot

#>>Red-winged Blackbird Graph----
RWBL_newdata = data.frame(TSH=c("a","a","a","b","b","b","c","c","c","d","d","d"),
                          HerbTreat = c("Con", "SnS", "Spr"),
                          Area_ha = 1,
                          Herb_Prop = 0.59)

RWBL_Predicted=PredictedValues(RWBL_Top,RWBL_newdata)
RWBL_Predicted$HerbTreat=factor(RWBL_Predicted$HerbTreat,levels=c("Con","Spr","SnS")) #ordering
RWBL.plot <- ggplot(data = RWBL_Predicted)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=TSH, 
               fill=HerbTreat), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,8), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  
  scale_x_discrete(breaks=c("a","b","c","d"),labels=c("1","2","3","4"))+
  geom_errorbar(aes(x = TSH, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbTreat), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Time Since Herbicide (y)", 
       fill = "Herbicide Treatment") + #sets label for whatever you used to "fill" the bars
  ggtitle("Red-winged Blackbirds")  #can use this to set a main title above the plot
RWBL.plot

#>>Henslow's Sparrow Graph----
HESP_newdata = data.frame(TSH=c("a","a","a","a","a","a","a","a","a","b","b","b","b","b","b","b","b","b","c","c","c","c","c","c","c","c","c","d","d","d","d","d","d","d","d","d"),
                          HerbTreat = c("Con", "SnS", "Spr"),
                          GrazingTreat = c("None","None","None","SLS","SLS","SLS","IES","IES","IES"),
                          Area_ha = 1,
                          Herb_Prop=0.59)


HESP_Predicted=PredictedValues(HESP_Top,HESP_newdata)
HESP_Predicted_Sum=HESP_Predicted %>% 
  group_by(TSH, HerbTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on herbicide treatments only
#View(HESP_Predicted_Sum) 

HESP_Predicted_Sum$HerbTreat=factor(HESP_Predicted_Sum$HerbTreat,levels=c("Con","Spr","SnS")) #ordering

HESP.plot <- ggplot(data = HESP_Predicted_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=TSH, 
               fill=HerbTreat), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  
  scale_x_discrete(breaks=c("a","b","c","d"),labels=c("1","2","3","4"))+
  geom_errorbar(aes(x = TSH, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbTreat), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Occupancy Probability", 
       x = "Time Since Herbicide (y)", 
       fill = "Herbicide Treatment") + #sets label for whatever you used to "fill" the bars
  ggtitle("Henslow's Sparrows")  #can use this to set a main title above the plot
HESP.plot


#>>Sedge Wren Graph----
SEWR_newdata = data.frame(TSH=c("a","a","a","a","a","a","a","a","a","b","b","b","b","b","b","b","b","b","c","c","c","c","c","c","c","c","c","d","d","d","d","d","d","d","d","d"),
                          GrazingTreat = c("None","None","None","SLS","SLS","SLS","IES","IES","IES"),
                          HerbTreat = c("Con", "SnS", "Spr"),
                          Area_ha = 1,
                          Tree_kmha = 0.72)


SEWR_Predicted=PredictedValues(SEWR_Top,SEWR_newdata)
SEWR_Predicted_Sum=SEWR_Predicted %>% 
  group_by(HerbTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on herbicide treatments only
#View(SEWR_Predicted_Sum) 

SEWR_Predicted_Sum$HerbTreat=factor(SEWR_Predicted_Sum$HerbTreat,levels=c("Con","Spr","SnS")) #ordering

SEWR.plot <- ggplot(data = SEWR_Predicted_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=HerbTreat,
               fill=HerbTreat), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  
  #scale_x_discrete(breaks=c("a","b","c","d"),labels=c("1","2","3","4"))+
  geom_errorbar(aes(x = HerbTreat, 
                    ymin = lower, 
                    ymax = upper), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Occupancy Probability", 
       x = "Herbicide Treatment", 
       fill = "Herbicide Treatment") + #sets label for whatever you used to "fill" the bars
  ggtitle("Sedge Wren")  #can use this to set a main title above the plot
SEWR.plot

#final Herb plot----
#final.plot=plot_grid(DICK.plot,BOBO.plot,EAME.plot,RWBL.plot,HESP.plot,SEWR.plot,nrow=3,ncol=2)

legend_a <- get_legend(
  DICK.plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

all.Herb.plot <- plot_grid(
  DICK.plot + theme(legend.position="none"),
  EAME.plot + theme(legend.position="none"),
  BOBO.plot + theme(legend.position="none"),
  RWBL.plot + theme(legend.position="none"),
  HESP.plot + theme(legend.position="none"),
  SEWR.plot + theme(legend.position="none"),
  align = 'vh',
  #labels = c("A", "B", "C"),
  hjust = -1,
  nrow = 3
)
final.Herb.plot<-plot_grid(
  all.Herb.plot,
  legend_a, 
  ncol = 1, 
  rel_heights = c(1, .1))
final.Herb.plot

#***************************************************************************####
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||####
#***************************************************************************####
#2 [GRAZE] CLEANING + FILTERING----
#***************************************************************************

Data_Cleaning_Gr = function (df) {
  
  #setting NAs from visits to 0
  df$Visit_1[is.na(df$Visit_1)] <- 0
  df$Visit_2[is.na(df$Visit_2)] <- 0
  df$Visit_3[is.na(df$Visit_3)] <- 0
  df$Visit_4[is.na(df$Visit_4)] <- 0
  df$Visit_5[is.na(df$Visit_5)] <- 0
  
  #replacing missing covariates to the mean
  df$Clouds_1[is.na(df$Clouds_1)] <- mean(df$Clouds_1,na.rm=T)
  df$Clouds_2[is.na(df$Clouds_2)] <- mean(df$Clouds_2,na.rm=T)
  df$Clouds_3[is.na(df$Clouds_3)] <- mean(df$Clouds_3,na.rm=T)
  df$Clouds_4[is.na(df$Clouds_4)] <- mean(df$Clouds_4,na.rm=T)
  df$Clouds_5[is.na(df$Clouds_5)] <- mean(df$Clouds_5,na.rm=T)
  
  df$Date_1_Ord[is.na(df$Date_1_Ord)] <- mean(df$Date_1_Ord,na.rm=T)
  df$Date_2_Ord[is.na(df$Date_2_Ord)] <- mean(df$Date_2_Ord,na.rm=T)
  df$Date_3_Ord[is.na(df$Date_3_Ord)] <- mean(df$Date_3_Ord,na.rm=T)
  df$Date_4_Ord[is.na(df$Date_4_Ord)] <- mean(df$Date_4_Ord,na.rm=T)
  df$Date_5_Ord[is.na(df$Date_5_Ord)] <- mean(df$Date_5_Ord,na.rm=T)
  
  df$StartTime_1[is.na(df$StartTime_1)] <- mean(df$StartTime_1,na.rm=T)
  df$StartTime_2[is.na(df$StartTime_2)] <- mean(df$StartTime_2,na.rm=T)
  df$StartTime_3[is.na(df$StartTime_3)] <- mean(df$StartTime_3,na.rm=T)
  df$StartTime_4[is.na(df$StartTime_4)] <- mean(df$StartTime_4,na.rm=T)
  df$StartTime_5[is.na(df$StartTime_5)] <- mean(df$StartTime_5,na.rm=T)
  
  df$Winds_1[is.na(df$Winds_1)] <- mean(df$Winds_1,na.rm=T)
  df$Winds_2[is.na(df$Winds_2)] <- mean(df$Winds_2,na.rm=T)
  df$Winds_3[is.na(df$Winds_3)] <- mean(df$Winds_3,na.rm=T)
  df$Winds_4[is.na(df$Winds_4)] <- mean(df$Winds_4,na.rm=T)
  df$Winds_5[is.na(df$Winds_5)] <- mean(df$Winds_5,na.rm=T)
  
  #rounding and and rescaling large numbers by dividing by 100
  df$Clouds_1 <- round((df$Clouds_1/100),2)
  df$Clouds_2 <- round((df$Clouds_2/100),2)
  df$Clouds_3 <- round((df$Clouds_3/100),2)
  df$Clouds_4 <- round((df$Clouds_4/100),2)
  df$Clouds_5 <- round((df$Clouds_5/100),2)
  
  df$Date_1_Ord <- round(df$Date_1_Ord/100,2)
  df$Date_2_Ord <- round(df$Date_2_Ord/100,2)
  df$Date_3_Ord <- round(df$Date_3_Ord/100,2)
  df$Date_4_Ord <- round(df$Date_4_Ord/100,2)
  df$Date_5_Ord <- round(df$Date_5_Ord/100,2)
  
  df$FirstOfSumOf2010_Length <- round(df$FirstOfSumOf2010_Length/100,0) #fixing new transect lengths
  df$FirstOfSumOf2010_Length[df$Pasture_patch_year=="235_S_2015"] = 4
  df$FirstOfSumOf2010_Length[df$Pasture_patch_year=="KLN_E_2015"] = 4
  df$FirstOfSumOf2010_Length[df$Pasture_patch_year=="KLN_E_2016"] = 4
  df$FirstOfSumOf2010_Length[df$Pasture_patch_year=="KLN_E_2017"] = 4
  df$FirstOfSumOf2010_Length[df$Pasture_patch_year=="KLN_E_2018"] = 4
  
  df$StartTime_1 <- round(df$StartTime_1,4)
  df$StartTime_2 <- round(df$StartTime_2,4)
  df$StartTime_3 <- round(df$StartTime_3,4)
  df$StartTime_4 <- round(df$StartTime_4,4)
  df$StartTime_5 <- round(df$StartTime_5,4)
  
  df$Winds_1 <- round(df$Winds_1,0)
  df$Winds_2 <- round(df$Winds_2,0)
  df$Winds_3 <- round(df$Winds_3,0)
  df$Winds_4 <- round(df$Winds_4,0)
  df$Winds_5 <- round(df$Winds_5,0)
  
  df$TSH[df$TSH==0.5]='a'
  df$TSH[df$TSH==1.5]='b'
  df$TSH[df$TSH==2.5]='c'
  df$TSH[df$TSH==3.5]='d'
  df$TSF[df$TSF=='UNK']=NA
  
  df$Year[df$Year==2015]='a'
  df$Year[df$Year==2016]='b'
  df$Year[df$Year==2017]='c'
  df$Year[df$Year==2018]='d'
  
  
  df$GrazingTreat[df$GrazingTreat=='Hay']='None' #Haying is technically no grazing, so this code combines those two categories in a new column
  df$GrazingTreat[df$GrazingTreat=='IES']='IES'
  df$GrazingTreat[df$GrazingTreat=='SLS']='SLS'
  df$GrazingTreat[df$GrazingTreat=='None']='None'
  df$GrazingTreat=factor(df$GrazingTreat)
 
  df

} #data cleaning and filtering function
addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}

DICK_Graze= Data_Cleaning_Gr(DICK_all) #>>Dickcissel----
  DICK_Graze$HerbTreat_14.18=factor(DICK_Graze$HerbTreat_14.18)
  DICK_Graze$TSF <- addLevel(DICK_Graze$TSF, "6")
  DICK_Graze$TSF[is.na(DICK_Graze$TSF)] <- "6"
  DICK_Graze$TSF=as.numeric(DICK_Graze$TSF)
#View(DICK_Graze)

BOBO_Graze= Data_Cleaning_Gr(BOBO_all) #>>Bobolink----
BOBO_Graze$HerbTreat_14.18=factor(BOBO_Graze$HerbTreat_14.18)
BOBO_Graze$TSF <- addLevel(BOBO_Graze$TSF, "6")
BOBO_Graze$TSF[is.na(BOBO_Graze$TSF)] <- "6"
BOBO_Graze$TSF=as.numeric(BOBO_Graze$TSF)

GRSP_Graze= Data_Cleaning_Gr(GRSP_all) #>>Grasshopper Sparrow----
GRSP_Graze$HerbTreat_14.18=factor(GRSP_Graze$HerbTreat_14.18)
GRSP_Graze$TSF <- addLevel(GRSP_Graze$TSF, "6")
GRSP_Graze$TSF[is.na(GRSP_Graze$TSF)] <- "6"
GRSP_Graze$TSF=as.numeric(GRSP_Graze$TSF)

EAME_Graze= Data_Cleaning_Gr(EAME_all) #>>Meadowlarks----
EAME_Graze$HerbTreat_14.18=factor(EAME_Graze$HerbTreat_14.18)
EAME_Graze$TSF <- addLevel(EAME_Graze$TSF, "6")
EAME_Graze$TSF[is.na(EAME_Graze$TSF)] <- "6"
EAME_Graze$TSF=as.numeric(EAME_Graze$TSF)


RWBL_Graze= Data_Cleaning_Gr(RWBL_all) #>>Red-winged Blackbird----
RWBL_Graze$HerbTreat_14.18=factor(RWBL_Graze$HerbTreat_14.18)
RWBL_Graze$TSF <- addLevel(RWBL_Graze$TSF, "6")
RWBL_Graze$TSF[is.na(RWBL_Graze$TSF)] <- "6"
RWBL_Graze$TSF=as.numeric(RWBL_Graze$TSF)

HESP_Graze= Data_Cleaning_Gr(HESP_all) #>>Henslow's Sparrow----
HESP_Graze$HerbTreat_14.18=factor(HESP_Graze$HerbTreat_14.18) 
HESP_Graze$TSF <- addLevel(HESP_Graze$TSF, "6")
HESP_Graze$TSF[is.na(HESP_Graze$TSF)] <- "6"
HESP_Graze$TSF=as.numeric(HESP_Graze$TSF)

SEWR_Graze= Data_Cleaning_Gr(SEWR_all) #>>Sedge Wren----
SEWR_Graze$HerbTreat_14.18=factor(SEWR_Graze$HerbTreat_14.18)
SEWR_Graze$TSF <- addLevel(SEWR_Graze$TSF, "6")
SEWR_Graze$TSF[is.na(SEWR_Graze$TSF)] <- "6"
SEWR_Graze$TSF=as.numeric(SEWR_Graze$TSF)


#How many detections?----
DICK_Det=sum(DICK_Graze$Visit_1)+sum(DICK_Graze$Visit_2)+sum(DICK_Graze$Visit_3)+sum(DICK_Graze$Visit_4)
BOBO_Det=sum(BOBO_Graze$Visit_1)+sum(BOBO_Graze$Visit_2)+sum(BOBO_Graze$Visit_3)+sum(BOBO_Graze$Visit_4)
GRSP_Det=sum(GRSP_Graze$Visit_1)+sum(GRSP_Graze$Visit_2)+sum(GRSP_Graze$Visit_3)+sum(GRSP_Graze$Visit_4)
EAME_Det=sum(EAME_Graze$Visit_1)+sum(EAME_Graze$Visit_2)+sum(EAME_Graze$Visit_3)+sum(EAME_Graze$Visit_4)
RWBL_Det=sum(RWBL_Graze$Visit_1)+sum(RWBL_Graze$Visit_2)+sum(RWBL_Graze$Visit_3)+sum(RWBL_Graze$Visit_4)
HESP_Det=sum(HESP_Graze$Visit_1)+sum(HESP_Graze$Visit_2)+sum(HESP_Graze$Visit_3)+sum(HESP_Graze$Visit_4)
Det_List=c(DICK_Det,BOBO_Det,GRSP_Det,EAME_Det, RWBL_Det, HESP_Det)
Det_List

#***************************************************************************####
#3 [Graze] UNMARKED SETUP----
#***************************************************************************

unmarked_data_Gr = function(df) {
  abundance_by_visit=df[,4:8] #sets the columns that will be used for abundance
  siteCovs=data.frame(list(Past_Pat_Year=df[,1], #site covariates
                           Year=df[,2], 
                           Pasture=df[,3], 
                           FireTreat=df[,34], 
                           TSF=df[,35],
                           TSH=df[,36],
                           HerbTreat=df[,37],
                           GrazingYesNo=df[,38],
                           HerbYesNo=df[,39],
                           GrazingTreat=df[,40],
                           Robel=df[,41],
                           Area_ha=df[,42], 
                           Tree_kmha=df[,44],
                           Herb_Prop=df[,45],
                           Crop_Prop=df[,46],
                           Tree_Prop=df[,47]
  ))
  obsCovs=list(Wind=df[,24:28],  #sets the columns that will be used for observation covariates (should match the number of columns for abundance)
               DOY=df[,14:18],
               Clouds=df[,9:13],
               StartTime=df[,19:23],
               Obs=df[,29:33])
  unmarkedFramePCount(abundance_by_visit,siteCovs,obsCovs)
} #setting up unmarked 'pcount'

DICK_PCount_Gr=unmarked_data_Gr(DICK_Graze) #>>Dickcissels----
BOBO_PCount_Gr=unmarked_data_Gr(BOBO_Graze) #>>Bobolink----
GRSP_PCount_Gr=unmarked_data_Gr(GRSP_Graze) #>>Grasshopper Sparrow----
EAME_PCount_Gr=unmarked_data_Gr(EAME_Graze) #>>Meadowlarks----
RWBL_PCount_Gr=unmarked_data_Gr(RWBL_Graze) #>>Red-winged Blackbirds----
HESP_PCount_Gr=unmarked_data_Gr(HESP_Graze) #>>Henslow's Sparrow----
SEWR_PCount_Gr=unmarked_data_Gr(SEWR_Graze) #>>Sedge Wren----
#***************************************************************************####
#4 [Graze] DISTRIBUTIONS----
#***************************************************************************#
#use same distribution models as earlier
Distribution_mods = function (df) {
  null_ZIP      =pcount(~1 ~1, data=df, mixture="ZIP",K=100,)
  null_poisson  =pcount(~1 ~1, data=df, mixture="P",K=100,)
  
  mods=list(null_ZIP,null_poisson)
  names=c("null_ZIP","null_poisson")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} #picking between ZIP and P

Distribution_mods(DICK_PCount_Gr) #>>Dickcissel----
      # #            K      AIC Delta_AIC AICWt Cum.Wt        LL
      # null_ZIP     3 3629.716    0.0000     1      1 -1811.858
      # null_poisson 2 3756.207  126.4909     0      1 -1876.103
#Chosen: ZIP
Distribution_mods(BOBO_PCount_Gr)#>>Bobolink----
    # #            K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 3376.976    0.0000     1      1 -1685.488
    # null_poisson 2 3731.559  354.5831     0      1 -1863.780
#Chosen:ZIP
Distribution_mods(GRSP_PCount_Gr)#>>Grasshopper Sparrow----
    #              K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 2492.221     0.000     1      1 -1243.111
    # null_poisson 2 2616.230   124.009     0      1 -1306.115

Distribution_mods(EAME_PCount_Gr)#>>Meadowlarks----
    #              K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 2478.108     0.000     1      1 -1236.054
    # null_poisson 2 2533.921    55.813     0      1 -1264.960
Distribution_mods(RWBL_PCount_Gr)#>>Red-winged Blackbird----
    #              K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 3206.111    0.0000     1      1 -1600.055
    # null_poisson 2 3511.845  305.7343     0      1 -1753.922

Distribution_mods(HESP_PCount_Gr)#>>Henslow's Sparrow----
    #              K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 1479.279    0.0000     1      1 -736.6394
    # null_poisson 2 1610.445  131.1658     0      1 -803.2222


Distribution_mods(SEWR_PCount_Gr)#>>Henslow's Sparrow----
    #             K      AIC Delta_AIC AICWt Cum.Wt        LL
    # null_ZIP     3 1351.035    0.0000     1      1 -672.5175
    # null_poisson 2 1509.789  158.7538     0      1 -752.8943



#***************************************************************************####
#5 [Graze] DETECTION----
#***************************************************************************
#use same detection models as earlier
Detection_mods=function(df) {
  # For detection variables, all single-variable models are included, plus each of the following groups in all subsets:
  #Weather: Wind,Clouds
  # Timing: StartTime, DOY
  # Observer: Obs
  # Visibility: Robel
  Null                      =pcount(~Robel ~1, data=df, mixture="ZIP",K=100,)
  Obs                       =pcount(~Obs+Robel ~1, data=df, mixture="ZIP",K=100)
  StartTime                 =pcount(~StartTime+Robel ~1, data=df, mixture="ZIP",K=100)
  DOY                       =pcount(~DOY+Robel ~1, data=df, mixture="ZIP",K=100)
  Winds                     =pcount(~Wind+Robel ~1, data=df, mixture="ZIP",K=100)
  Clouds                    =pcount(~Clouds+Robel ~1, data=df, mixture="ZIP",K=100)
  Winds_Clouds              =pcount(~Wind+Clouds+Robel ~1, data=df, mixture="ZIP",K=100)
  StartTime_DOY             =pcount(~StartTime+DOY+Robel ~1, data=df, mixture="ZIP",K=100)
  Obs_StartTime_DOY         =pcount(~Obs+DOY+StartTime+Robel ~1, data=df, mixture="ZIP",K=100)
  Obs_Winds_Clouds          =pcount(~Obs+Wind+Clouds+Robel ~1, data=df, mixture="ZIP",K=100)
  Global                    =pcount(~Obs+DOY+StartTime+Robel+Wind+Clouds ~1, data=df, mixture="ZIP",K=100)
  
  mods=list(Null, Obs,   StartTime,  DOY,   Winds,   Clouds,   Winds_Clouds,   StartTime_DOY,   Obs_StartTime_DOY,   Obs_Winds_Clouds,   Global)
  names=c("Null","Obs", "StartTime","DOY", "Winds", "Clouds", "Winds_Clouds", "StartTime_DOY", "Obs_StartTime_DOY", "Obs_Winds_Clouds", "Global")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}
Detection_mods(DICK_PCount_Gr) #>>Dickcissel: Obs_Winds----
      #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Obs_Winds_Clouds  14 3550.818    0.0000 0.3874 0.3874 -1761.409
      # Obs               12 3551.804    0.9856 0.2367 0.6241 -1763.902
      # Global            16 3552.123    1.3049 0.2017 0.8258 -1760.062
      # Obs_StartTime_DOY 14 3552.418    1.5991 0.1742 1.0000 -1762.209
      # Winds              5 3620.569   69.7509 0.0000 1.0000 -1805.285
      # Winds_Clouds       6 3621.110   70.2917 0.0000 1.0000 -1804.555
      # DOY                5 3621.811   70.9930 0.0000 1.0000 -1805.906
      # StartTime_DOY      6 3623.675   72.8571 0.0000 1.0000 -1805.838
      # Null               4 3628.191   77.3723 0.0000 1.0000 -1810.095
      # Clouds             5 3629.463   78.6445 0.0000 1.0000 -1809.732
      # StartTime          5 3630.173   79.3543 0.0000 1.0000 -1810.086
  DICK_det_mod_Gr=pcount(~Obs+Wind+Clouds+Robel ~1, data=DICK_PCount_Gr, mixture="ZIP",K=100)
  confint(DICK_det_mod_Gr,type="det",level=0.85)
  #Robel, Clouds uninformative
  
Detection_mods(BOBO_PCount_Gr) #>>Bobolink: Obs+Wind+Clouds+DOY---- 
      #                    K      AIC Delta_AIC AICWt Cum.Wt        LL
      # Global            16 3304.880    0.0000 0.992  0.992 -1636.440
      # Obs_StartTime_DOY 14 3314.519    9.6392 0.008  1.000 -1643.260
      # DOY                5 3334.324   29.4437 0.000  1.000 -1662.162
      # StartTime_DOY      6 3336.034   31.1536 0.000  1.000 -1662.017
      # Obs_Winds_Clouds  14 3350.002   45.1213 0.000  1.000 -1661.001
      # Obs               12 3356.604   51.7236 0.000  1.000 -1666.302
      # Winds              5 3374.643   69.7625 0.000  1.000 -1682.321
      # Winds_Clouds       6 3375.352   70.4715 0.000  1.000 -1681.676
      # Clouds             5 3377.862   72.9818 0.000  1.000 -1683.931
      # Null               4 3378.026   73.1452 0.000  1.000 -1685.013
      # StartTime          5 3378.288   73.4072 0.000  1.000 -1684.144
  BOBO_det_mod_Gr=pcount(~Obs+Wind+Clouds+Robel+DOY+StartTime ~1, data=BOBO_PCount_Gr, mixture="ZIP",K=100)
  confint(BOBO_det_mod_Gr,type="det",level=0.85) 
      #StartTime,Robel pretending

Detection_mods(GRSP_PCount_Gr) #>>Grasshopper Sparrow: Obs+Winds+Clouds---- 
      #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Obs_Winds_Clouds  14 2257.124    0.0000 0.4293 0.4293 -1114.562
      # Global            16 2257.360    0.2360 0.3815 0.8108 -1112.680
      # Obs               12 2259.725    2.6012 0.1169 0.9278 -1117.863
      # Obs_StartTime_DOY 14 2260.688    3.5642 0.0722 1.0000 -1116.344
      # Winds              5 2338.057   80.9331 0.0000 1.0000 -1164.029
      # Winds_Clouds       6 2339.150   82.0259 0.0000 1.0000 -1163.575
      # DOY                5 2349.603   92.4789 0.0000 1.0000 -1169.802
      # Null               4 2350.408   93.2837 0.0000 1.0000 -1171.204
      # Clouds             5 2351.287   94.1630 0.0000 1.0000 -1170.643
      # StartTime_DOY      6 2351.593   94.4689 0.0000 1.0000 -1169.796
      # StartTime          5 2352.401   95.2765 0.0000 1.0000 -1171.200
  GRSP_det_mod_Gr=pcount(~Obs+Wind+Clouds+Robel ~1, data=GRSP_PCount_Gr, mixture="ZIP",K=100)
  confint(GRSP_det_mod_Gr,type="det",level=0.85) 
      #no pretending
Detection_mods(EAME_PCount_Gr) #>>Meadowlarks: StartTime+Obs+Wind+Clouds+Robel---- 
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Global            16 2370.120    0.0000 0.4401 0.4401 -1169.060
      # Obs_Winds_Clouds  14 2370.786    0.6664 0.3154 0.7555 -1171.393
      # Obs               12 2372.653    2.5331 0.1240 0.8795 -1174.326
      # Obs_StartTime_DOY 14 2372.739    2.6191 0.1188 0.9983 -1172.369
      # Null               4 2384.027   13.9078 0.0004 0.9988 -1188.014
      # StartTime          5 2384.459   14.3394 0.0003 0.9991 -1187.229
      # Clouds             5 2384.547   14.4276 0.0003 0.9994 -1187.274
      # Winds              5 2385.942   15.8225 0.0002 0.9996 -1187.971
      # DOY                5 2385.966   15.8465 0.0002 0.9997 -1187.983
      # StartTime_DOY      6 2386.366   16.2463 0.0001 0.9999 -1187.183
      # Winds_Clouds       6 2386.411   16.2910 0.0001 1.0000 -1187.205
  EAME_det_mod_Gr=pcount(~Obs+Wind+Clouds+Robel+DOY+StartTime ~1, data=EAME_PCount_Gr, mixture="ZIP",K=100)
  confint(EAME_det_mod_Gr,type="det",level=0.85) 
      #pretending: DOY
  
Detection_mods(RWBL_PCount_Gr) #>>Red-winged Blackbird: Obs_StarTime_DOY---- 
      #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Obs_StartTime_DOY 14 2975.346    0.0000 0.8286 0.8286 -1473.673
      # Global            16 2978.497    3.1509 0.1714 1.0000 -1473.248
      # StartTime_DOY      6 3035.952   60.6058 0.0000 1.0000 -1511.976
      # DOY                5 3037.030   61.6838 0.0000 1.0000 -1513.515
      # Obs               12 3041.548   66.2019 0.0000 1.0000 -1508.774
      # Obs_Winds_Clouds  14 3045.083   69.7365 0.0000 1.0000 -1508.541
      # StartTime          5 3091.832  116.4859 0.0000 1.0000 -1540.916
      # Winds              5 3094.111  118.7653 0.0000 1.0000 -1542.056
      # Null               4 3095.331  119.9846 0.0000 1.0000 -1543.665
      # Winds_Clouds       6 3095.483  120.1365 0.0000 1.0000 -1541.741
      # Clouds             5 3097.091  121.7451 0.0000 1.0000 -1543.546
    RWBL_det_mod_Gr=pcount(~Obs+Robel+StartTime+DOY ~1, data=RWBL_PCount_Gr, mixture="ZIP",K=100)
    confint(RWBL_det_mod_Gr,type="det",level=0.85) 
      #pretending: none

Detection_mods(HESP_PCount_Gr) #>>Henslow's Sparrow: Obs+StartTime+Clouds---- 
      #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Global            16 1447.965    0.0000 0.5776 0.5776 -707.9827
      # Obs_StartTime_DOY 14 1449.985    2.0196 0.2104 0.7881 -710.9925
      # Obs_Winds_Clouds  14 1450.203    2.2372 0.1887 0.9768 -711.1013
      # Obs               12 1454.395    6.4300 0.0232 1.0000 -715.1977
      # Winds_Clouds       6 1473.929   25.9633 0.0000 1.0000 -730.9644
      # StartTime          5 1475.358   27.3926 0.0000 1.0000 -732.6790
      # Winds              5 1475.776   27.8103 0.0000 1.0000 -732.8879
      # Clouds             5 1476.162   28.1966 0.0000 1.0000 -733.0810
      # StartTime_DOY      6 1477.087   29.1220 0.0000 1.0000 -732.5437
      # Null               4 1480.501   32.5358 0.0000 1.0000 -736.2506
      # DOY                5 1482.426   34.4609 0.0000 1.0000 -736.2132             
    HESP_det_mod_Gr=pcount(~Obs+Robel+StartTime+DOY+Clouds+Wind ~1, data=HESP_PCount_Gr, mixture="ZIP",K=100)
    confint(HESP_det_mod_Gr,type="det",level=0.85) 
      #pretending: Wind, DOY,Robel
Detection_mods(SEWR_PCount_Gr) #>>Sedge wren: Obs+StartTime+Clouds+Robel+DOY---- 
    #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # Global            16 1144.911    0.0000 0.6779 0.6779 -556.4553
    # Obs_StartTime_DOY 14 1146.399    1.4881 0.3221 1.0000 -559.1993
    # StartTime_DOY      6 1196.229   51.3184 0.0000 1.0000 -592.1145
    # DOY                5 1234.739   89.8284 0.0000 1.0000 -612.3695
    # Obs_Winds_Clouds  14 1268.880  123.9693 0.0000 1.0000 -620.4399
    # Obs               12 1274.760  129.8497 0.0000 1.0000 -625.3801
    # Winds_Clouds       6 1299.387  154.4764 0.0000 1.0000 -643.6935
    # Winds              5 1301.883  156.9727 0.0000 1.0000 -645.9417
    # StartTime          5 1302.087  157.1764 0.0000 1.0000 -646.0435
    # Clouds             5 1317.633  172.7221 0.0000 1.0000 -653.8163
    # Null               4 1326.044  181.1336 0.0000 1.0000 -659.0221

  SEWR_det_mod_Gr=pcount(~Obs+Robel+StartTime+DOY+Clouds+Wind ~1, data=SEWR_PCount_Gr, mixture="ZIP",K=100)
  confint(SEWR_det_mod_Gr,type="det",level=0.85) 
    #pretending: Wind
#**************************************************************************#####
#6 [Graze] LANDSCAPE----
#***************************************************************************

DICK_Landscape_mods_Gr = function(df) {
  Null                =pcount(~Obs+Wind  ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp            =pcount(~Obs+Wind  ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp   =pcount(~Obs+Wind  ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp            =pcount(~Obs+Wind  ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp   =pcount(~Obs+Wind  ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp            =pcount(~Obs+Wind  ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)

  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   CropProp,    CropProp_TreeProp,  TreeProp)
  names=c( "Null", "HerbProp", "HerbProp_TreeProp",  "CropProp", "CropProp_TreeProp","TreeProp")
     print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 
BOBO_Landscape_mods_Gr = function(df) {
  Null                 =pcount(~Obs+DOY+Wind+Clouds ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp             =pcount(~Obs+DOY+Wind+Clouds ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp    =pcount(~Obs+DOY+Wind+Clouds ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp             =pcount(~Obs+DOY+Wind+Clouds ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp    =pcount(~Obs+DOY+Wind+Clouds ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp             =pcount(~Obs+DOY+Wind+Clouds ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)

  mods=list(Null,   HerbProp,   HerbProp_TreeProp,    CropProp,   CropProp_TreeProp,    TreeProp )
  names=c( "Null", "HerbProp", "HerbProp_TreeProp",  "CropProp", "CropProp_TreeProp",  "TreeProp")  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 
GRSP_Landscape_mods_Gr = function(df) {
  Null                =pcount(~Obs+Wind+Clouds   ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp            =pcount(~Obs+Wind+Clouds   ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp   =pcount(~Obs+Wind+Clouds   ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp            =pcount(~Obs+Wind+Clouds   ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp   =pcount(~Obs+Wind+Clouds   ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp            =pcount(~Obs+Wind+Clouds   ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)

  mods=list(Null,   HerbProp,   HerbProp_TreeProp,    CropProp,   CropProp_TreeProp,    TreeProp )
  names=c( "Null", "HerbProp", "HerbProp_TreeProp",  "CropProp", "CropProp_TreeProp",  "TreeProp" )
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 
EAME_Landscape_mods_Gr = function(df) {
  Null                =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp            =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp   =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp            =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp   =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp            =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)

  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   CropProp,   CropProp_TreeProp,   TreeProp  )
  names=c( "Null", "HerbProp", "HerbProp_TreeProp",  "CropProp", "CropProp_TreeProp", "TreeProp")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 
RWBL_Landscape_mods_Gr = function(df) {
  Null                =pcount(~Obs+StartTime+DOY    ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp            =pcount(~Obs+StartTime+DOY    ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp   =pcount(~Obs+StartTime+DOY    ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp            =pcount(~Obs+StartTime+DOY    ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp   =pcount(~Obs+StartTime+DOY    ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp            =pcount(~Obs+StartTime+DOY    ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)

  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   CropProp,    CropProp_TreeProp,    TreeProp)  
  names=c( "Null", "HerbProp", "HerbProp_TreeProp",  "CropProp", "CropProp_TreeProp",  "TreeProp")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 
HESP_Landscape_mods_Gr = function(df) {
  Null                =pcount(~Obs+StartTime+Clouds   ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp            =pcount(~Obs+StartTime+Clouds   ~offset(log(Area_ha))+Herb_Prop, data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp   =pcount(~Obs+StartTime+Clouds   ~offset(log(Area_ha))+Herb_Prop+Tree_Prop, data=df, mixture="ZIP",K=100)
  CropProp            =pcount(~Obs+StartTime+Clouds   ~offset(log(Area_ha))+Crop_Prop, data=df, mixture="ZIP",K=100)
  CropProp_TreeProp   =pcount(~Obs+StartTime+Clouds   ~offset(log(Area_ha))+Crop_Prop+Tree_Prop, data=df, mixture="ZIP",K=100)
  TreeProp            =pcount(~Obs+StartTime+Clouds   ~offset(log(Area_ha))+Tree_Prop, data=df, mixture="ZIP",K=100)

  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   CropProp,   CropProp_TreeProp,    TreeProp)   
  names=c( "Null", "HerbProp", "HerbProp_TreeProp",  "CropProp", "CropProp_TreeProp",  "TreeProp")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 
SEWR_Landscape_mods_Gr = function(df) {
  Null                =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp            =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~offset(log(Area_ha))+Herb_Prop, data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp   =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~offset(log(Area_ha))+Herb_Prop+Tree_Prop, data=df, mixture="ZIP",K=100)
  CropProp            =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~offset(log(Area_ha))+Crop_Prop, data=df, mixture="ZIP",K=100)
  CropProp_TreeProp   =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~offset(log(Area_ha))+Crop_Prop+Tree_Prop, data=df, mixture="ZIP",K=100)
  TreeProp            =pcount(~Obs+DOY+StartTime+Robel+Clouds   ~offset(log(Area_ha))+Tree_Prop, data=df, mixture="ZIP",K=100)

  mods=list(Null,   HerbProp,   HerbProp_TreeProp,    CropProp,   CropProp_TreeProp,    TreeProp )  
  names=c( "Null", "HerbProp", "HerbProp_TreeProp",  "CropProp", "CropProp_TreeProp",  "TreeProp")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 

DICK_Landscape_mods_Gr(DICK_PCount_Gr) #>>Dickcissels: CropProp+TreeProp----
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # CropProp_TreeProp  14 3357.582    0.0000 0.4580 0.4580 -1664.791
      # HerbProp_TreeProp  14 3358.116    0.5341 0.3506 0.8086 -1665.058
      # TreeProp           13 3359.352    1.7701 0.1890 0.9976 -1666.676
      # HerbProp_Tree_kmha 14 3368.924   11.3418 0.0016 0.9992 -1670.462
      # Tree_kmHa          13 3371.121   13.5392 0.0005 0.9997 -1672.561
      # CropProp_TreekmHa  14 3372.433   14.8511 0.0003 1.0000 -1672.216
      # HerbProp           13 3387.356   29.7742 0.0000 1.0000 -1680.678
      # CropProp           13 3417.012   59.4299 0.0000 1.0000 -1695.506
      # Null               12 3418.173   60.5905 0.0000 1.0000 -1697.086

#                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
# CropProp_TreeProp 14 3357.582    0.0000 0.4591 0.4591 -1664.791
# HerbProp_TreeProp 14 3358.116    0.5341 0.3515 0.8105 -1665.058
# TreeProp          13 3359.352    1.7701 0.1895 1.0000 -1666.676
# HerbProp          13 3387.356   29.7742 0.0000 1.0000 -1680.678
# CropProp          13 3417.012   59.4299 0.0000 1.0000 -1695.506
# Null              12 3418.173   60.5905 0.0000 1.0000 -1697.086

  DICK_landscape_mod_Gr = pcount(~Obs+Wind  ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=DICK_PCount_Gr, mixture="ZIP",K=100)
  confint(DICK_landscape_mod_Gr, type="state", level=0.85)

#Carry forward: nothing or CropProp?

BOBO_Landscape_mods_Gr(BOBO_PCount_Gr) #>>Bobolinks: CropProp+TreekmHa ----
    #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # CropProp_TreekmHa  16 3139.222    0.0000 0.9979 0.9979 -1553.611
    # HerbProp_Tree_kmha 16 3151.524   12.3022 0.0021 1.0000 -1559.762
    # Tree_kmHa          15 3165.056   25.8346 0.0000 1.0000 -1567.528
    # HerbProp_TreeProp  16 3187.916   48.6945 0.0000 1.0000 -1577.958
    # CropProp_TreeProp  16 3193.210   53.9888 0.0000 1.0000 -1580.605
    # HerbProp           15 3199.472   60.2506 0.0000 1.0000 -1584.736
    # TreeProp           15 3224.109   84.8876 0.0000 1.0000 -1597.055
    # Null               14 3302.077  162.8553 0.0000 1.0000 -1637.039
    # CropProp           15 3302.617  163.3952 0.0000 1.0000 -1636.308

#                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
# HerbProp_TreeProp 16 3187.916    0.0000 0.9311 0.9311 -1577.958
# CropProp_TreeProp 16 3193.210    5.2944 0.0660 0.9971 -1580.605
# HerbProp          15 3199.472   11.5561 0.0029 1.0000 -1584.736
# TreeProp          15 3224.109   36.1931 0.0000 1.0000 -1597.055
# Null              14 3302.077  114.1609 0.0000 1.0000 -1637.039
# CropProp          15 3302.617  114.7008 0.0000 1.0000 -1636.308


  BOBO_landscape_mod_Gr    =pcount(~Obs+DOY+Wind+Clouds ~Crop_Prop+Tree_kmha+offset(log(Area_ha)), data=BOBO_PCount_Gr, mixture="ZIP",K=100)
  confint(BOBO_landscape_mod_Gr, type="state", level=0.85)
    #all good

  
  
GRSP_Landscape_mods_Gr(GRSP_PCount_Gr) #>>Grasshoopper Sparrow:HerbProp_Tree_kmha----
    #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbProp_Tree_kmha 15 2241.398    0.0000 0.7050 0.7050 -1105.699
    # CropProp_TreeProp  15 2244.687    3.2886 0.1362 0.8412 -1107.343
    # HerbProp_TreeProp  15 2245.337    3.9385 0.0984 0.9396 -1107.668
    # CropProp_TreekmHa  15 2247.701    6.3027 0.0302 0.9697 -1108.850
    # CropProp           14 2247.720    6.3223 0.0299 0.9996 -1109.860
    # HerbProp           14 2256.440   15.0419 0.0004 1.0000 -1114.220
    # Null               13 2266.349   24.9515 0.0000 1.0000 -1120.175
    # Tree_kmHa          14 2267.705   26.3067 0.0000 1.0000 -1119.852
    # TreeProp           14 2268.179   26.7808 0.0000 1.0000 -1120.089

  GRSP_landscape_mod_Gr    =pcount(~Obs+Wind+Clouds ~Herb_Prop+Tree_kmha+offset(log(Area_ha)), data=GRSP_PCount_Gr, mixture="ZIP",K=100)
  confint(GRSP_landscape_mod_Gr, type="state", level=0.85)
    #all good

EAME_Landscape_mods_Gr(EAME_PCount_Gr) #>>Meadowlarks: HerbProp----
    #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbProp           16 2238.816    0.0000 0.3249 0.3249 -1103.408
    # CropProp_TreeProp  17 2239.144    0.3280 0.2758 0.6007 -1102.572
    # HerbProp_Tree_kmha 17 2240.529    1.7135 0.1379 0.7386 -1103.265
    # HerbProp_TreeProp  17 2240.581    1.7650 0.1344 0.8731 -1103.290
    # CropProp_TreekmHa  17 2240.933    2.1173 0.1127 0.9858 -1103.467
    # CropProp           16 2247.357    8.5409 0.0045 0.9903 -1107.678
    # Tree_kmHa          16 2247.517    8.7008 0.0042 0.9945 -1107.758
    # TreeProp           16 2247.695    8.8794 0.0038 0.9984 -1107.848
    # Null               15 2249.388   10.5720 0.0016 1.0000 -1109.694
  EAME_landscape_mod_Gr    =pcount(~Obs+Wind+Clouds+Robel ~Herb_Prop+offset(log(Area_ha)), data=EAME_PCount_Gr, mixture="ZIP",K=100)
  confint(EAME_landscape_mod_Gr, type="state", level=0.85)
    #all good

RWBL_Landscape_mods_Gr(RWBL_PCount_Gr) #>>Red-winged blackbirds: CropProp_TreekmHa ----
    #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # CropProp_TreekmHa  15 3047.530    0.0000 0.6473 0.6473 -1508.765
    # HerbProp_Tree_kmha 15 3049.476    1.9457 0.2447 0.8920 -1509.738
    # HerbProp           14 3053.088    5.5578 0.0402 0.9322 -1512.544
    # HerbProp_TreeProp  15 3053.442    5.9119 0.0337 0.9658 -1511.721
    # Tree_kmHa          14 3054.740    7.2098 0.0176 0.9834 -1513.370
    # CropProp_TreeProp  15 3054.887    7.3565 0.0164 0.9998 -1512.443
    # TreeProp           14 3063.660   16.1295 0.0002 1.0000 -1517.830
    # Null               13 3079.332   31.8017 0.0000 1.0000 -1526.666
    # CropProp           14 3080.343   32.8129 0.0000 1.0000 -1526.171
  RWBL_landscape_mod_Gr    =pcount(~Obs+StartTime+DOY ~Crop_Prop+Tree_kmha+offset(log(Area_ha)), data=RWBL_PCount_Gr, mixture="ZIP",K=100)
  confint(RWBL_landscape_mod_Gr, type="state", level=0.85)
  #all good

HESP_Landscape_mods_Gr(HESP_PCount_Gr) #>>Henslow's Sparrow:HerbProp----
    #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbProp           14 1410.636    0.0000 0.3426 0.3426 -691.3179
    # HerbProp_Tree_kmha 15 1412.008    1.3721 0.1725 0.5151 -691.0039
    # HerbProp_TreeProp  15 1412.057    1.4211 0.1684 0.6835 -691.0284
    # CropProp_TreeProp  15 1412.549    1.9130 0.1316 0.8151 -691.2744
    # CropProp_TreekmHa  15 1413.238    2.6024 0.0933 0.9084 -691.6191
    # Tree_kmHa          14 1414.331    3.6950 0.0540 0.9624 -693.1654
    # TreeProp           14 1415.131    4.4950 0.0362 0.9986 -693.5653
    # Null               13 1422.432   11.7962 0.0009 0.9995 -698.2160
    # CropProp           14 1423.798   13.1619 0.0005 1.0000 -697.8988
  HESP_landscape_mod_Gr    =pcount(~Obs+StartTime+Clouds ~Herb_Prop+offset(log(Area_ha)), data=HESP_PCount_Gr, mixture="ZIP",K=100)
  confint(HESP_landscape_mod_Gr, type="state", level=0.85)
    #all good

SEWR_Landscape_mods_Gr(SEWR_PCount_Gr) #>>Sedge Wren:Crop_Prop+Tree_Prop----
      #                    K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # CropProp_TreeProp  17 1104.553    0.0000 0.4570 0.4570 -535.2767
      # HerbProp_TreeProp  17 1104.692    0.1386 0.4264 0.8835 -535.3460
      # HerbProp_Tree_kmha 17 1108.314    3.7604 0.0697 0.9532 -537.1569
      # TreeProp           16 1110.661    6.1073 0.0216 0.9747 -539.3303
      # CropProp_TreekmHa  17 1111.279    6.7253 0.0158 0.9906 -538.6393
      # Tree_kmHa          16 1112.459    7.9055 0.0088 0.9994 -540.2294
      # HerbProp           16 1117.688   13.1352 0.0006 1.0000 -542.8442
      # Null               15 1135.147   30.5935 0.0000 1.0000 -552.5734
      # CropProp           16 1135.818   31.2642 0.0000 1.0000 -551.9087

  SEWR_landscape_mod_Gr    =pcount(~Obs+StartTime+Clouds+Robel+DOY ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=SEWR_PCount_Gr, mixture="ZIP",K=100)
  confint(SEWR_landscape_mod_Gr, type="state", level=0.85)
      #
#**************************************************************************#####
#7 [Graze] TREATMENT----
#***************************************************************************

DICK_Abundance_mods_Gr = function(df) {
  TSF                               =pcount(~Obs+Wind   ~TSF+Crop_Prop+Tree_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo_TSF                     =pcount(~Obs+Wind   ~TSF+Crop_Prop+Tree_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat_TSF      =pcount(~Obs+Wind   ~TSF+Crop_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat_TSF        =pcount(~Obs+Wind   ~TSF+Crop_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat_TSF                  =pcount(~Obs+Wind   ~TSF+Crop_Prop+Tree_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  Null                              =pcount(~Obs+Wind   ~Crop_Prop+Tree_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                         =pcount(~Obs+Wind   ~Crop_Prop+Tree_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat          =pcount(~Obs+Wind   ~Crop_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat            =pcount(~Obs+Wind   ~Crop_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                      =pcount(~Obs+Wind   ~Crop_Prop+Tree_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(TSF,  HerbYesNo_TSF,  HerbYesNo_v_GrazingTreat_TSF,  HerbYesNo_GrazingTreat_TSF,  GrazingTreat_TSF,   Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("TSF","HerbYesNo_TSF","HerbYesNo_v_GrazingTreat_TSF","HerbYesNo_GrazingTreat_TSF","GrazingTreat_TSF", "Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
}
BOBO_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+DOY+Wind+Clouds  ~Herb_Prop+Tree_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+DOY+Wind+Clouds  ~Herb_Prop+Tree_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+DOY+Wind+Clouds  ~Herb_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+DOY+Wind+Clouds  ~Herb_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+DOY+Wind+Clouds  ~Herb_Prop+Tree_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
 
  TSF                           =pcount(~Obs+DOY+Wind+Clouds  ~TSF+Herb_Prop+Tree_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo_TSF                 =pcount(~Obs+DOY+Wind+Clouds  ~TSF+Herb_Prop+Tree_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat_TSF    =pcount(~Obs+DOY+Wind+Clouds  ~TSF+Herb_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat_TSF  =pcount(~Obs+DOY+Wind+Clouds  ~TSF+Herb_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat_TSF              =pcount(~Obs+DOY+Wind+Clouds  ~TSF+Herb_Prop+Tree_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
   
  mods=list(TSF,  HerbYesNo_TSF,  HerbYesNo_v_GrazingTreat_TSF,  HerbYesNo_GrazingTreat_TSF,  GrazingTreat_TSF,   Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("TSF","HerbYesNo_TSF","HerbYesNo_v_GrazingTreat_TSF","HerbYesNo_GrazingTreat_TSF","GrazingTreat_TSF", "Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}
GRSP_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+Wind+Clouds   ~Herb_Prop+Tree_kmha+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+Wind+Clouds   ~Herb_Prop+Tree_kmha+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+Wind+Clouds   ~Herb_Prop+Tree_kmha+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+Wind+Clouds   ~Herb_Prop+Tree_kmha+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+Wind+Clouds   ~Herb_Prop+Tree_kmha+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
 
  TSF                           =pcount(~Obs+Wind+Clouds   ~TSF+Herb_Prop+Tree_kmha+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo_TSF                 =pcount(~Obs+Wind+Clouds   ~TSF+Herb_Prop+Tree_kmha+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat_TSF    =pcount(~Obs+Wind+Clouds   ~TSF+Herb_Prop+Tree_kmha+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat_TSF  =pcount(~Obs+Wind+Clouds   ~TSF+Herb_Prop+Tree_kmha+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat_TSF              =pcount(~Obs+Wind+Clouds   ~TSF+Herb_Prop+Tree_kmha+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
   
  mods=list(TSF,  HerbYesNo_TSF,  HerbYesNo_v_GrazingTreat_TSF,  HerbYesNo_GrazingTreat_TSF,  GrazingTreat_TSF,   Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("TSF","HerbYesNo_TSF","HerbYesNo_v_GrazingTreat_TSF","HerbYesNo_GrazingTreat_TSF","GrazingTreat_TSF", "Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}
EAME_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Herb_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Herb_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Herb_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Herb_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Herb_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  TSF                           =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~TSF+Herb_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo_TSF                 =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~TSF+Herb_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat_TSF    =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~TSF+Herb_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat_TSF  =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~TSF+Herb_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat_TSF              =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~TSF+Herb_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(TSF,  HerbYesNo_TSF,  HerbYesNo_v_GrazingTreat_TSF,  HerbYesNo_GrazingTreat_TSF,  GrazingTreat_TSF,   Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("TSF","HerbYesNo_TSF","HerbYesNo_v_GrazingTreat_TSF","HerbYesNo_GrazingTreat_TSF","GrazingTreat_TSF", "Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
}
RWBL_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+StartTime+DOY   ~Crop_Prop+Tree_kmha+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+StartTime+DOY   ~Crop_Prop+Tree_kmha+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+StartTime+DOY   ~Crop_Prop+Tree_kmha+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+StartTime+DOY   ~Crop_Prop+Tree_kmha+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+StartTime+DOY   ~Crop_Prop+Tree_kmha+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
 
  TSF                           =pcount(~Obs+StartTime+DOY   ~TSF+Crop_Prop+Tree_kmha+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo_TSF                 =pcount(~Obs+StartTime+DOY   ~TSF+Crop_Prop+Tree_kmha+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat_TSF    =pcount(~Obs+StartTime+DOY   ~TSF+Crop_Prop+Tree_kmha+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat_TSF  =pcount(~Obs+StartTime+DOY   ~TSF+Crop_Prop+Tree_kmha+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat_TSF              =pcount(~Obs+StartTime+DOY   ~TSF+Crop_Prop+Tree_kmha+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(TSF,  HerbYesNo_TSF,  HerbYesNo_v_GrazingTreat_TSF,  HerbYesNo_GrazingTreat_TSF,  GrazingTreat_TSF,   Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("TSF","HerbYesNo_TSF","HerbYesNo_v_GrazingTreat_TSF","HerbYesNo_GrazingTreat_TSF","GrazingTreat_TSF", "Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}
HESP_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  TSF                           =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+TSF+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo_TSF                 =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+TSF+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat_TSF    =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+TSF+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat_TSF  =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+TSF+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat_TSF              =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+TSF+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  
  mods=list(TSF,  HerbYesNo_TSF,  HerbYesNo_v_GrazingTreat_TSF,  HerbYesNo_GrazingTreat_TSF,  GrazingTreat_TSF,   Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("TSF","HerbYesNo_TSF","HerbYesNo_v_GrazingTreat_TSF","HerbYesNo_GrazingTreat_TSF","GrazingTreat_TSF", "Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}
SEWR_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+StartTime+Clouds+Robel+DOY   ~Crop_Prop+Tree_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+StartTime+Clouds+Robel+DOY   ~Crop_Prop+Tree_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+StartTime+Clouds+Robel+DOY   ~Crop_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+StartTime+Clouds+Robel+DOY   ~Crop_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+StartTime+Clouds+Robel+DOY   ~Crop_Prop+Tree_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
 
  TSF                           =pcount(~Obs+StartTime+Clouds+Robel+DOY   ~TSF+Crop_Prop+Tree_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo_TSF                 =pcount(~Obs+StartTime+Clouds+Robel+DOY   ~TSF+Crop_Prop+Tree_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat_TSF    =pcount(~Obs+StartTime+Clouds+Robel+DOY   ~TSF+Crop_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat_TSF =pcount(~Obs+StartTime+Clouds+Robel+DOY    ~TSF+Crop_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat_TSF              =pcount(~Obs+StartTime+Clouds+Robel+DOY   ~TSF+Crop_Prop+Tree_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
   
  mods=list(TSF,  HerbYesNo_TSF,  HerbYesNo_v_GrazingTreat_TSF,  HerbYesNo_GrazingTreat_TSF,  GrazingTreat_TSF,   Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("TSF","HerbYesNo_TSF","HerbYesNo_v_GrazingTreat_TSF","HerbYesNo_GrazingTreat_TSF","GrazingTreat_TSF", "Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}

DICK_Abundance_mods_Gr(DICK_PCount_Gr) #>>Dickcissels:HerbYesNo_v_GrazingTreat----
    #                              K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbYesNo_v_GrazingTreat     22 3210.410    0.0000 0.6336 0.6336 -1583.205
    # HerbYesNo_v_GrazingTreat_TSF 23 3211.516    1.1062 0.3644 0.9980 -1582.758
    # HerbYesNo_GrazingTreat_TSF   21 3222.784   12.3740 0.0013 0.9993 -1590.392
    # HerbYesNo_GrazingTreat       20 3223.999   13.5890 0.0007 1.0000 -1592.000
    # HerbYesNo                    18 3237.253   26.8424 0.0000 1.0000 -1600.626
    # HerbYesNo_TSF                19 3239.211   28.8007 0.0000 1.0000 -1600.605
    # GrazingTreat                 19 3333.366  122.9554 0.0000 1.0000 -1647.683
    # GrazingTreat_TSF             20 3333.410  123.0003 0.0000 1.0000 -1646.705
    # TSF                          18 3337.155  126.7452 0.0000 1.0000 -1650.578
    # Null                         17 3338.631  128.2210 0.0000 1.0000 -1652.316

BOBO_Abundance_mods_Gr(BOBO_PCount_Gr) #>>Bobolinks: HerbYesNo_v_GrazingTreat_TSF----
    #                              K      AIC Delta_AIC AICWt Cum.Wt        LL
    # HerbYesNo_v_GrazingTreat_TSF 25 2969.517    0.0000     1      1 -1459.758
    # GrazingTreat_TSF             22 2991.010   21.4927     0      1 -1473.505
    # HerbYesNo_GrazingTreat_TSF   23 2991.595   22.0776     0      1 -1472.797
    # TSF                          20 3006.085   36.5678     0      1 -1483.042
    # HerbYesNo_TSF                21 3007.935   38.4179     0      1 -1482.967
    # HerbYesNo_v_GrazingTreat     24 3035.201   65.6837     0      1 -1493.600
    # HerbYesNo_GrazingTreat       22 3099.738  130.2205     0      1 -1527.869
    # GrazingTreat                 21 3099.947  130.4304     0      1 -1528.974
    # Null                         19 3137.311  167.7941     0      1 -1549.656
    # HerbYesNo                    20 3138.762  169.2453     0      1 -1549.381

GRSP_Abundance_mods_Gr(GRSP_PCount_Gr) #>>Grasshopper Sparrow:HerbYesNo_v_GrazingTreat----
      #                              K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbYesNo_v_GrazingTreat     23 2187.200    0.0000 0.3084 0.3084 -1070.600
      # HerbYesNo_GrazingTreat       21 2187.626    0.4261 0.2492 0.5576 -1072.813
      # GrazingTreat                 20 2188.586    1.3860 0.1542 0.7118 -1074.293
      # HerbYesNo_v_GrazingTreat_TSF 24 2189.080    1.8795 0.1205 0.8323 -1070.540
      # HerbYesNo_GrazingTreat_TSF   22 2189.396    2.1956 0.1029 0.9352 -1072.698
      # GrazingTreat_TSF             21 2190.321    3.1211 0.0648 1.0000 -1074.161
      # HerbYesNo_TSF                20 2230.807   43.6072 0.0000 1.0000 -1095.404
      # TSF                          19 2232.204   45.0033 0.0000 1.0000 -1097.102
      # HerbYesNo                    19 2234.335   47.1351 0.0000 1.0000 -1098.168
      # Null                         18 2235.083   47.8828 0.0000 1.0000 -1099.542

EAME_Abundance_mods_Gr(EAME_PCount_Gr) #>>Meadowlarks: HerbYesNo_GrazingTreat_TSF----
    #                              K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbYesNo_GrazingTreat_TSF   23 2203.247    0.0000 0.5173 0.5173 -1078.623
    # HerbYesNo_v_GrazingTreat_TSF 25 2204.265    1.0178 0.3110 0.8283 -1077.132
    # HerbYesNo_v_GrazingTreat     24 2206.184    2.9364 0.1192 0.9474 -1079.092
    # HerbYesNo_GrazingTreat       22 2207.903    4.6560 0.0504 0.9979 -1081.951
    # GrazingTreat_TSF             22 2214.848   11.6009 0.0016 0.9994 -1085.424
    # GrazingTreat                 21 2216.856   13.6089 0.0006 1.0000 -1087.428
    # HerbYesNo                    20 2227.554   24.3070 0.0000 1.0000 -1093.777
    # HerbYesNo_TSF                21 2228.641   25.3937 0.0000 1.0000 -1093.320
    # Null                         19 2231.441   28.1944 0.0000 1.0000 -1096.721
    # TSF                          20 2233.203   29.9555 0.0000 1.0000 -1096.601

RWBL_Abundance_mods_Gr(RWBL_PCount_Gr) #>>Red-winged Blackbird: HerbYesNo_GrazingTreat_TSF----
        #                              K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # HerbYesNo_GrazingTreat_TSF   22 2859.041    0.0000 0.3712 0.3712 -1407.520
        # HerbYesNo_GrazingTreat       21 2859.327    0.2865 0.3217 0.6929 -1408.664
        # HerbYesNo_v_GrazingTreat_TSF 24 2860.492    1.4509 0.1797 0.8726 -1406.246
        # HerbYesNo_v_GrazingTreat     23 2861.222    2.1811 0.1247 0.9974 -1407.611
        # GrazingTreat                 20 2869.584   10.5427 0.0019 0.9993 -1414.792
        # GrazingTreat_TSF             21 2871.545   12.5045 0.0007 1.0000 -1414.773
        # TSF                          19 3006.155  147.1142 0.0000 1.0000 -1484.078
        # HerbYesNo_TSF                20 3007.440  148.3993 0.0000 1.0000 -1483.720
        # HerbYesNo                    19 3042.776  183.7353 0.0000 1.0000 -1502.388
        # Null                         18 3043.888  184.8468 0.0000 1.0000 -1503.944

HESP_Abundance_mods_Gr(HESP_PCount_Gr) #>>Henslow's Sparrow:HerbYesNo_v_GrazingTreat_TSF----
      #                              K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbYesNo_v_GrazingTreat_TSF 23 1397.494    0.0000 0.8159 0.8159 -675.7468
      # HerbYesNo_GrazingTreat_TSF   21 1401.751    4.2569 0.0971 0.9130 -679.8752
      # HerbYesNo_v_GrazingTreat     22 1403.841    6.3472 0.0341 0.9471 -679.9203
      # HerbYesNo_TSF                19 1404.087    6.5931 0.0302 0.9773 -683.0433
      # GrazingTreat_TSF             20 1405.546    8.0526 0.0146 0.9919 -682.7730
      # TSF                          18 1408.111   10.6170 0.0040 0.9959 -686.0552
      # HerbYesNo_GrazingTreat       20 1409.563   12.0696 0.0020 0.9979 -684.7815
      # GrazingTreat                 19 1409.667   12.1737 0.0019 0.9997 -685.8336
      # HerbYesNo                    18 1414.511   17.0171 0.0002 0.9999 -689.2553
      # Null                         17 1415.588   18.0945 0.0001 1.0000 -690.7940
SEWR_Abundance_mods_Gr(SEWR_PCount_Gr)  #>>Sedge Wren: HerbYesNo_v_GrazingTreat_TSF ----
    #                              K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbYesNo_v_GrazingTreat_TSF 26 1036.146    0.0000 0.7201 0.7201 -492.0732
    # HerbYesNo_v_GrazingTreat     25 1039.322    3.1757 0.1472 0.8673 -494.6610
    # GrazingTreat_TSF             23 1040.674    4.5273 0.0749 0.9422 -497.3368
    # HerbYesNo_GrazingTreat_TSF   24 1041.485    5.3386 0.0499 0.9921 -496.7425
    # GrazingTreat                 22 1045.795    9.6487 0.0058 0.9979 -500.8975
    # HerbYesNo_GrazingTreat       23 1047.793   11.6464 0.0021 1.0000 -500.8964
    # HerbYesNo_TSF                22 1078.334   42.1879 0.0000 1.0000 -517.1671
    # TSF                          21 1085.280   49.1333 0.0000 1.0000 -521.6399
    # HerbYesNo                    21 1102.962   66.8153 0.0000 1.0000 -530.4808
    # Null                         20 1103.174   67.0281 0.0000 1.0000 -531.5872
#***************************************************************************####
#8 [Graze] CHOSEN MODELS----
#***************************************************************************
Coefficients =function(model) {
  coefs=c(coef(model, type = "state"))
  coefs=data.frame(coefs)
}
ConfidenceIntervals = function(model,coefs) {
  CIs = as.data.frame(confint(model, type = "state", level = 0.85))
  colnames(CIs)=c("LCL", "UCL") #renames columns
  CIs
  coefs$LCL=CIs$LCL
  coefs$UCL=CIs$UCL
  coefs
} #confidence intervals

#Top dickcissel model: ~Obs+Wind   ~Tree_Prop+Year+HerbYesNo*GrazingTreat----
DICK_Top_Gr=pcount(~Obs+Wind   ~Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=DICK_PCount_Gr, mixture="ZIP",K=100)
summary(DICK_Top_Gr) 
DICK_Abund_coef_df_Gr=Coefficients(DICK_Top_Gr) 
    #crop proportion was a pretending variable,dropped


#Top bobolink model: ~Obs+DOY+Wind   ~TSF+Crop_Prop+Tree_kmha+Year+HerbYesNo*GrazingTreat ----
BOBO_Top_Gr=pcount(~Obs+DOY+Wind   ~TSF+Herb_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=BOBO_PCount_Gr, mixture="ZIP",K=100)
      summary(BOBO_Top_Gr)
      BOBO_Abund_coef_df_Gr=Coefficients(BOBO_Top_Gr) 
      confint(BOBO_Top_Gr,type="state",level=0.85)
      #clouds seems to be a pretending variable, dropped

#Top GRSP model: ~Obs+Wind   ~Herb_Prop+Tree_kmha+Year+HerbYesNo+GrazingTreat ----
GRSP_Top_Gr=pcount(~Obs+Wind   ~Herb_Prop+Tree_kmha+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=GRSP_PCount_Gr, mixture="ZIP",K=100)
      summary(GRSP_Top_Gr)
      GRSP_Abund_coef_df_Gr=Coefficients(GRSP_Top_Gr)
      confint(GRSP_Top_Gr,type="det",level=0.85)
      #looks like clouds is pretending, removed

#Top meadowlarks model: ~Obs+Robel   ~TSF+Herb_Prop+Year+HerbYesNo+GrazingTreat ----
EAME_Top_Gr=pcount(~Obs+Robel   ~TSF+Herb_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=EAME_PCount_Gr, mixture="ZIP",K=100)
    summary(EAME_Top_Gr)
    EAME_Abund_coef_df_Gr=Coefficients(EAME_Top_Gr) 
    confint(EAME_Top_Gr,type="det",level=0.85)
    #wind and starttime AND clouds look like they are uninformative....?!

#Top red-winged blackbird model: ~Obs+StartTime+DOY  ~Crop_Prop+Year+HerbYesNo+GrazingTreat ----
RWBL_Top_Gr=pcount(~Obs+StartTime+DOY  ~Crop_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=RWBL_PCount_Gr, mixture="ZIP",K=100)
    summary(RWBL_Top_Gr)
    RWBL_Abund_coef_df_Gr=Coefficients(RWBL_Top_Gr) 
    confint(RWBL_Top_Gr,type="state",level=0.85)
    #Tree_kmha is likely uninformative, also TSF

#Top Henslow's sparrow model: ~Obs+StartTime+Clouds   ~TSF+Herb_Prop+Year+HerbYesNo*GrazingTreat+TSF ----
HESP_Top_Gr=pcount(~Obs+StartTime+Clouds   ~TSF+Herb_Prop+Year+HerbYesNo*GrazingTreat+TSF+offset(log(Area_ha)), data=HESP_PCount_Gr, mixture="ZIP",K=100)
    summary(HESP_Top_Gr)
    HESP_Abund_coef_df_Gr=Coefficients(HESP_Top_Gr) 
    confint(HESP_Top_Gr,type="state",level=0.85)
    #startime may be uninformative - but keeping because it's close. 

#Top sedge wren model: ~Obs+StartTime++DOY   ~TSF+Crop_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat ----
SEWR_Top_Gr=pcount(~Obs+StartTime+DOY   ~TSF+Crop_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=SEWR_PCount_Gr, mixture="ZIP",K=100)
    summary(SEWR_Top_Gr)
    SEWR_Abund_coef_df_Gr=Coefficients(SEWR_Top_Gr)
    confint(SEWR_Top_Gr,type="state",level=0.85)
    #robel and clouds uninformative

#Coefs and CIs
DICK_Coefs_CIs_Gr=ConfidenceIntervals(DICK_Top_Gr,DICK_Abund_coef_df_Gr) #>>Dickcissels----
print(DICK_Coefs_CIs_Gr)
    #                                        coefs        LCL         UCL
    # lam(Int)                           -0.4278596 -0.7850291 -0.07069007
    # lam(Tree_Prop)                     -1.1716067 -1.7356118 -0.60760165
    # lam(Yearb)                          0.5573372  0.3383662  0.77630815
    # lam(Yearc)                          0.4129375  0.1754565  0.65041844
    # lam(Yeard)                          0.6646025  0.4335240  0.89568106
    # lam(HerbYesNoYes)                   1.4466334  1.1458657  1.74740114
    # lam(GrazingTreatNone)               1.0854407  0.7999338  1.37094760
    # lam(GrazingTreatSLS)                0.6911441  0.4077084  0.97457982
    # lam(HerbYesNoYes:GrazingTreatNone) -0.9377849 -1.2727757 -0.60279422
    # lam(HerbYesNoYes:GrazingTreatSLS)  -0.6040995 -0.9532252 -0.25497384

BOBO_Coefs_CIs_Gr=ConfidenceIntervals(BOBO_Top_Gr,BOBO_Abund_coef_df_Gr) #>>Bobolinks----
print(BOBO_Coefs_CIs_Gr)
    #                                        coefs         LCL        UCL
    # lam(Int)                           -1.1831663 -1.66449668 -0.7018359
    # lam(TSF)                            0.1332735  0.10966886  0.1568781
    # lam(Crop_Prop)                     -1.8656483 -2.39208777 -1.3392088
    # lam(Tree_kmha)                      2.1410688  1.81601109  2.4661264
    # lam(Yearb)                          0.5534394  0.32488695  0.7819918
    # lam(Yearc)                          0.3908850  0.12028107  0.6614889
    # lam(Yeard)                          0.5328973  0.28538795  0.7804066
    # lam(HerbYesNoYes)                  -1.3180339 -1.94983146 -0.6862363
    # lam(GrazingTreatNone)               0.2239174 -0.07548341  0.5233182
    # lam(GrazingTreatSLS)                0.6657508  0.37223225  0.9592693
    # lam(HerbYesNoYes:GrazingTreatNone)  1.5786725  0.93047096  2.2268741
    # lam(HerbYesNoYes:GrazingTreatSLS)   0.8831896  0.22590766  1.5404715

GRSP_Coefs_CIs_Gr=ConfidenceIntervals(GRSP_Top_Gr,GRSP_Abund_coef_df_Gr) #>>Grasshopper Sparrow----
print(GRSP_Coefs_CIs_Gr)
      #                            coefs         LCL        UCL
      # lam(Int)               0.27520980 -0.17574462  0.7261642
      # lam(Herb_Prop)         2.16120456  1.32492514  2.9974840
      # lam(Tree_kmha)        -0.69942280 -1.15476008 -0.2440855
      # lam(Yearb)            -0.57322734 -0.79980797 -0.3466467
      # lam(Yearc)            -0.50684865 -0.77471307 -0.2389842
      # lam(Yeard)            -0.55905243 -0.81060658 -0.3074983
      # lam(HerbYesNoYes)      0.16831419  0.03192109  0.3047073
      # lam(GrazingTreatNone) -1.27842655 -1.55185902 -1.0049941
      # lam(GrazingTreatSLS)  -0.01606099 -0.16628895  0.1341670

EAME_Coefs_CIs_Gr=ConfidenceIntervals(EAME_Top_Gr,EAME_Abund_coef_df_Gr) #>>Meadowlarks----
print(EAME_Coefs_CIs_Gr)

    #                           coefs         LCL         UCL
    # lam(Int)               0.3357535 -0.35710914  1.02861614
    # lam(TSF)               0.0555445  0.02569348  0.08539551
    # lam(Herb_Prop)         2.1572649  1.55814215  2.75638768
    # lam(Yearb)            -0.2813329 -0.52277877 -0.03988697
    # lam(Yearc)            -0.5903577 -0.88354235 -0.29717297
    # lam(Yeard)            -0.3014156 -0.57337257 -0.02945864
    # lam(HerbYesNoYes)     -0.3569569 -0.49557230 -0.21834159
    # lam(GrazingTreatNone) -1.0258475 -1.38912309 -0.66257196
    # lam(GrazingTreatSLS)   0.1320544 -0.01143201  0.27554091

RWBL_Coefs_CIs_Gr=ConfidenceIntervals(RWBL_Top_Gr,RWBL_Abund_coef_df_Gr) #>>Red-winged blackbird----
print(RWBL_Coefs_CIs_Gr)
    #                           coefs         LCL        UCL
    # lam(Int)               0.3800129  0.09008569  0.6699400
    # lam(Crop_Prop)        -1.4101491 -1.93600450 -0.8842938
    # lam(Yearb)             0.3595054  0.13200735  0.5870034
    # lam(Yearc)             0.3747472  0.11674432  0.6327500
    # lam(Yeard)             0.1111448 -0.15552261  0.3778122
    # lam(HerbYesNoYes)      0.2632856  0.15403528  0.3725359
    # lam(GrazingTreatNone)  1.2942439  1.12553479  1.4629529
    # lam(GrazingTreatSLS)   0.2051901  0.01722081  0.3931594

HESP_Coefs_CIs_Gr=ConfidenceIntervals(HESP_Top_Gr,HESP_Abund_coef_df_Gr) #>>Henslow's Sparrow----
print(HESP_Coefs_CIs_Gr)
    #                                        coefs         LCL        UCL
    # lam(Int)                           -1.9669828 -2.91449519 -1.0194703
    # lam(TSF)                            0.1058802  0.05241402  0.1593464
    # lam(Herb_Prop)                      1.7564795  0.82652363  2.6864353
    # lam(Yearb)                          0.3416083 -0.12609444  0.8093111
    # lam(Yearc)                          0.6496753  0.17142095  1.1279297
    # lam(Yeard)                          0.5573353  0.06534447  1.0493261
    # lam(HerbYesNoYes)                  -1.9599470 -2.92974703 -0.9901470
    # lam(GrazingTreatNone)               0.2491421 -0.19663144  0.6949156
    # lam(GrazingTreatSLS)                0.1815431 -0.27518871  0.6382749
    # lam(HerbYesNoYes:GrazingTreatNone)  1.5878676  0.55798577  2.6177495
    # lam(HerbYesNoYes:GrazingTreatSLS)   1.8690425  0.84316555  2.8949195

SEWR_Coefs_CIs_Gr=ConfidenceIntervals(SEWR_Top_Gr,SEWR_Abund_coef_df_Gr) #>>Sedge Wren----
print(SEWR_Coefs_CIs_Gr)
      #                                        coefs         LCL        UCL
      # lam(Int)                           -0.9434255 -1.64805934 -0.2387917
      # lam(TSF)                            0.0510421  0.01713291  0.0849513
      # lam(Crop_Prop)                     -1.3806618 -2.31171196 -0.4496117
      # lam(Tree_Prop)                     -5.0285525 -6.46425147 -3.5928536
      # lam(Yearb)                          0.5139921  0.11400604  0.9139781
      # lam(Yearc)                          0.9686072  0.56820571  1.3690087
      # lam(Yeard)                          0.2590959 -0.28239215  0.8005840
      # lam(HerbYesNoYes)                  -1.4193350 -2.94459814  0.1059281
      # lam(GrazingTreatNone)               1.9026380  1.40499574  2.4002803
      # lam(GrazingTreatSLS)                0.3238686 -0.25970320  0.9074405
      # lam(HerbYesNoYes:GrazingTreatNone)  1.5511149  0.01049038  3.0917394
      # lam(HerbYesNoYes:GrazingTreatSLS)   0.1911650 -1.48704431  1.8693743




#***************************************************************************####
#9 [Graze] PREDICTION + GRAPHS----
#***************************************************************************
mean((predict(DICK_Top_Gr, type = "det"))[,1])
mean((predict(BOBO_Top_Gr, type = "det"))[,1])
mean((predict(EAME_Top_Gr, type = "det"))[,1])
mean((predict(RWBL_Top_Gr, type = "det"))[,1])
mean((predict(HESP_Top_Gr, type = "det"))[,1])

LandCoverMeans_Gr=matrix(c(mean(DICK_Graze$AvgOfHerbProp),mean(DICK_Graze$AvgOfCropProp),mean(DICK_Graze$AvgOfTreeProp),mean(DICK_Graze$AvgOfTree_kmHa)))

    # [1,] 0.6225418 - Herb
    # [2,] 0.1785696 - Crop
    # [3,] 0.1749629 - Tree
    # [4,] 0.7091316 - Tree kmHa

dodge <- position_dodge(width=0.9) #ggplot prep

#produce predicted values for abundance "state" variables across values in newdata
PredictedValues=function(model,newdata) {
  abundance_estimates = as.data.frame(predict(model, type = "state", newdata = newdata, level=0.85,appendData = T))
  abundance_estimates
}

#>>Dickcissel Graph----
DICK_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                          Tree_Prop=0.17)

DICK_Predicted_Gr=PredictedValues(DICK_Top_Gr,DICK_newdata_Gr)
DICK_Predicted_Gr_Sum= DICK_Predicted_Gr %>% 
  group_by(GrazingTreat,HerbYesNo) %>% 
  summarise_at(vars(Predicted, lower, upper), mean) #averages out the grazing treatments so we can focus on Grazeicide treatments only
#View(DICK_Predicted_Gr_Sum) 

DICK_Predicted_Gr_Sum$GrazingTreat=factor(DICK_Predicted_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None")) #ordering

DICK.plot_Gr <- ggplot(data = DICK_Predicted_Gr_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=GrazingTreat, 
               fill=HerbYesNo), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,5), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"),labels=c("No","Yes"))+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10,color="black",face="bold"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  
  scale_x_discrete(labels=c("Intensive-Early","Season-Long","None"))+
  geom_errorbar(aes(x = GrazingTreat, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbYesNo), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Grazing Treatment", 
       fill = "Herbicide Treatment") + #sets label for whatever you used to "fill" the bars
  ggtitle("Dickcissels")  #can use this to set a main title above the plot
DICK.plot_Gr


#>>Bobolink Graph----
BOBO_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                              GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                              HerbYesNo = c("Yes", "No"),
                              Area_ha = 1,
                          Herb_Prop = 0.62,
                          Tree_Prop = 0.17,
                          TSF=4)


BOBO_Predicted_Gr=PredictedValues(BOBO_Top_Gr,BOBO_newdata_Gr)
BOBO_Predicted_Gr_Sum=BOBO_Predicted_Gr %>% 
  group_by(GrazingTreat, HerbYesNo) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on Grazeicide treatments only
#View(BOBO_Predicted_Gr_Sum) 

BOBO_Predicted_Gr_Sum$GrazingTreat=factor(BOBO_Predicted_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None")) #ordering

BOBO.plot_Gr <- ggplot(data = BOBO_Predicted_Gr_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=GrazingTreat, 
               fill=HerbYesNo), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,5), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=10, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  
  scale_x_discrete(labels=c("Intensive-Early","Season-Long","None"))+
  geom_errorbar(aes(x = GrazingTreat, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbYesNo), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Grazing Treatment", 
       fill = "Herbicide Treatment") + #sets label for whatever you used to "fill" the bars
  ggtitle("Bobolinks")  #can use this to set a main title above the plot
BOBO.plot_Gr

mean(DICK_Graze$TSF)

#>>Grasshopper Sparrow Graph----
GRSP_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                             Herb_Prop = 0.62,
                             Tree_kmha=0.71)
GRSP_Predicted_Gr=PredictedValues(GRSP_Top_Gr,GRSP_newdata_Gr)
GRSP_Predicted_Gr_Sum=GRSP_Predicted_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on Grazeicide treatments only
#View(GRSP_Predicted_Gr_Sum) 
GRSP_Predicted_Gr_Sum$GrazingTreat=factor(GRSP_Predicted_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None")) #ordering

GRSP.plot_Gr <- ggplot(data = GRSP_Predicted_Gr_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=GrazingTreat, 
               fill=HerbYesNo), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(breaks=c(0,1,2,3,4),limits = c(0,4), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=10, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  scale_x_discrete(labels=c("Intensive-Early","Season-Long","None"))+
  geom_errorbar(aes(x = GrazingTreat, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbYesNo), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Grazing Treatment", 
       fill = "Herbicide") + #sets label for whatever you used to "fill" the bars
  ggtitle("Meadowlarks")  #can use this to set a main title above the plot
GRSP.plot_Gr


#>>Meadowlark Graph----
EAME_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                             Herb_Prop = 0.62,
                             TSF=3)
EAME_Predicted_Gr=PredictedValues(EAME_Top_Gr,EAME_newdata_Gr)
EAME_Predicted_Gr_Sum=EAME_Predicted_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on Grazeicide treatments only
#View(EAME_Predicted_Gr_Sum) 
EAME_Predicted_Gr_Sum$GrazingTreat=factor(EAME_Predicted_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None")) #ordering

EAME.plot_Gr <- ggplot(data = EAME_Predicted_Gr_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=GrazingTreat, 
               fill=HerbYesNo), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(breaks=c(0,2,4,6,8,10),limits = c(0,11), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=10, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  scale_x_discrete(labels=c("Intensive-Early","Season-Long","None"))+
  geom_errorbar(aes(x = GrazingTreat, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbYesNo), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Grazing Treatment", 
       fill = "Herbicide") + #sets label for whatever you used to "fill" the bars
  ggtitle("Meadowlarks")  #can use this to set a main title above the plot
EAME.plot_Gr

#>>Red-winged Blackbird Graph: STOPPED HERE----
RWBL_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                             Crop_Prop=0.18)


RWBL_Predicted_Gr=PredictedValues(RWBL_Top_Gr,RWBL_newdata_Gr)
RWBL_Predicted_Gr_Sum=RWBL_Predicted_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on Grazeicide treatments only
#View(RWBL_Predicted_Gr_Sum) 


RWBL_Predicted_Gr_Sum$GrazingTreat=factor(RWBL_Predicted_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None")) #ordering

RWBL.plot_Gr <- ggplot(data = RWBL_Predicted_Gr_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=GrazingTreat, 
               fill=HerbYesNo), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,7), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=10, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  scale_x_discrete(labels=c("Intensive-Early","Season-Long","None"))+
  geom_errorbar(aes(x = GrazingTreat, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbYesNo), #you need to use group so it knows to distribute the bars across your groups
                    position = dodge, 
                    width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Grazing Treatment", 
       fill = "Herbicide") + #sets label for whatever you used to "fill" the bars
  ggtitle("Red-winged Blackbirds")  #can use this to set a main title above the plot
RWBL.plot_Gr

#>>Henslow's Sparrow Graph----
HESP_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                            Herb_Prop=0.62,
                            TSF=3)


HESP_Predicted_Gr=PredictedValues(HESP_Top_Gr,HESP_newdata_Gr)
HESP_Predicted_Gr_Sum=HESP_Predicted_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on Grazeicide treatments only
#View(HESP_Predicted_Gr_Sum) 

HESP_Predicted_Gr_Sum$GrazingTreat=factor(HESP_Predicted_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None")) #ordering

HESP.plot_Gr <- ggplot(data = HESP_Predicted_Gr_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=GrazingTreat, 
               fill=HerbYesNo), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,3), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=10, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  
  scale_x_discrete(labels=c("Intensive-Early","Season-Long","None"))+
  geom_errorbar(aes(x = GrazingTreat, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbYesNo), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Grazing Treatment", 
       fill = "Herbicide") + #sets label for whatever you used to "fill" the bars
  ggtitle("Henslow's Sparrows")  #can use this to set a main title above the plot
HESP.plot_Gr

#>>Sedge wren Graph ----
SEWR_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                             Crop_Prop=0.18,
                             Tree_Prop=0.17,
                             TSF=3)


SEWR_Predicted_Gr=PredictedValues(SEWR_Top_Gr,SEWR_newdata_Gr)
SEWR_Predicted_Gr_Sum=SEWR_Predicted_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on Grazeicide treatments only
#View(SEWR_Predicted_Gr_Sum) 

SEWR_Predicted_Gr_Sum$GrazingTreat=factor(SEWR_Predicted_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None")) #ordering

SEWR.plot_Gr <- ggplot(data = SEWR_Predicted_Gr_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=GrazingTreat, 
               fill=HerbYesNo), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,3), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=10, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5)
  )+
  
  scale_x_discrete(labels=c("Intensive-Early","Season-Long","None"))+
  geom_errorbar(aes(x = GrazingTreat, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbYesNo), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Birds per Ha", 
       x = "Grazing Treatment", 
       fill = "Herbicide") + #sets label for whatever you used to "fill" the bars
  ggtitle("Sedge Wrens")  #can use this to set a main title above the plot
SEWR.plot_Gr

#final Herb plot----
#final.plot=plot_grid(DICK.plot,BOBO.plot,EAME.plot,RWBL.plot,HESP.plot,SEWR.plot,nrow=3,ncol=2)

legend_a <- get_legend(
  DICK.plot_Gr + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

all.Graze.plot <- plot_grid(
  DICK.plot_Gr + theme(legend.position="none"),
  EAME.plot_Gr + theme(legend.position="none"),
  BOBO.plot_Gr + theme(legend.position="none"),
  RWBL.plot_Gr + theme(legend.position="none"),
  HESP.plot_Gr + theme(legend.position="none"),
  SEWR.plot_Gr + theme(legend.position="none"),
  align = 'vh',
  #labels = c("A", "B", "C"),
  hjust = -1,
  nrow = 3
)
final.Graze.plot<-plot_grid(
  all.Graze.plot,
  legend_a, 
  ncol = 1, 
  rel_heights = c(1, .1))
final.Graze.plot





#*******************************####
#### goodness of fit from Jane, this is sketchy. You want chisq p>0.05 according to Kirk ####

#fitstats <- function(model_12){
 observed <- getY(model_12@data)
expected <- fitted(model_12)
resids <- residuals(model_12)
sse <- sum(resids^2)
chisq <- sum((observed - expected)^2 / expected)
freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
out <- c(SSe = sse, Chisq = chisq, freemanTukey = freeTuke)
return(out)
}
#pb = goodness of fit results
#freeman Tukey value of T0 should be within the t_B quantile ranges to indicate a good fit
#see Reiley and Benson 2019
(pb <- parboot(model_12, fitstats, nsim = 25, report = 1))
plot(pb, main = "")


#SAMPLE OCCU CODE from Jane####



df.GRCA = unmarkedFrameOccu(y = GRCA_visits,
                            siteCovs = data.frame((Area = GRCA[,c("Area")]), 
                                                  (treatment = GRCA[,c("Secondary_treatment")]),
                                                  (maxheight = GRCA[,c("max_height")]),
                                                  (robel = GRCA[,c("robel")]),
                                                  (cv_maxheight = GRCA[,c("cv_maxheight")]),
                                                  (year = GRCA[,c("Year.1")])),
                            obsCovs  = list(clouds = GRCA_clouds, dates = GRCA_dates, obss = GRCA_obss, temps = GRCA_temps, 
                                            winds = GRCA_winds, times = GRCA_times))




##detection
#shrubland
null.GRCA    = occu(~1 ~1, data = df.GRCA)
clouds.GRCA  = occu(~clouds ~1, data = df.GRCA)
dates.GRCA   = occu(~dates ~1, data = df.GRCA)
obss.GRCA    = occu(~obss ~1, data = df.GRCA)
temps.GRCA   = occu(~temps ~1, data = df.GRCA)
winds.GRCA   = occu(~winds ~1, data = df.GRCA)
times.GRCA   = occu(~times ~1, data = df.GRCA)

fitlist_detection_GRCA = fitList(null.GRCA, clouds.GRCA, dates.GRCA, obss.GRCA, temps.GRCA, winds.GRCA, times.GRCA)
modSel(fitlist_detection)


model.GRCA        = occu(~obss ~year+robel+cv_maxheight+maxheight+Area+treatment, data = df.GRCA)
summary(model.GRCA)


predict(model.GRCA, type = "state", newdata = newdata, appendData = T)
occu_GRCA = predict(model.GRCA, type = "state", newdata = newdata)


#test for correlations
myvars1 = c("Clouds_1", "Date_1_Ord", "Winds_1","AvgOfAvg_Robel","StartTime_1")
myvars2 = c("Clouds_2", "Date_2_Ord", "Winds_2","AvgOfAvg_Robel","StartTime_2")
myvars3 = c("Clouds_3", "Date_3_Ord", "Winds_3","AvgOfAvg_Robel","StartTime_3")
myvars4 = c("Clouds_4", "Date_4_Ord", "Winds_4","AvgOfAvg_Robel","StartTime_4")
myvars5 = c("Clouds_5", "Date_5_Ord", "Winds_5","AvgOfAvg_Robel","StartTime_5")

check_correlations = DICK_Herb[myvars5]
cor(check_correlations)




#checking on crop proportions impact on dickcissels
CropPredictedValues=function(model,newdata) {
  abundance_estimates = as.data.frame(predict(model, type = "state", newdata = newdata,level=0.85, appendData = T, ))
  abundance_estimates
}
Crop =pcount(~Obs+StartTime+Robel+DOY   ~Crop_Prop+offset(log(Area_ha)), data=DICK_PCount, mixture="ZIP", K=100)
summary(DICK_Herb$AvgOfCropProp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0469  0.1119  0.1199  0.1957  0.1970  0.6086 
Crop_newdata = data.frame(Crop_Prop=c(00.1,0.2,0.3,0.4,0.5,0.6),Area_ha = 1)
Crop_Predicted=CropPredictedValues(Crop,Crop_newdata)
Crop_Predicted

