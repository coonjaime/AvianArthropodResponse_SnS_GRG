####METADATA####

#Attributions
#>> Code accompanies the following in review manuscript:
#Coon, J.J., S.B. Maresh Nelson, R.C. Daughtridge, W.H. Schacht, D.M. Debinski, and J.R. Miller
#Title....
#>> Code was written by J. Coon, with assistance from R. Daughtridge and T. Swartz.

#Definitions of terms
#>>HERB  n=7 sites, treated with experimental herbicide with controls
#>>GRAZE n=18 sites, mix of experimental and non-experimental herbicide, 
#used to assess interactions between herbicide and grazing

#Grassland Bird Species Alpha Codes
#>>Bobolink = BOBO
#>>Dickcissel = DICK (I know)
#>>Grasshoopper sparrow = GRSP
#>>Henslow's Sparrow = HESP
#>>Eastern meadowlark = EAME
#>>Red-winged blackbird = RWBL

#**************************************************************************####
#SETUP>>>>>>>>>>.........####
#**************************************************************************####
#1. Packages----
#install.packages('easypackages') #only once
library(easypackages)
packages('unmarked','ggplot2','ggthemes','ggppubr','gridExtra','AICcmodavg','tidyverse','cowplot','wesanderson','dplyr') 

#2. Overall functions----
Data_Cleaning       = function (df) {
  
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
} # cleaning and filtering function for n=7 dataset
Data_Cleaning_Occu  = function (df) {
  df$Visit_1[df$Visit_1>0]=1
  df$Visit_2[df$Visit_2>0]=1
  df$Visit_3[df$Visit_3>0]=1
  df$Visit_4[df$Visit_4>0]=1
  df$Visit_5[df$Visit_5>0]=1
  df
} # cleaning and filtering function for occupancy modeling on n=7
unmarked_data       = function(df) {
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
} #setting up unmarked 'pcount' for n=7 
occu_unmarked_data  = function(df) {
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
} #setting up unmarked 'occu' for n=7
Detection_mods      = function(df) {
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
} #set of detection models used for all species
Coefficients        = function(model) {
  coefs=c(coef(model, type = "state"))
  coefs=data.frame(coefs)
} #extracting coefficients from final models
ConfidenceIntervals = function(model,coefs) {
  CIs = as.data.frame(confint(model, type = "state", level = 0.85))
  colnames(CIs)=c("LCL", "UCL") #renames columns
  CIs
  coefs$LCL=CIs$LCL
  coefs$UCL=CIs$UCL
  coefs
} #extracting and organizing confidence intervals from final models
PredictedValues     = function(model,newdata) {
  abundance_estimates = as.data.frame(predict(model, type = "state", newdata = newdata, level=0.85,appendData = T))
  abundance_estimates
} #calculating predicted values for grazing
Data_Cleaning_Gr    = function (df) {
  
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
  
} # cleaning and filtering function for n=18 dataset
addLevel            = function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
} #adding a new level to a factor
unmarked_data_Gr    = function(df) {
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
}#setting up unmarked 'pcount' for n=18
dodge <- position_dodge(width=0.9) #ggplot prep for graphing


#2. Importing----
setwd("/cloud/project/Bird_Abund")

load("DICK_Herb.Rdata") #abundance n=7
load("BOBO_Herb.Rdata") #abundance n=7
load("GRSP_Herb.Rdata") #abundance n=7
load("EAME_Herb.Rdata") #abundance n=7
load("RWBL_Herb.Rdata") #abundance n=7
load("HESP_Herb.Rdata") #abundance n=7
load("SEWR_Herb.Rdata") #abundance n=7
load("HESP_Pres.Rdata") #presence/absence n=7
load("SEWR_Pres.Rdata") #presence/absence n=7
load("DICK_Graze.Rdata")#abundance n=18
load("BOBO_Graze.Rdata")#abundance n=18
load("GRSP_Graze.Rdata")#abundance n=18
load("EAME_Graze.Rdata")#abundance n=18
load("RWBL_Graze.Rdata")#abundance n=18
load("HESP_Graze.Rdata")#abundance n=18
load("SEWR_Graze.Rdata")#abundance n=18


#3. Exploring Data ----

#How many detections in the n=18 Graze sample?
DICK_Det_Gr=sum(DICK_Graze$Visit_1)+sum(DICK_Graze$Visit_2)+sum(DICK_Graze$Visit_3)+sum(DICK_Graze$Visit_4)
BOBO_Det_Gr=sum(BOBO_Graze$Visit_1)+sum(BOBO_Graze$Visit_2)+sum(BOBO_Graze$Visit_3)+sum(BOBO_Graze$Visit_4)
GRSP_Det_Gr=sum(GRSP_Graze$Visit_1)+sum(GRSP_Graze$Visit_2)+sum(GRSP_Graze$Visit_3)+sum(GRSP_Graze$Visit_4)
EAME_Det_Gr=sum(EAME_Graze$Visit_1)+sum(EAME_Graze$Visit_2)+sum(EAME_Graze$Visit_3)+sum(EAME_Graze$Visit_4)
RWBL_Det_Gr=sum(RWBL_Graze$Visit_1)+sum(RWBL_Graze$Visit_2)+sum(RWBL_Graze$Visit_3)+sum(RWBL_Graze$Visit_4)
HESP_Det_Gr=sum(HESP_Graze$Visit_1)+sum(HESP_Graze$Visit_2)+sum(HESP_Graze$Visit_3)+sum(HESP_Graze$Visit_4) 
SEWR_Det_Gr=sum(SEWR_Graze$Visit_1)+sum(SEWR_Graze$Visit_2)+sum(SEWR_Graze$Visit_3)+sum(SEWR_Graze$Visit_4) 

Number_of_Detections_Gr= c(DICK_Det_Gr,BOBO_Det_Gr,GRSP_Det_Gr,EAME_Det_Gr, RWBL_Det_Gr,HESP_Det_Gr, SEWR_Det_Gr)
Det_Names_Gr= c("DICK","BOBO","GRSP","EAME","RWBL","HESP","SEWR")
Detections_Graze=as.data.frame(Number_of_Detections_Gr, Det_Names_Gr)
Detections_Graze

#Number_of_Detections_Gr
#DICK                    1298
#BOBO                    1008
#GRSP                     709
#EAME                     629
#RWBL                    1122
#HESP                     305
#SEWR                     200

#Mean land cover by type in sample?
LandCoverMeans_Gr=matrix(c(mean(DICK_Graze$AvgOfHerbProp),mean(DICK_Graze$AvgOfCropProp),mean(DICK_Graze$AvgOfTreeProp),mean(DICK_Graze$AvgOfTree_kmHa)))
LandCoverMeans_Gr
# [1,] 0.6225418 - Herb
# [2,] 0.1785696 - Crop
# [3,] 0.1749629 - Tree
# [4,] 0.7091316 - Tree kmHa

#test for correlations in covariates
myvars1 = c("Clouds_1", "Date_1_Ord", "Winds_1","AvgOfAvg_Robel","StartTime_1")
myvars2 = c("Clouds_2", "Date_2_Ord", "Winds_2","AvgOfAvg_Robel","StartTime_2")
myvars3 = c("Clouds_3", "Date_3_Ord", "Winds_3","AvgOfAvg_Robel","StartTime_3")
myvars4 = c("Clouds_4", "Date_4_Ord", "Winds_4","AvgOfAvg_Robel","StartTime_4")
myvars5 = c("Clouds_5", "Date_5_Ord", "Winds_5","AvgOfAvg_Robel","StartTime_5")

check_correlations = DICK_Graze[myvars5]
cor(check_correlations)
#********************************************************************************####
####ANALYSIS OF GRAZE SAMPLE####
#********************************************************************************####

  #Process for each bird: unmarked_data() prepares the dataset, Detection_mods() runs detection models, 
  #X_Landscape_mods() runs landscape models

#_______________________________________
#1. GRAZE_DICK####
DICK_PCount_Gr=unmarked_data_Gr(DICK_Graze) 
#>>Stage 1: Obs_Wind (no Robel)  ----

Detection_mods(DICK_PCount_Gr)
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

DICK_det_mod_Gr   =pcount(~Obs+Wind+Clouds+Robel ~1, data=DICK_PCount_Gr, mixture="ZIP",K=100)
confint(DICK_det_mod_Gr, type="det", level=0.85) #indicates Clouds  uninformative parameter
            #Cloud and Robel pretending

#_______________________________________
#>>Stage 2: Tree_Prop+Crop_Prop  ----
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

DICK_Landscape_mods_Gr(DICK_PCount_Gr)
        #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
        #CropProp_TreeProp 15 3337.153    0.0000 0.6479 0.6479 -1653.576
        #HerbProp_TreeProp 15 3338.446    1.2929 0.3394 0.9873 -1654.223
        #TreeProp          14 3345.076    7.9232 0.0123 0.9996 -1658.538
        #HerbProp          14 3352.190   15.0371 0.0004 1.0000 -1662.095
        #Null              13 3389.001   51.8481 0.0000 1.0000 -1681.501
        #CropProp          14 3390.997   53.8439 0.0000 1.0000 -1681.498


DICK_landscape_mod_Gr   =pcount(~Obs+Wind+Robel ~Tree_Prop+Crop_Prop, data=DICK_PCount_Gr, mixture="ZIP",K=100)
confint(DICK_landscape_mod_Gr, type="state", level=0.85) 

#_______________________________________
#>>Stage 3: HerbYesNo_v_GrazingTreat----
DICK_Abundance_mods_Gr = function(df) {
  Null                              =pcount(~Obs+Wind   ~Crop_Prop+Tree_Prop+Year                       +offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo                         =pcount(~Obs+Wind   ~Crop_Prop+Tree_Prop+Year+HerbYesNo             +offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat          =pcount(~Obs+Wind   ~Crop_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat            =pcount(~Obs+Wind   ~Crop_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                      =pcount(~Obs+Wind   ~Crop_Prop+Tree_Prop+Year+GrazingTreat          +offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
}

DICK_Abundance_mods_Gr(DICK_PCount_Gr) 
          #                           K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #HerbYesNo_v_GrazingTreat 23 3192.021    0.0000 0.9976 0.9976 -1573.011
          #HerbYesNo_GrazingTreat   21 3204.629   12.6072 0.0018 0.9995 -1581.314
          #HerbYesNo                19 3207.055   15.0338 0.0005 1.0000 -1584.528
          #GrazingTreat             20 3310.507  118.4853 0.0000 1.0000 -1635.253
          #Null                     18 3317.782  125.7607 0.0000 1.0000 -1640.891

DICK_abundance_mod_Gr   =pcount(~Obs+Wind+Robel ~Tree_Prop+Crop_Prop+HerbYesNo*GrazingTreat+Year, data=DICK_PCount_Gr, mixture="ZIP",K=100)
confint (DICK_abundance_mod_Gr, type="det", level=0.85) #indicates Robel is uninformative parameter
confint (DICK_abundance_mod_Gr, type="state", level=0.85) #indicates Robel is uninformative parameter

#_______________________________________
#>>Top model, Coefs, CIs----
DICK_Top_Gr=pcount(~Obs+Wind   ~Tree_Prop+Crop_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=DICK_PCount_Gr, mixture="ZIP",K=100)
summary(DICK_Top_Gr) 
DICK_Abund_coef_df_Gr=Coefficients(DICK_Top_Gr) 

DICK_Coefs_CIs_Gr=ConfidenceIntervals(DICK_Top_Gr,DICK_Abund_coef_df_Gr)
print(DICK_Coefs_CIs_Gr)
            #                                        coefs        LCL         UCL
            #lam(Int)                           -0.3975626 -0.7667213 -0.02840393
            #lam(Tree_Prop)                     -1.2206256 -1.8052911 -0.63596002
            #lam(Crop_Prop)                     -0.1400253 -0.5842057  0.30415511
            #lam(Yearb)                          0.5537600  0.3342941  0.77322587
            #lam(Yearc)                          0.4098896  0.1721224  0.64765686
            #lam(Yeard)                          0.6601794  0.4284991  0.89185974
            #lam(HerbYesNoYes)                   1.4461916  1.1457007  1.74668246
            #lam(GrazingTreatNone)               1.0927087  0.8066557  1.37876165
            #lam(GrazingTreatSLS)                0.6971332  0.4134578  0.98080873
            #lam(HerbYesNoYes:GrazingTreatNone) -0.9401962 -1.2750222 -0.60537027
            #lam(HerbYesNoYes:GrazingTreatSLS)  -0.6099973 -0.9597232 -0.26027144

#_______________________________________
#>>Dickcissel Graph----

DICK_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                             Tree_Prop=0.17,
                             Crop_Prop=0.18)

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
  scale_y_continuous(limits = c(0,6), expand = c(0, 0)) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 3))+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"),labels=c("No","Yes"))+
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12,color="black",face="bold"),
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


#__________________________####
#2. GRAZE_BOBO####
BOBO_PCount_Gr=unmarked_data_Gr(BOBO_Graze)
#>>Stage 1: Obs+Wind+Clouds+DOY---- 
Detection_mods(BOBO_PCount_Gr) 
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

#_______________________________________

#>>Stage2: Herb_Prop+Tree_Prop----
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

BOBO_Landscape_mods_Gr(BOBO_PCount_Gr) 

            #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
            # HerbProp_TreeProp 16 3187.916    0.0000 0.9311 0.9311 -1577.958
            # CropProp_TreeProp 16 3193.210    5.2944 0.0660 0.9971 -1580.605
            # HerbProp          15 3199.472   11.5561 0.0029 1.0000 -1584.736
            # TreeProp          15 3224.109   36.1931 0.0000 1.0000 -1597.055
            # Null              14 3302.077  114.1609 0.0000 1.0000 -1637.039
            # CropProp          15 3302.617  114.7008 0.0000 1.0000 -1636.308

BOBO_landscape_mod_Gr    =pcount(~Obs+DOY+Wind+Clouds ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=BOBO_PCount_Gr, mixture="ZIP",K=100)
confint(BOBO_landscape_mod_Gr, type="state", level=0.85)
            #all good

#_______________________________________

#>>Stage 3: HerbYesNo_v_GrazingTreat----

BOBO_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+DOY+Wind+Clouds  ~Herb_Prop+Tree_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+DOY+Wind+Clouds  ~Herb_Prop+Tree_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+DOY+Wind+Clouds  ~Herb_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+DOY+Wind+Clouds  ~Herb_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+DOY+Wind+Clouds  ~Herb_Prop+Tree_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}

BOBO_Abundance_mods_Gr(BOBO_PCount_Gr) 
            #
            #K      AIC Delta_AIC AICWt Cum.Wt        LL
            #HerbYesNo_v_GrazingTreat 24 3076.398    0.0000     1      1 -1514.199
            #GrazingTreat             21 3125.357   48.9584     0      1 -1541.678
            #HerbYesNo_GrazingTreat   22 3126.543   50.1447     0      1 -1541.272
            #Null                     19 3187.291  110.8930     0      1 -1574.646
            #HerbYesNo                20 3189.206  112.8073     0      1 -1574.603
BOBO_abundance_mod_Gr   =pcount(~Obs+Wind+Clouds+DOY ~Tree_Prop+Herb_Prop+HerbYesNo*GrazingTreat, data=BOBO_PCount_Gr, mixture="ZIP",K=100)
confint (BOBO_abundance_mod_Gr, type="det", level=0.85) #indicates Clouds is uninformative parameter
confint (BOBO_abundance_mod_Gr, type="state", level=0.85) #all good

#_______________________________________

#Top model, Coefs, CIs----
BOBO_Top_Gr=pcount(~Obs+DOY+Wind   ~Herb_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=BOBO_PCount_Gr, mixture="ZIP",K=100)
summary(BOBO_Top_Gr)
BOBO_Abund_coef_df_Gr=Coefficients(BOBO_Top_Gr) 
confint(BOBO_Top_Gr,type="state",level=0.85)
            #clouds seems to be a pretending variable, dropped


BOBO_Coefs_CIs_Gr=ConfidenceIntervals(BOBO_Top_Gr,BOBO_Abund_coef_df_Gr) 
print(BOBO_Coefs_CIs_Gr)
            #                                        coefs          LCL        UCL
            #lam(Int)                           -0.8576717 -1.412541714 -0.3028017
            #lam(Herb_Prop)                      2.3244265  1.757361590  2.8914915
            #lam(Tree_Prop)                     -1.9624210 -2.720020491 -1.2048216
            #lam(Yearb)                          0.3595160  0.131324082  0.5877080
            #lam(Yearc)                          0.2679332 -0.004828526  0.5406950
            #lam(Yeard)                          0.4104961  0.153265609  0.6677265
            #lam(HerbYesNoYes)                  -1.0210250 -1.645242748 -0.3968072
            #lam(GrazingTreatNone)               0.6955851  0.399681135  0.9914890
            #lam(GrazingTreatSLS)                0.8806938  0.576171506  1.1852161
            #lam(HerbYesNoYes:GrazingTreatNone)  1.6068080  0.972714952  2.2409010
            #lam(HerbYesNoYes:GrazingTreatSLS)   0.4603926 -0.204797148  1.1255824

#_______________________________________

#>>Bobolink Graph----
BOBO_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                             Herb_Prop = 0.62,
                             Tree_Prop = 0.17)


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
  scale_y_continuous(limits = c(0,7), expand = c(0, 0)) +
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


#__________________________####

#3. GRAZE_GRSP####
GRSP_PCount_Gr=unmarked_data_Gr(GRSP_Graze)

#>>Stage 1: Obs+Winds+Clouds+Robel---- 
Detection_mods(GRSP_PCount_Gr) 
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
#summary(GRSP_det_mod_Gr)
            #no pretending
#_______________________________________

#>>Stage 2: Herb_Prop----

GRSP_Landscape_mods_Gr = function(df) {
  Null                =pcount(~Obs+Wind+Clouds+Robel   ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp            =pcount(~Obs+Wind+Clouds+Robel   ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp   =pcount(~Obs+Wind+Clouds+Robel   ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp            =pcount(~Obs+Wind+Clouds+Robel   ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp   =pcount(~Obs+Wind+Clouds+Robel   ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp            =pcount(~Obs+Wind+Clouds+Robel   ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,   HerbProp,   HerbProp_TreeProp,    CropProp,   CropProp_TreeProp,    TreeProp )
  names=c( "Null", "HerbProp", "HerbProp_TreeProp",  "CropProp", "CropProp_TreeProp",  "TreeProp" )
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 
GRSP_Landscape_mods_Gr(GRSP_PCount_Gr) 
            #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
            #HerbProp          15 2166.395    0.0000 0.3997 0.3997 -1068.198
            #HerbProp_TreeProp 16 2167.393    0.9972 0.2427 0.6424 -1067.696
            #CropProp_TreeProp 16 2167.514    1.1182 0.2285 0.8709 -1067.757
            #CropProp          15 2170.029    3.6337 0.0650 0.9359 -1070.015
            #Null              14 2170.999    4.6032 0.0400 0.9759 -1071.499
            #TreeProp          15 2172.009    5.6136 0.0241 1.0000 -1071.005

GRSP_landscape_mod_Gr    =pcount(~Obs+Wind+Clouds+Robel ~Herb_Prop+offset(log(Area_ha)), data=GRSP_PCount_Gr, mixture="ZIP",K=100)
confint(GRSP_landscape_mod_Gr, type="state", level=0.85)
            #all good

#_______________________________________

#>>>Stage 3: HerbYesNo_v_GrazingTreat----
GRSP_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+Wind+Clouds+Robel   ~Herb_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+Wind+Clouds+Robel   ~Herb_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+Wind+Clouds+Robel   ~Herb_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+Wind+Clouds+Robel   ~Herb_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+Wind+Clouds+Robel   ~Herb_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(  Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  }

GRSP_Abundance_mods_Gr(GRSP_PCount_Gr) 
            #                          K      AIC Delta_AIC  AICWt Cum.Wt        LL
            #HerbYesNo_v_GrazingTreat 23 2186.814    0.0000 0.6390 0.6390 -1070.407
            #HerbYesNo_GrazingTreat   21 2189.175    2.3609 0.1963 0.8353 -1073.588
            #GrazingTreat             20 2189.525    2.7113 0.1647 1.0000 -1074.763
            #Null                     18 2239.296   52.4817 0.0000 1.0000 -1101.648
            #HerbYesNo                19 2239.527   52.7128 0.0000 1.0000 -1100.763

GRSP_abundance_mod_Gr    =pcount(~Obs+Wind+Robel ~Herb_Prop+HerbYesNo*GrazingTreat+Year+offset(log(Area_ha)), data=GRSP_PCount_Gr, mixture="ZIP",K=100)
confint(GRSP_abundance_mod_Gr, type="det", level=0.85)  #clouds pretending
confint(GRSP_abundance_mod_Gr, type="state", level=0.85)  #clouds pretending

#_______________________________________

#>>Top model, Coefs, CIs----
GRSP_Top_Gr=pcount(~Obs+Wind+Robel   ~Herb_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=GRSP_PCount_Gr, mixture="ZIP",K=100)
#summary(GRSP_Top_Gr)
GRSP_Abund_coef_df_Gr=Coefficients(GRSP_Top_Gr)
confint(GRSP_Top_Gr,type="state",level=0.85)

GRSP_Coefs_CIs_Gr=ConfidenceIntervals(GRSP_Top_Gr,GRSP_Abund_coef_df_Gr) 
print(GRSP_Coefs_CIs_Gr)
            #                                        coefs        LCL         UCL
            #lam(Int)                           -0.1251706 -0.8333417  0.58300058
            #lam(Herb_Prop)                      1.8287922  0.9761360  2.68144848
            #lam(Tree_Prop)                      1.2735660  0.4917931  2.05533893
            #lam(Yearb)                         -0.5413948 -0.7690216 -0.31376799
            #lam(Yearc)                         -0.4905062 -0.7592156 -0.22179690
            #lam(Yeard)                         -0.5474442 -0.7996482 -0.29524029
            #lam(HerbYesNoYes)                  -0.1888264 -0.4397710  0.06211815
            #lam(GrazingTreatNone)              -1.4929848 -1.8212930 -1.16467667
            #lam(GrazingTreatSLS)               -0.1957053 -0.3751114 -0.01629925
            #lam(HerbYesNoYes:GrazingTreatNone)  0.5909761  0.1053375  1.07661470
            #lam(HerbYesNoYes:GrazingTreatSLS)   0.5246242  0.1889530  0.86029545


#Grasshopper sparrow graph----
GRSP_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                             Herb_Prop = 0.62)


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
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
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
  ggtitle("Grasshopper Sparrows")  #can use this to set a main title above the plot
GRSP.plot_Gr


#__________________________####

#4. GRAZE_EAME####
EAME_PCount_Gr=unmarked_data_Gr(EAME_Graze)

#>>Stage 1: StartTime+Obs+Wind+Clouds+Robel---- 

Detection_mods(EAME_PCount_Gr)
              #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
              #Global            16 2370.120    0.0000 0.4401 0.4401 -1169.060
              #Obs_Winds_Clouds  14 2370.786    0.6664 0.3154 0.7555 -1171.393
              #Obs               12 2372.653    2.5331 0.1240 0.8795 -1174.326
              #Obs_StartTime_DOY 14 2372.739    2.6191 0.1188 0.9983 -1172.369
              #Null               4 2384.027   13.9078 0.0004 0.9988 -1188.014
              #StartTime          5 2384.459   14.3394 0.0003 0.9991 -1187.229
              #Clouds             5 2384.547   14.4276 0.0003 0.9994 -1187.274
              #Winds              5 2385.942   15.8225 0.0002 0.9996 -1187.971
              #DOY                5 2385.966   15.8465 0.0002 0.9997 -1187.983
              #StartTime_DOY      6 2386.366   16.2463 0.0001 0.9999 -1187.183
              #Winds_Clouds       6 2386.411   16.2910 0.0001 1.0000 -1187.205

EAME_det_mod_Gr=pcount(~Obs+Wind+Clouds+Robel+DOY+StartTime ~1, data=EAME_PCount_Gr, mixture="ZIP",K=100)
            confint(EAME_det_mod_Gr,type="det",level=0.85) 
            #pretending: DOY

#_______________________________________

#>>Stage 2: Herb_Prop----
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

EAME_Landscape_mods_Gr(EAME_PCount_Gr) 
            #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
            #HerbProp          16 2238.816    0.0000 0.4360 0.4360 -1103.408
            #CropProp_TreeProp 17 2239.144    0.3280 0.3701 0.8061 -1102.572
            #HerbProp_TreeProp 17 2240.581    1.7650 0.1804 0.9866 -1103.290
            #CropProp          16 2247.357    8.5409 0.0061 0.9926 -1107.678
            #TreeProp          16 2247.695    8.8794 0.0051 0.9978 -1107.848
            #Null              15 2249.388   10.5720 0.0022 1.0000 -1109.694

EAME_landscape_mod_Gr    =pcount(~Obs+Wind+Clouds+Robel ~Herb_Prop+offset(log(Area_ha)), data=EAME_PCount_Gr, mixture="ZIP",K=100)
confint(EAME_landscape_mod_Gr, type="state", level=0.85)
            #all good

#_______________________________________

#>>Stage 3: HerbYesNo_GrazingTreat----
EAME_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Year+Herb_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Year+Herb_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Year+Herb_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Year+Herb_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+StartTime+Robel+Wind+Clouds   ~Year+Herb_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
}

EAME_Abundance_mods_Gr(EAME_PCount_Gr)
          #                          K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #HerbYesNo_v_GrazingTreat 24 2206.184    0.0000 0.7002 0.7002 -1079.092
          #HerbYesNo_GrazingTreat   22 2207.903    1.7196 0.2964 0.9966 -1081.951
          #GrazingTreat             21 2216.856   10.6724 0.0034 1.0000 -1087.428
          #HerbYesNo                20 2227.554   21.3705 0.0000 1.0000 -1093.777
          #Null                     19 2231.441   25.2579 0.0000 1.0000 -1096.721

EAME_abundance_mod_Gr    =pcount(~Obs+Wind+Clouds+Robel+StartTime ~Year+Herb_Prop+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=EAME_PCount_Gr, mixture="ZIP",K=100)
confint(EAME_abundance_mod_Gr, type="det", level=0.85) #StartTime, Clouds, and Wind pretending
confint(EAME_abundance_mod_Gr, type="state", level=0.85)

#_______________________________________

##>>Top model, Coefs, CIs----

EAME_Top_Gr=pcount(~Obs+Robel   ~Herb_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=EAME_PCount_Gr, mixture="ZIP",K=100)
summary(EAME_Top_Gr)
EAME_Abund_coef_df_Gr=Coefficients(EAME_Top_Gr) 
confint(EAME_Top_Gr,type="det",level=0.85)

EAME_Coefs_CIs_Gr=ConfidenceIntervals(EAME_Top_Gr,EAME_Abund_coef_df_Gr) 
print(EAME_Coefs_CIs_Gr)

            #                                        coefs         LCL         UCL
            #lam(Int)                            0.3171720 -0.39523404  1.02957810
            #lam(Herb_Prop)                      2.4490010  1.77800520  3.11999680
            #lam(Yearb)                         -0.3257154 -0.57072097 -0.08070978
            #lam(Yearc)                         -0.6100905 -0.90812151 -0.31205950
            #lam(Yeard)                         -0.3027611 -0.57909066 -0.02643146
            #lam(HerbYesNoYes)                  -0.3217797 -0.61104145 -0.03251793
            #lam(GrazingTreatNone)              -1.0495288 -1.44004161 -0.65901593
            #lam(GrazingTreatSLS)                0.1913026  0.02046410  0.36214102
            #lam(HerbYesNoYes:GrazingTreatNone)  0.4757243  0.04288046  0.90856805
            #lam(HerbYesNoYes:GrazingTreatSLS)  -0.1575126 -0.51235986  0.19733461


#_______________________________________

#>>Meadowlark Graph----
EAME_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                            GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                            HerbYesNo = c("Yes", "No"),
                            Area_ha = 1,
                            Herb_Prop = 0.62)
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
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
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

#__________________________####

#5. GRAZE_RWBL####
RWBL_PCount_Gr=unmarked_data_Gr(RWBL_Graze) 

#>>Stage 1: Obs_StarTime_DOY_Robel---- 
Detection_mods(RWBL_PCount_Gr) 
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
  
#_______________________________________

#>>Stage 2: Herb_Prop + Tree_Prop----
RWBL_Landscape_mods_Gr = function(df) {
  Null                =pcount(~Obs+StartTime+DOY+Robel    ~offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbProp            =pcount(~Obs+StartTime+DOY+Robel    ~Herb_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbProp_TreeProp   =pcount(~Obs+StartTime+DOY+Robel    ~Herb_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp            =pcount(~Obs+StartTime+DOY+Robel    ~Crop_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  CropProp_TreeProp   =pcount(~Obs+StartTime+DOY+Robel    ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  TreeProp            =pcount(~Obs+StartTime+DOY+Robel    ~Tree_Prop+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,   HerbProp,   HerbProp_TreeProp,   CropProp,    CropProp_TreeProp,    TreeProp)  
  names=c( "Null", "HerbProp", "HerbProp_TreeProp",  "CropProp", "CropProp_TreeProp",  "TreeProp")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
  
} 

RWBL_Landscape_mods_Gr(RWBL_PCount_Gr) 
          #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #HerbProp_TreeProp 16 2865.746    0.0000 0.4895 0.4895 -1416.873
          #CropProp_TreeProp 16 2866.303    0.5567 0.3706 0.8601 -1417.151
          #CropProp          15 2869.223    3.4766 0.0861 0.9462 -1419.611
          #HerbProp          15 2870.161    4.4153 0.0538 1.0000 -1420.081
          #Null              14 2887.837   22.0913 0.0000 1.0000 -1429.919
          #TreeProp          15 2889.780   24.0344 0.0000 1.0000 -1429.890

RWBL_landscape_mod_Gr    =pcount(~Obs+StartTime+DOY+Robel ~Herb_Prop+offset(log(Area_ha)), data=RWBL_PCount_Gr, mixture="ZIP",K=100)
confint(RWBL_landscape_mod_Gr, type="state", level=0.85)
            #all good

#_______________________________________

#>>Stage 3:  HerbYesNo+GrazingTreat ----

RWBL_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+StartTime+DOY   ~Herb_Prop+Tree_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+StartTime+DOY   ~Herb_Prop+Tree_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+StartTime+DOY   ~Herb_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+StartTime+DOY   ~Herb_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+StartTime+DOY   ~Herb_Prop+Tree_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}

RWBL_Abundance_mods_Gr(RWBL_PCount_Gr)
          #                          K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #HerbYesNo_GrazingTreat   20 2863.158    0.0000 0.8462 0.8462 -1411.579
          #HerbYesNo_v_GrazingTreat 22 2866.582    3.4248 0.1527 0.9989 -1411.291
          #GrazingTreat             19 2876.459   13.3015 0.0011 1.0000 -1419.230
          #HerbYesNo                18 3048.858  185.7000 0.0000 1.0000 -1506.429
          #Null                     17 3049.474  186.3159 0.0000 1.0000 -1507.737

RWBL_abundance_mod_Gr    =pcount(~Obs+StartTime+DOY+Robel ~Herb_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=RWBL_PCount_Gr, mixture="ZIP",K=100)
confint(RWBL_abundance_mod_Gr, type="det", level=0.85) #all good
confint(RWBL_abundance_mod_Gr, type="state", level=0.85) #all good
#_______________________________________

##>>Top model, Coefs, CIs----
RWBL_Top_Gr=pcount(~Obs+StartTime+DOY+Robel  ~Herb_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=RWBL_PCount_Gr, mixture="ZIP",K=100)
summary(RWBL_Top_Gr)
RWBL_Abund_coef_df_Gr=Coefficients(RWBL_Top_Gr) 
confint(RWBL_Top_Gr,type="state",level=0.85)

RWBL_Coefs_CIs_Gr=ConfidenceIntervals(RWBL_Top_Gr,RWBL_Abund_coef_df_Gr) 
print(RWBL_Coefs_CIs_Gr)
            #                            coefs        LCL         UCL
            #lam(Int)              -0.41177681 -0.7874506 -0.03610301
            #lam(Herb_Prop)         1.02481655  0.5935306  1.45610245
            #lam(Yearb)             0.38374814  0.1577824  0.60971390
            #lam(Yearc)             0.36064051  0.1050177  0.61626333
            #lam(Yeard)             0.11062710 -0.1533701  0.37462429
            #lam(HerbYesNoYes)      0.29683875  0.1882684  0.40540908
            #lam(GrazingTreatNone)  1.09748349  0.9275800  1.26738700
            #lam(GrazingTreatSLS)   0.02549859 -0.1662561  0.21725331

#_______________________________________

#>>Red-winged blackbird graph----
RWBL_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                             Herb_Prop=0.62)


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
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
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

#__________________________####

#6. GRAZE_HESP####
HESP_PCount_Gr=unmarked_data_Gr(HESP_Graze)

#>>Stage 1: Obs+StartTime+Clouds---- 
Detection_mods(HESP_PCount_Gr) 
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

#_______________________________________

#>>Stage 2: Herb_Prop----
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

HESP_Landscape_mods_Gr(HESP_PCount_Gr) 
            #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
            #HerbProp          14 1410.636    0.0000 0.5037 0.5037 -691.3179
            #HerbProp_TreeProp 15 1412.057    1.4211 0.2475 0.7512 -691.0284
            #CropProp_TreeProp 15 1412.549    1.9130 0.1935 0.9447 -691.2744
            #TreeProp          14 1415.131    4.4950 0.0532 0.9979 -693.5653
            #Null              13 1422.432   11.7962 0.0014 0.9993 -698.2160
            #CropProp          14 1423.798   13.1619 0.0007 1.0000 -697.8988

HESP_landscape_mod_Gr    =pcount(~Obs+StartTime+Clouds ~Herb_Prop+offset(log(Area_ha)), data=HESP_PCount_Gr, mixture="ZIP",K=100)
confint(HESP_landscape_mod_Gr, type="state", level=0.85)
            #all good

#_______________________________________

#>>Stage 3:  HerbYesNo*GrazingTreat (StartTime pretending)----

HESP_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+StartTime+Clouds   ~Herb_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}

HESP_Abundance_mods_Gr(HESP_PCount_Gr)
          #                          K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #HerbYesNo_v_GrazingTreat 22 1403.841    0.0000 0.8936 0.8936 -679.9203
          #HerbYesNo_GrazingTreat   20 1409.563    5.7224 0.0511 0.9447 -684.7815
          #GrazingTreat             19 1409.667    5.8265 0.0485 0.9932 -685.8336
          #HerbYesNo                18 1414.511   10.6699 0.0043 0.9975 -689.2553
          #Null                     17 1415.588   11.7474 0.0025 1.0000 -690.7940

HESP_abundance_mod_Gr    =pcount(~Obs+StartTime+Clouds ~Year+Herb_Prop+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=HESP_PCount_Gr, mixture="ZIP",K=100)
confint(HESP_abundance_mod_Gr, type="det", level=0.85)#Clouds is pretending
confint(HESP_abundance_mod_Gr, type="state", level=0.85)#all good
#_______________________________________

##>>Top model, Coefs, CIs----

HESP_Top_Gr=pcount(~Obs+Clouds   ~Herb_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=HESP_PCount_Gr, mixture="ZIP",K=100)
summary(HESP_Top_Gr)
HESP_Abund_coef_df_Gr=Coefficients(HESP_Top_Gr) 
confint(HESP_Top_Gr,type="state",level=0.85)

HESP_Coefs_CIs_Gr=ConfidenceIntervals(HESP_Top_Gr,HESP_Abund_coef_df_Gr) 
print(HESP_Coefs_CIs_Gr)
            # lam(Int)                           -1.29404758 -2.18783336 -0.4002618
            # lam(Herb_Prop)                      2.02749741  1.13928652  2.9157083
            # lam(Yearb)                          0.32381109 -0.15526071  0.8028829
            # lam(Yearc)                          0.52814432  0.03747730  1.0188113
            # lam(Yeard)                          0.46900768 -0.03108908  0.9691044
            # lam(HerbYesNoYes)                  -2.10741433 -3.08274783 -1.1320808
            # lam(GrazingTreatNone)               0.29315966 -0.13446376  0.7207831
            # lam(GrazingTreatSLS)                0.05020956 -0.38693805  0.4873572
            # lam(HerbYesNoYes:GrazingTreatNone)  2.06026472  1.04617317  3.0743563
            # lam(HerbYesNoYes:GrazingTreatSLS)   1.92802633  0.89648840  2.9595643

#_______________________________________
#>>Henslow's Sparrow Graph----
HESP_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                             Herb_Prop=0.62)


HESP_Predicted_Gr=PredictedValues(HESP_Top_Gr,HESP_newdata_Gr)
HESP_Predicted_Gr_Sum=HESP_Predicted_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) 
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
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
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


#__________________________####

#7. GRAZE_SEWR####
SEWR_PCount_Gr=unmarked_data_Gr(SEWR_Graze)

#>>Stage 1: Obs+StartTime+Clouds+Robel+DOY ---- 
Detection_mods(SEWR_PCount_Gr) 
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

#_______________________________________

#>>Stage 2: CropProp+TreeProp----

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

SEWR_Landscape_mods_Gr(SEWR_PCount_Gr)
            #                   K      AIC Delta_AIC  AICWt Cum.Wt        LL
            #CropProp_TreeProp 17 1104.553    0.0000 0.5046 0.5046 -535.2767
            #HerbProp_TreeProp 17 1104.692    0.1386 0.4708 0.9755 -535.3460
            #TreeProp          16 1110.661    6.1073 0.0238 0.9993 -539.3303
            #HerbProp          16 1117.688   13.1352 0.0007 1.0000 -542.8442
            #Null              15 1135.147   30.5935 0.0000 1.0000 -552.5734
            #CropProp          16 1135.818   31.2642 0.0000 1.0000 -551.9087
            #
            #SEWR_landscape_mod_Gr    =pcount(~Obs+StartTime+Clouds+Robel+DOY ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=SEWR_PCount_Gr, mixture="ZIP",K=100)
            #confint(SEWR_landscape_mod_Gr, type="state", level=0.85)

SEWR_landscape_mod_Gr=pcount(~Obs+Robel+StartTime+DOY+Clouds ~Crop_Prop+Tree_Prop+offset(log(Area_ha)), data=SEWR_PCount_Gr, mixture="ZIP",K=100)
confint(SEWR_landscape_mod_Gr,type="state",level=0.85) 
            #Clouds pretending
#_______________________________________

##>>Stage 3: HerbYesNo*GrazingTreat----

SEWR_Abundance_mods_Gr = function(df) {
  Null                          =pcount(~Obs+StartTime+Robel+DOY+Clouds   ~Crop_Prop+Tree_Prop+Year+offset(log(Area_ha)), data=df, mixture="ZIP", K=100)
  HerbYesNo                     =pcount(~Obs+StartTime+Robel+DOY+Clouds   ~Crop_Prop+Tree_Prop+Year+HerbYesNo+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_GrazingTreat        =pcount(~Obs+StartTime+Robel+DOY+Clouds   ~Crop_Prop+Tree_Prop+Year+HerbYesNo+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  HerbYesNo_v_GrazingTreat      =pcount(~Obs+StartTime+Robel+DOY+Clouds   ~Crop_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  GrazingTreat                  =pcount(~Obs+StartTime+Robel+DOY+Clouds   ~Crop_Prop+Tree_Prop+Year+GrazingTreat+offset(log(Area_ha)), data=df, mixture="ZIP",K=100)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c ("Null","HerbYesNo","HerbYesNo_v_GrazingTreat","HerbYesNo_GrazingTreat","GrazingTreat")
  
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}

SEWR_Abundance_mods_Gr(SEWR_PCount_Gr)  #
          #                          K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #HerbYesNo_v_GrazingTreat 25 1039.322    0.0000 0.9490 0.9490 -494.6610
          #GrazingTreat             22 1045.795    6.4730 0.0373 0.9863 -500.8975
          #HerbYesNo_GrazingTreat   23 1047.793    8.4707 0.0137 1.0000 -500.8964
          #HerbYesNo                21 1102.962   63.6397 0.0000 1.0000 -530.4808
          #Null                     20 1103.174   63.8524 0.0000 1.0000 -531.5872
SEWR_abundance_mod_Gr=pcount(~Obs+Robel+StartTime+DOY+Clouds ~Crop_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=SEWR_PCount_Gr, mixture="ZIP",K=100)
confint(SEWR_abundance_mod_Gr,type="det",level=0.85)  #clouds pretending
confint(SEWR_abundance_mod_Gr,type="state",level=0.85) 

#_______________________________________
#>>Top model, Coefs, CIs----

SEWR_Top_Gr=pcount(~Obs+StartTime+DOY   ~Crop_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=SEWR_PCount_Gr, mixture="ZIP",K=100)
summary(SEWR_Top_Gr)
SEWR_Abund_coef_df_Gr=Coefficients(SEWR_Top_Gr)
confint(SEWR_Top_Gr,type="det",level=0.85)
#Robel pretending

SEWR_Coefs_CIs_Gr=ConfidenceIntervals(SEWR_Top_Gr,SEWR_Abund_coef_df_Gr) #>>Sedge Wren----
print(SEWR_Coefs_CIs_Gr)
            #                                        coefs           LCL         UCL
            # lam(Int)                           -0.7260336 -1.3955068006 -0.05656038
            # lam(Crop_Prop)                     -1.4997054 -2.4731834394 -0.52622741
            # lam(Tree_Prop)                     -5.3155251 -6.7672652343 -3.86378495
            # lam(Yearb)                          0.3926398 -0.0002576018  0.78553718
            # lam(Yearc)                          0.8621784  0.4669556162  1.25740115
            # lam(Yeard)                          0.1132963 -0.4078822008  0.63447475
            # lam(HerbYesNoYes)                  -1.3573495 -2.8817481866  0.16704910
            # lam(GrazingTreatNone)               1.9637050  1.4670154855  2.46039447
            # lam(GrazingTreatSLS)                0.3300041 -0.2563892415  0.91639744
            # lam(HerbYesNoYes:GrazingTreatNone)  1.6581313  0.1187430034  3.19751964
            # lam(HerbYesNoYes:GrazingTreatSLS)   0.1037138 -1.5767084788  1.78413600
#_______________________________________

#>>Sedge Wren Graph----

SEWR_newdata_Gr = data.frame(Year=c("a","a","a","a","a","a","b","b","b","b","b","b","c","c","c","c","c","c","d","d","d","d","d","d"),
                             GrazingTreat = c("None","None","SLS","SLS","IES","IES"),
                             HerbYesNo = c("Yes", "No"),
                             Area_ha = 1,
                             Crop_Prop=0.18,
                             Tree_Prop=0.17)


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
  scale_y_continuous(limits = c(0,3.5), expand = c(0, 0)) +
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

#__________________________####

#8. Graphing all####

legend_b <- get_legend(
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
  legend_b, 
  ncol = 1, 
  rel_heights = c(1, .1))
final.Graze.plot

#__________________________####
#9. Creating the summary figure----

DICK_None= DICK_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
DICK_SLS= DICK_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
DICK_IES= DICK_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
DICK_comb_IES <- cbind(Species="Dickcissel",DICK_IES[1],DICK_IES[2],round(DICK_IES[,3]/DICK_None[,3],4))
DICK_comb_SLS <- cbind(Species="Dickcissel",DICK_SLS[1],DICK_SLS[2],round(DICK_SLS[,3]/DICK_None[,3],4))

BOBO_None= BOBO_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
BOBO_SLS= BOBO_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
BOBO_IES= BOBO_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
BOBO_comb_IES <- cbind(Species="Bobolink",BOBO_IES[1],BOBO_IES[2],round(BOBO_IES[,3]/BOBO_None[,3],4))
BOBO_comb_SLS <- cbind(Species="Bobolink",BOBO_SLS[1],BOBO_SLS[2],round(BOBO_SLS[,3]/BOBO_None[,3],4))

GRSP_None= GRSP_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
GRSP_SLS= GRSP_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
GRSP_IES= GRSP_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
GRSP_comb_IES <- cbind(Species="Grasshopper Sparrow",GRSP_IES[1],GRSP_IES[2],round(GRSP_IES[,3]/GRSP_None[,3],4))
GRSP_comb_SLS <- cbind(Species="Grasshopper Sparrow",GRSP_SLS[1],GRSP_SLS[2],round(GRSP_SLS[,3]/GRSP_None[,3],4))


EAME_None= EAME_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
EAME_SLS= EAME_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
EAME_IES= EAME_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
EAME_comb_IES <- cbind(Species="Meadowlarks",EAME_IES[1],EAME_IES[2],round(EAME_IES[,3]/EAME_None[,3],4))
EAME_comb_SLS <- cbind(Species="Meadowlarks",EAME_SLS[1],EAME_SLS[2],round(EAME_SLS[,3]/EAME_None[,3],4))


RWBL_None= RWBL_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
RWBL_SLS= RWBL_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
RWBL_IES= RWBL_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
RWBL_comb_IES <- cbind(Species="Red-winged Blackbird",RWBL_IES[1],RWBL_IES[2],round(RWBL_IES[,3]/RWBL_None[,3],4))
RWBL_comb_SLS <- cbind(Species="Red-winged Blackbird",RWBL_SLS[1],RWBL_SLS[2],round(RWBL_SLS[,3]/RWBL_None[,3],4))

HESP_None= HESP_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
HESP_SLS= HESP_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
HESP_IES= HESP_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
HESP_comb_IES <- cbind(Species="Henslow's Sparrow",HESP_IES[1],HESP_IES[2],round(HESP_IES[,3]/HESP_None[,3],4))
HESP_comb_SLS <- cbind(Species="Henslow's Sparrow",HESP_SLS[1],HESP_SLS[2],round(HESP_SLS[,3]/HESP_None[,3],4))


SEWR_None= SEWR_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
SEWR_SLS= SEWR_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
SEWR_IES= SEWR_Predicted_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
SEWR_comb_IES <- cbind(Species="Sedge Wren",SEWR_IES[1],SEWR_IES[2],round(SEWR_IES[,3]/SEWR_None[,3],4))
SEWR_comb_SLS <- cbind(Species="Sedge Wren",SEWR_SLS[1],SEWR_SLS[2],round(SEWR_SLS[,3]/SEWR_None[,3],4))


#TSH=c("a","b","c","d")


All_Birds_Gr=rbind(DICK_comb_IES,DICK_comb_SLS,
                   BOBO_comb_IES,BOBO_comb_SLS,
                   EAME_comb_IES,EAME_comb_SLS,
                   RWBL_comb_IES,RWBL_comb_SLS,
                   HESP_comb_IES,HESP_comb_SLS,
                   GRSP_comb_IES,GRSP_comb_SLS,
                   SEWR_comb_IES,SEWR_comb_SLS)

#All_Birds$Bird_Treatment=factor(All_Birds$Bird_Treatment,) #in case ordering needs to be changed

# Making the heatmap 
BirdSummaryFig_Gr =ggplot(All_Birds_Gr, aes(HerbYesNo, GrazingTreat, fill= Predicted)) + 
  geom_tile()+
  scale_fill_gradientn(colours = c("darkorange3","gray95","deepskyblue3","dodgerblue4"), 
                       values = rescale(c(0,0.4,1.3,2,3)),
                       guide = "colorbar")+
  facet_wrap(Species~., strip.position="left",ncol=1)+
  scale_y_discrete(expand=c(0,0),position = "left")+
  scale_x_discrete(expand=c(0,0))+
  labs(y= "Grazing", x =  "Herbicide",fill="Grazing Treatment/\nNo Grazing")+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=10,color="black"),
        axis.line.x = element_line(color="black",size=0.5),
        axis.line.y = element_line(color="black",size=0.5,),
        legend.text=element_text(size=10, color="black"),
        strip.placement = "outside",
        legend.position = ("top"),
        legend.title = element_text(vjust=0.85,size=12,color="black"))

setwd("/cloud/project")
  
BirdSummaryFig_Gr
ggsave(filename="BirdSummaryFig_Graze.jpg", plot = BirdSummaryFig_Gr,
       scale = 1, width = 3, height = 8, units = c("in"),dpi = 300,path="Figs")



