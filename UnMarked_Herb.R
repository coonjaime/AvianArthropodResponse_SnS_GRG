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
#METADATA TO BE CONTINUED AS I THINK OF THINGS THAT SHOULD BE ADDED#

#**************************************************************************####
#SETUP.....................................................................####
#**************************************************************************####
#1. Packages----
library('unmarked')
library('ggplot2')
library('ggthemes')
library('ggppubr')
library('gridExtra')
library('AICcmodavg')
library('tidyverse') 
library('cowplot')
library('wesanderson')
library ('dplyr') #sometimes needs to be turned off
library('RColorBrewer')

#2. Overall functions----
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
} # cleaning and filtering function for n=7 dataset
Data_Cleaning_Occu= function (df) {
  df$Visit_1[df$Visit_1>0]=1
  df$Visit_2[df$Visit_2>0]=1
  df$Visit_3[df$Visit_3>0]=1
  df$Visit_4[df$Visit_4>0]=1
  df$Visit_5[df$Visit_5>0]=1
  df
} # cleaning and filtering function for occupancy modeling on n=7
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
} #setting up unmarked 'pcount' for n=7 
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
} #setting up unmarked 'occu' for n=7
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
} #set of detection models used for all species
Coefficients =function(model) {
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
PredictedValues=function(model,newdata) {
  abundance_estimates = as.data.frame(predict(model, type = "state", newdata = newdata, level=0.85,appendData = T))
  abundance_estimates
} #calculating predicted values for grazing
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
  
} # cleaning and filtering function for n=18 dataset
addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
} #adding a new level to a factor
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
}#setting up unmarked 'pcount' for n=18
dodge <- position_dodge(width=0.9) #ggplot prep for graphing


#2. Importing----

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

#How many detections in the n=7 Herb sample?
DICK_Det=sum(DICK_Herb$Visit_1)+sum(DICK_Herb$Visit_2)+sum(DICK_Herb$Visit_3)+sum(DICK_Herb$Visit_4)
BOBO_Det=sum(BOBO_Herb$Visit_1)+sum(BOBO_Herb$Visit_2)+sum(BOBO_Herb$Visit_3)+sum(BOBO_Herb$Visit_4)
GRSP_Det=sum(GRSP_Herb$Visit_1)+sum(GRSP_Herb$Visit_2)+sum(GRSP_Herb$Visit_3)+sum(GRSP_Herb$Visit_4)
EAME_Det=sum(EAME_Herb$Visit_1)+sum(EAME_Herb$Visit_2)+sum(EAME_Herb$Visit_3)+sum(EAME_Herb$Visit_4)
RWBL_Det=sum(RWBL_Herb$Visit_1)+sum(RWBL_Herb$Visit_2)+sum(RWBL_Herb$Visit_3)+sum(RWBL_Herb$Visit_4)
HESP_Det=sum(HESP_Herb$Visit_1)+sum(HESP_Herb$Visit_2)+sum(HESP_Herb$Visit_3)+sum(HESP_Herb$Visit_4) #probably need to do occu
SEWR_Det=sum(SEWR_Herb$Visit_1)+sum(SEWR_Herb$Visit_2)+sum(SEWR_Herb$Visit_3)+sum(SEWR_Herb$Visit_4) #probably need to do occu

Number_of_Detections= c(DICK_Det,BOBO_Det,GRSP_Det,EAME_Det, RWBL_Det,HESP_Det, SEWR_Det)
Det_Names= c("DICK","BOBO","GRSP","EAME","RWBL","HESP","SEWR")
Detections_Herb=as.data.frame(Number_of_Detections, Det_Names)
Detections_Herb

#Number_of_Detections
#DICK                  497
#BOBO                  525
#GRSP                  213
#EAME                  183
#RWBL                  473
#HESP                  113
#SEWR                  105

#Mean land cover by type in sample?
LandCoverMeans_Gr=matrix(c(mean(DICK_Graze$AvgOfHerbProp),mean(DICK_Graze$AvgOfCropProp),mean(DICK_Graze$AvgOfTreeProp),mean(DICK_Graze$AvgOfTree_kmHa)))

# [1,] 0.6225418 - Herb
# [2,] 0.1785696 - Crop
# [3,] 0.1749629 - Tree
# [4,] 0.7091316 - Tree kmHa

#test for correlations in covariate
myvars1 = c("Clouds_1", "Date_1_Ord", "Winds_1","AvgOfAvg_Robel","StartTime_1")
myvars2 = c("Clouds_2", "Date_2_Ord", "Winds_2","AvgOfAvg_Robel","StartTime_2")
myvars3 = c("Clouds_3", "Date_3_Ord", "Winds_3","AvgOfAvg_Robel","StartTime_3")
myvars4 = c("Clouds_4", "Date_4_Ord", "Winds_4","AvgOfAvg_Robel","StartTime_4")
myvars5 = c("Clouds_5", "Date_5_Ord", "Winds_5","AvgOfAvg_Robel","StartTime_5")

check_correlations = DICK_Herb[myvars5]
cor(check_correlations)
#********************************************************************************####
#ANALYSIS OF HERB SAMPLE (n=7)...................................................####
#********************************************************************************####
#Process for each bird: unmarked_data() prepares the dataset, Detection_mods() runs detection models, 
#X_Landscape_mods() runs landscape models, 
#1. HERB_DICK####
DICK_PCount=unmarked_data(DICK_Herb) 
Detection_mods(DICK_PCount) #............Stage 1: Obs+Starttime+DOY (no Robel)----
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
#............Stage 2: no landscape  ----
DICK_Landscape_mods(DICK_PCount) 
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
DICK_Abundance_mods(DICK_PCount) #............Stage 3: HerbTreat*TSH+GrazingTreat----
#                             K      AIC Delta_AIC  AICWt Cum.Wt        LL
# HerbTreat*TSH+GrazingTreat 26 1269.460    0.0000 0.9997 0.9997 -608.7300
# HerbTreat+GrazingTreat     17 1285.968   16.5083 0.0003 1.0000 -625.9841
# HerbTreat*TSH              24 1299.533   30.0733 0.0000 1.0000 -625.7666
# TSH+GrazingTreat           18 1308.833   39.3732 0.0000 1.0000 -636.4166
# HerbTreat                  15 1309.005   39.5446 0.0000 1.0000 -639.5023
# GrazingTreat               15 1314.918   45.4576 0.0000 1.0000 -642.4588
# TSH                        16 1335.103   65.6431 0.0000 1.0000 -651.5516
# Null                       13 1336.976   67.5164 0.0000 1.0000 -655.4882


#>>Top model, Coefs, CIs----
DICK_Top=pcount(~Obs+StartTime+DOY   ~HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=DICK_PCount, mixture="ZIP",K=100)
DICK_Abund_coef_df=Coefficients(DICK_Top) 

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

#__________________________####
#2. HERB_BOBO####
BOBO_PCount=unmarked_data(BOBO_Herb) 
Detection_mods(BOBO_PCount) #............Stage 1: Obs+DOY+Robel+Clouds---- 
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
BOBO_Landscape_mods(BOBO_PCount) #............Stage2: crop_prop+tree_prop----
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
BOBO_Abundance_mods(BOBO_PCount) #............Stage 3: HerbTreat*TSH+GrazingTreat----
#                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
# HerbTreat*TSH+GrazingTreat 30 1095.940    0.0000 0.8913 0.8913 -517.9700
# GrazingTreat               19 1101.115    5.1748 0.0670 0.9584 -531.5574
# HerbTreat+GrazingTreat     21 1102.982    7.0417 0.0264 0.9847 -530.4908
# TSH+GrazingTreat           22 1104.412    8.4723 0.0129 0.9976 -530.2062
# HerbTreat*TSH              28 1108.914   12.9744 0.0014 0.9990 -526.4572
# null                       17 1110.068   14.1278 0.0008 0.9997 -538.0339
# HerbTreat                  19 1113.120   17.1796 0.0002 0.9999 -537.5598
# TSH                        20 1114.030   18.0904 0.0001 1.0000 -537.0152


#Top model, Coefs, CIs----
BOBO_Top=pcount(~Obs+DOY+Robel+Clouds   ~Crop_Prop+Tree_Prop+HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=BOBO_PCount, mixture="ZIP",K=100)
#summary(BOBO_Top)
BOBO_Abund_coef_df=Coefficients(BOBO_Top) 
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

#__________________________####

#3. HERB_GRSP####
GRSP_PCount=unmarked_data(GRSP_Herb) 
Detection_mods(GRSP_PCount) #............Stage 1: Obs+Robel---- 
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
GRSP_Landscape_mods(GRSP_PCount) #............Stage2: no landscape----
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
GRSP_Abundance_mods(GRSP_PCount) #>............Stage 3:  null ----
#                             K      AIC Delta_AIC  AICWt Cum.Wt        LL
# null                       12 705.5563    0.0000 0.4357 0.4357 -340.7781
# HerbTreat                  14 706.5439    0.9877 0.2659 0.7016 -339.2720
# HerbTreat+GrazingTreat     16 708.0443    2.4881 0.1256 0.8271 -338.0222
# GrazingTreat               14 708.2514    2.6952 0.1132 0.9403 -340.1257
# TSH                        15 710.7486    5.1923 0.0325 0.9728 -340.3743
# HerbTreat*TSH              23 713.0159    7.4596 0.0105 0.9833 -333.5079
# TSH+GrazingTreat           17 713.3546    7.7984 0.0088 0.9921 -339.6773
# HerbTreat*TSH+GrazingTreat 25 713.5797    8.0234 0.0079 1.0000 -331.7899

#>>Top model, Coefs, CIs----

GRSP_KindaTop=pcount(~Robel+Obs+HerbTreat ~offset(log(Area_ha)), data=GRSP_PCount, mixture="ZIP",K=100)
#summary(GRSP_KindaTop)
GRSP_Abund_coef_df=Coefficients(GRSP_KindaTop) 
#Grasshopper sparrow graph----
GRSP_newdata = data.frame(HerbTreat = c("Con", "SnS", "Spr"),
                          Area_ha = 1)

GRSP_Predicted=PredictedValues(GRSP_KindaTop,GRSP_newdata)
GRSP_Predicted_Sum=GRSP_Predicted %>% 
  group_by(HerbTreat) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T) #averages out the grazing treatments so we can focus on herbicide treatments only
#View(GRSP_Predicted_Sum) 

GRSP_Predicted_Sum$HerbTreat=factor(GRSP_Predicted_Sum$HerbTreat,levels=c("Con","Spr","SnS")) #ordering

GRSP.plot <- ggplot(data = GRSP_Predicted_Sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=HerbTreat, 
               fill=HerbTreat), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  scale_y_continuous(limits = c(0,5), expand = c(0, 0)) +
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
  
  scale_x_discrete(breaks=c("Con","Spr","SnS"))+
  geom_errorbar(aes(x = HerbTreat, 
                    ymin = lower, 
                    ymax = upper,
                    group = HerbTreat), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2)+ #this is the width of the error bar ends
  labs(y = "Abundance", 
       x = "Herbicide Treatment", 
       fill = "Herbicide Treatment") + #sets label for whatever you used to "fill" the bars
  ggtitle("Grasshopper Sparrows")  #can use this to set a main title above the plot
GRSP.plot #no apparent trends!


#__________________________####

#4. HERB_EAME####
EAME_PCount=unmarked_data(EAME_Herb) 
Detection_mods(EAME_PCount) #............Stage 1: Robel---- 
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
EAME_Landscape_mods(EAME_PCount) #............Stage 2: Crop_Prop+Tree_Prop----
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
EAME_Abundance_mods(EAME_PCount) #............Stage 3: HerbTreat*TSH+GrazingTreat Kinda top----
#                              K      AIC Delta_AIC  AICWt Cum.Wt        LL
# GrazingTreat                8 726.1389    0.0000 0.4142 0.4142 -355.0694
# HerbTreat*TSH+GrazingTreat 19 726.8314    0.6925 0.2929 0.7071 -344.4157
# HerbTreat+GrazingTreat     10 729.1554    3.0165 0.0917 0.7988 -354.5777
# null                        6 729.6449    3.5060 0.0718 0.8705 -358.8225
# HerbTreat*TSH              17 729.7224    3.5835 0.0690 0.9395 -347.8612
# TSH+GrazingTreat           11 730.9845    4.8456 0.0367 0.9763 -354.4923
# HerbTreat                   8 732.4083    6.2694 0.0180 0.9943 -358.2041
# TSH                         9 734.7070    8.5681 0.0057 1.0000 -358.3535 #Probably grazing is the whole deal - but examine HerbTreat*TSH + GrazingTreat
##>>Top model, Coefs, CIs----
EAME_KindaTop=pcount(~Robel   ~Crop_Prop+Tree_Prop+HerbTreat*TSH+GrazingTreat+offset(log(Area_ha)), data=EAME_PCount, mixture="ZIP",K=100)
#summary(EAME_Top)
EAME_Abund_coef_df=Coefficients(EAME_KindaTop)   

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
#__________________________####

#5. HERB_RWBL####
RWBL_PCount=unmarked_data(RWBL_Herb) 
Detection_mods(RWBL_PCount) #............Stage 1: Obs+Robel+DOY---- 
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
RWBL_Landscape_mods(RWBL_PCount) #............Stage 2: Herb_Prop----
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
RWBL_Abundance_mods(RWBL_PCount) #............Stage 3:  HerbTreat*TSH ----
#                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
# HerbTreat*TSH              25 1142.886    0.0000 0.5171 0.5171 -546.4429
# HerbTreat*TSH+GrazingTreat 27 1143.107    0.2210 0.4630 0.9802 -544.5534
# TSH                        17 1150.625    7.7393 0.0108 0.9910 -558.3126
# TSH+GrazingTreat           19 1151.911    9.0249 0.0057 0.9967 -556.9554
# null                       14 1155.488   12.6021 0.0009 0.9976 -563.7440
# HerbTreat                  16 1155.513   12.6268 0.0009 0.9985 -561.7564
# GrazingTreat               16 1155.560   12.6739 0.0009 0.9995 -561.7799
# HerbTreat+GrazingTreat     18 1156.617   13.7312 0.0005 1.0000 -560.3085

##>>Top model, Coefs, CIs----
RWBL_Top=pcount(~Robel+Obs+DOY   ~Herb_Prop+HerbTreat*TSH+offset(log(Area_ha)), data=RWBL_PCount, mixture="ZIP",K=100)
#summary(RWBL_Top)
RWBL_Abund_coef_df=Coefficients(RWBL_Top) 
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


#__________________________####

#6. HERB_HESP####
HESP_Occu=occu_unmarked_data(HESP_Presence)
Detection_mods_occu(HESP_Occu) #............Stage 1: Clouds+Robel---- 
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
HESP_Landscape_mods(HESP_Occu)   #............Stage 2: Herb_Prop----
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
HESP_Occu_mods(HESP_Occu) #............Stage 3: HerbTreat*TSH ----
#                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
# HerbTreat*TSH              16 312.0452    0.0000 0.8258 0.8258 -140.0226
# TSH+GrazingTreat           10 316.4121    4.3669 0.0930 0.9188 -148.2060
# HerbTreat*TSH+GrazingTreat 18 317.5407    5.4955 0.0529 0.9717 -140.7703
# TSH                         8 319.2027    7.1575 0.0230 0.9948 -151.6014
# GrazingTreat                7 323.9134   11.8682 0.0022 0.9969 -154.9567
# HerbTreat+GrazingTreat      9 324.7219   12.6767 0.0015 0.9984 -153.3610
# null                        5 325.6404   13.5952 0.0009 0.9993 -157.8202
# HerbTreat                   7 326.2648   14.2196 0.0007 1.0000 -156.1324

##>>Top model, Coefs, CIs----
HESP_Top=occu(~Robel+Clouds   ~Herb_Prop+HerbTreat*TSH+GrazingTreat, data=HESP_Occu)
#summary(HESP_Top)
HESP_Abund_coef_df=Coefficients(HESP_Top) 
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

#__________________________####

#7. HERB_SEWR####
SEWR_Occu=occu_unmarked_data(SEWR_Presence)
Detection_mods_occu(SEWR_Occu) #............Stage 1: Obs+StartTime+DOY+Robel---- 
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
SEWR_Landscape_mods(SEWR_Occu)   #............Stage 2: Tree_kmha----
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
SEWR_Occu_mods(SEWR_Occu) ##............Stage 3: HerbTreat+GrazingTreat (kind of)----
#                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
# GrazingTreat               16 244.0548    0.0000 0.5042 0.5042 -106.0274
# HerbTreat+GrazingTreat     18 244.3886    0.3338 0.4267 0.9309 -104.1943
# TSH+GrazingTreat           19 248.1710    4.1162 0.0644 0.9953 -105.0855
# HerbTreat*TSH+GrazingTreat 27 253.8059    9.7510 0.0038 0.9991  -99.9029
# HerbTreat                  16 257.6437   13.5889 0.0006 0.9997 -112.8219
# null                       14 258.8910   14.8361 0.0003 1.0000 -115.4455
# TSH                        17 263.9447   19.8899 0.0000 1.0000 -114.9724
# HerbTreat*TSH              25 267.2178   23.1630 0.0000 1.0000 -108.6089
#>>Top model, Coefs, CIs----
SEWR_Top=occu(~Obs+StartTime+DOY+Robel   ~Tree_kmha+HerbTreat+GrazingTreat+offset(log(Area_ha)), data=SEWR_Occu)
#summary(SEWR_Top)
SEWR_Abund_coef_df=Coefficients(SEWR_Top)
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




#__________________________####

#8. Graphing all####

#Extracting the legend
legend_a <- get_legend(
  DICK.plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

#putting it together withing the legend
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

ggsave(filename="Avian_Herb_Result.jpg", plot = final.Herb.plot,
       scale = 1, width = 6.5, height = 9, units = c("in"),dpi = 300)
#__________________________####
#9. Creating the summary figure----

library("scales")

#extracting the data for the heatmap
DICK_Con= DICK_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Con")
DICK_SnS= DICK_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="SnS") 
DICK_Spr= DICK_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Spr")
DICK_comb <- cbind(Species="Dickcissel",Bird_Treatment="SnS",DICK_SnS[1],round(DICK_SnS[,3]/DICK_Con[,3],4))
DICK_comb_Spr <- cbind(Species="Dickcissel",Bird_Treatment="Spr",DICK_Spr[1],round(DICK_Spr[,3]/DICK_Con[,3],4))

BOBO_Con= BOBO_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Con")
BOBO_SnS= BOBO_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="SnS")
BOBO_Spr= BOBO_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Spr")
BOBO_comb <- cbind(Species="Bobolink",Bird_Treatment="SnS",BOBO_SnS[1],round(BOBO_SnS[,3]/BOBO_Con[,3],4))
BOBO_comb_Spr <- cbind(Species="Bobolink",Bird_Treatment="Spr",BOBO_Spr[1],round(BOBO_Spr[,3]/BOBO_Con[,3],4))

EAME_Con= EAME_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Con")
EAME_SnS= EAME_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="SnS")
EAME_Spr= EAME_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Spr")
EAME_comb <- cbind(Species="Meadowlarks",Bird_Treatment="SnS",EAME_SnS[1],round(EAME_SnS[,3]/EAME_Con[,3],4))
EAME_comb_Spr <- cbind(Species="Meadowlarks",Bird_Treatment="Spr",EAME_Spr[1],round(EAME_Spr[,3]/EAME_Con[,3],4))

HESP_Con= HESP_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Con")
HESP_SnS= HESP_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="SnS")
HESP_Spr= HESP_Predicted_Sum %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Spr")
HESP_comb <- cbind(Species="Henslow's\nSparrow",Bird_Treatment="SnS",HESP_SnS[1],round(HESP_SnS[,3]/HESP_Con[,3],4))
HESP_comb_Spr <- cbind(Species="Henslow's\nSparrow",Bird_Treatment="Spr",HESP_Spr[1],round(HESP_Spr[,3]/HESP_Con[,3],4))

RWBL_Con= RWBL_Predicted %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Con")
RWBL_SnS= RWBL_Predicted %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="SnS")
RWBL_Spr= RWBL_Predicted %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Spr")
RWBL_comb <- cbind(Species="Red-winged\nBlackbird",Bird_Treatment="SnS",RWBL_SnS[,5],round(RWBL_SnS[,1]/RWBL_Con[,1],4))
RWBL_comb_Spr <- cbind(Species="Red-winged\nBlackbird",Bird_Treatment="Spr",RWBL_Spr[,5],round(RWBL_Spr[,1]/RWBL_Con[,1],4))

TSH=c("a","b","c","d")

#Because GRSP top model was null, filling in all 1's since Con and Treatment are equal
GRSP_comb <- data.frame(Species="Grasshopper\nSparrow",Bird_Treatment = "SnS", 
                        TSH = TSH, 
                        Predicted = 1, stringsAsFactors = FALSE)
GRSP_comb_Spr <- data.frame(Species="Grasshopper\nSparrow",Bird_Treatment = "Spr", 
                            TSH = TSH, 
                            Predicted = 1, stringsAsFactors = FALSE)

#SEWR had no TSH component (abundances by treatment did not differ by year)
SEWR_comb <- data.frame(Species="Sedge\nWren",Bird_Treatment = "SnS", 
                        TSH = TSH, 
                        Predicted = 0.09/0.27, stringsAsFactors = FALSE)
SEWR_comb_Spr <- data.frame(Species="Sedge\nWren",Bird_Treatment = "Spr", 
                            TSH = TSH, 
                            Predicted = 0.286/0.270, stringsAsFactors = FALSE)

All_Birds=rbind(DICK_comb,DICK_comb_Spr,
                BOBO_comb,BOBO_comb_Spr,
                EAME_comb,EAME_comb_Spr,
                RWBL_comb,RWBL_comb_Spr,
                HESP_comb,HESP_comb_Spr,
                GRSP_comb,GRSP_comb_Spr,
                SEWR_comb,SEWR_comb_Spr)

#All_Birds$Bird_Treatment=factor(All_Birds$Bird_Treatment,) #in case ordering needs to be changed

# Making the heatmap 
BirdSummaryFig =ggplot(All_Birds, aes(TSH, Bird_Treatment, fill= Predicted)) + 
  geom_tile()+
  scale_fill_gradientn(colours = c("darkorange3","gray95","deepskyblue3","dodgerblue4"), 
                       values = rescale(c(0,0.4,1.3,2,3)),
                       guide = "colorbar")+
  facet_grid(Species~., space="free_x", scales="free_y", switch="y")+
  scale_y_discrete(expand=c(0,0),position = "left")+
  scale_x_discrete(expand=c(0,0),breaks=c("a","b","c","d"),labels=c("1","2","3","4"))+
  #coord_cartesian(clip="off") +
  #annotate("segment", x = 0.5, xend=0.5,y = 0.5, yend = 2.5, size=1) +
  labs(y= "Treatment", x =  "Years-Since-Treatment",fill="Treatment/Control")+
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line.x = element_line(color="black",size=0.5),
        axis.line.y = element_line(color="black",size=0.5,),
        legend.text=element_text(size=12, color="black"),
        strip.placement = "outside",
        legend.position = ("top"),
        legend.title = element_text(vjust=0.85,size=12,color="black"))
#panel.grid=element_blank()
#plot.title=element_text(hjust=0.5)
BirdSummaryFig
ggsave(filename="BirdSummaryFig.jpg", plot = BirdSummaryFig,
       scale = 1, width = 6, height = 8, units = c("in"),dpi = 300)


