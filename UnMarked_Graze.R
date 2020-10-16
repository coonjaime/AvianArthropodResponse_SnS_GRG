####METADATA####

#Attributions
#>> Code accompanies the following in review manuscript:
#Coon, J.J., S.B. Maresh Nelson, R.C. Daughtridge, W.H. Schacht, D.M. Debinski, and J.R. Miller
#Title....
#>> Code was written by J. Coon, with assistance from R. Daughtridge and T. Swartz.

#Definitions of terms
#>>HERB  n=7 sites, treated with experimental Herbicide with controls
#>>GRAZE n=18 sites, mix of experimental and non-experimental Herbicide, 
#used to assess interactions between Herbicide and grazing

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
setwd("/cloud/project/UnMarkedData")

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

check_correlations = DICK_GRAZE[myvars5]
cor(check_correlations)
#********************************************************************************####
#ANALYSIS OF GRAZE SAMPLE (n=7)...................................................####
#********************************************************************************####
#Process for each bird: unmarked_data() prepares the dataset, Detection_mods() runs detection models, 
#X_Landscape_mods() runs landscape models, 
#1. GRAZE_DICK####




#............Stage 2: no landscape  ----




#............Stage 3: GRAZETreat*TSH+GrazingTreat----





#>>Top model, Coefs, CIs----




#>>Dickcissel Graph----






#__________________________####
#2. GRAZE_BOBO####




#............Stage2: crop_prop+tree_prop----




#............Stage 3: GRAZETreat*TSH+GrazingTreat----






#Top model, Coefs, CIs----




#>>Bobolink Graph----







#__________________________####

#3. GRAZE_GRSP####



#............Stage2: no landscape----



#>............Stage 3:  null ----




#>>Top model, Coefs, CIs----




#Grasshopper sparrow graph----






#__________________________####

#4. GRAZE_EAME####


#............Stage 1: Robel---- 







#............Stage 2: Crop_Prop+Tree_Prop----











#............Stage 3: GRAZETreat*TSH+GrazingTreat Kinda top----




##>>Top model, Coefs, CIs----




#>>Meadowlark Graph----







#__________________________####

#5. GRAZE_RWBL####




#............Stage 2: GRAZE_Prop----











#............Stage 3:  GRAZETreat*TSH ----





##>>Top model, Coefs, CIs----



#>>Red-winged Blackbird Graph----








#__________________________####

#6. GRAZE_HESP####



#............Stage 2: GRAZE_Prop----





#............Stage 3: GRAZETreat*TSH ----




##>>Top model, Coefs, CIs----



#>>Henslow's Sparrow Graph----







#__________________________####

#7. GRAZE_SEWR####


#............Stage 1: Obs+StartTime+DOY+Robel---- 




#............Stage 2: Tree_kmha----
##............Stage 3: GRAZETreat+GrazingTreat (kind of)----



#>>Top model, Coefs, CIs----



#>>Sedge Wren Graph----







#__________________________####

#8. Graphing all####

#Extracting the legend



#putting it together with the legend



#__________________________####
#9. Creating the summary figure----



