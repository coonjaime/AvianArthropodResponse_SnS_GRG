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

#....................................................................................#####
#1. Packages & Functions----
#....................................................................................#
#Load libraries----
library(AICcmodavg)
library(ggplot2)
library(tidyverse)
library(glmmTMB)
library(ggeffects)

#>>Functions etc----

Data_Cleaning_Gr = function (df) {
  
  #setting NAs from visits to 0
  df = mutate(df, Temperature=case_when(
    Temperature %in% 30:35 ~ 32,
    Temperature %in% 35:40 ~ 37,
    Temperature %in% 40:45 ~ 42,
    Temperature %in% 45:50 ~ 47,
    Temperature %in% 50:55 ~ 52,
    Temperature %in% 55:60 ~ 57,
    Temperature %in% 60:65 ~ 62,
    Temperature %in% 65:70 ~ 67,
    Temperature %in% 70:75 ~ 72,
    Temperature %in% 75:80 ~ 77,
    Temperature %in% 80:85 ~ 82,
    Temperature %in% 85:90 ~ 87,
    Temperature %in% 90:95 ~ 92,
    Temperature %in% 95:100 ~ 97,
    Temperature %in% 100:105 ~ 102,
    Temperature %in% 105:110 ~ 107,
    Temperature %in% 110:115 ~ 112,
    Temperature %in% 115:120 ~ 117))
  
  #replacing missing covariates to the mean
  df$Temperature        [is.na(df$Temperature)]         <- mean(df$Temperature,na.rm=T)
  df$Winds              [is.na(df$Winds)]               <- mean(df$Winds,na.rm=T)
  df$OrdinalSamplingDate[is.na(df$OrdinalSamplingDate)] <- mean(df$OrdinalSamplingDate,na.rm=T)
  df$StartTime          [is.na(df$StartTime)]           <- mean(df$StartTime,na.rm=T)
  
  #rounding and and rescaling large numbers by dividing by 100
  df$Temperature <- round((df$Temperature/100),2)
  df$OrdinalSamplingDate <- round((df$OrdinalSamplingDate/100),2)
  df$StartTime <- round(df$StartTime,4)
  
  df$TSH[df$TSH==0.5]='a'
  df$TSH[df$TSH==1.5]='b'
  df$TSH[df$TSH==2.5]='c'
  df$TSH[df$TSH==3.5]='d'
  
  df$Year[df$Year==2015]='a'
  df$Year[df$Year==2016]='b'
  df$Year[df$Year==2017]='c'
  
  df$GrazingTreat[df$GrazingTreat=='Hay']='None' #Haying is technically no grazing, so this code combines those two categories in a new column
  df$GrazingTreat[df$GrazingTreat=='IES']='IES'
  df$GrazingTreat[df$GrazingTreat=='SLS']='SLS'
  df$GrazingTreat[df$GrazingTreat=='None']='None'
  df$GrazingTreat=factor(df$GrazingTreat)
  
  df
  df%>%
    filter(!(is.na(Weight_mg)))
}

AICregress_Nuisance_mods <- function(y,df,f) { #NB = negative binomial
  Null                   = glmmTMB(y~1+                                                       (1|Pasture),REML="FALSE", family=f, data=df)
  SweepVac               = glmmTMB(y~SweepVac+                                                (1|Pasture),REML="FALSE", family=f, data=df)
  Winds                  = glmmTMB(y~Winds+                                                   (1|Pasture),REML="FALSE", family=f, data=df)
  OrdDate                = glmmTMB(y~OrdinalSamplingDate+                                     (1|Pasture),REML="FALSE", family=f, data=df)
  Temp                   = glmmTMB(y~Temperature+                                             (1|Pasture),REML="FALSE", family=f, data=df)
  StartTime              = glmmTMB(y~StartTime+                                               (1|Pasture),REML="FALSE", family=f, data=df)
  SamplingCond           = glmmTMB(y~Winds+Temperature+StartTime+                             (1|Pasture),REML="FALSE", family=f, data=df)
  SamplingCond_OrdDate   = glmmTMB(y~OrdinalSamplingDate+StartTime+Winds+Temperature+         (1|Pasture),REML="FALSE", family=f, data=df)
  SamplingCond_Method    = glmmTMB(y~Winds+Temperature+StartTime+SweepVac+                    (1|Pasture),REML="FALSE", family=f, data=df)
  OrdDate_Method         = glmmTMB(y~OrdinalSamplingDate+SweepVac+                            (1|Pasture),REML="FALSE", family=f, data=df)
  Global                 = glmmTMB(y~SweepVac+Winds+OrdinalSamplingDate+Temperature+StartTime+(1|Pasture),REML="FALSE", family=f, data=df)
  
  mods=list(Null, SweepVac,  Winds,  OrdDate,  Temp,  StartTime,  SamplingCond,  SamplingCond_OrdDate,  SamplingCond_Method,  OrdDate_Method,  Global)
  names=c("Null","SweepVac","Winds","OrdDate","Temp","StartTime","SamplingCond","SamplingCond_OrdDate","SamplingCond_Method","OrdDate_Method","Global")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
}
dodge <- position_dodge(width=0.9) #(this is dumb, but not too hard to get around)

#....................................................................................#####
#2. Load Data & Biometric equations----
#....................................................................................#
setwd("/cloud/project/Arth_Biomass")
#Importing the data.
Biomass_All=read.csv(file="Biomass_16.10.2019.csv",header=T)
#View(Biomass_All)
#str(Biomass_All)


#calculating biomass, Sabo et al. 2002
#W=aL^b
#Aranea:      W = 0.05 * L ^ 2.74
#Orthoptera:  W = 0.03 * L ^ 2.55
#Coleoptera:  W = 0.04 * L ^ 2.64 (NOT USED)

#Sample et al. 1993 
#e^b(LxW)^a      b       a
#Coleoptera:     -1.857  1.296
#Lepidoptera ad: -2.607  1.457
#Lepidoptera la  -3.138  1.483
e<-exp(1)
Biomass_All$Weight_mg <- ifelse(Biomass_All$Order                             =="orth",   0.03*Biomass_All$Length^2.55,
                                ifelse(Biomass_All$Order                      =="cole_ad",e^-1.857*((Biomass_All$Length*Biomass_All$Width)^1.296),
                                       ifelse(Biomass_All$Order               =="aran",   0.05*Biomass_All$Length^2.74,
                                              ifelse(Biomass_All$Order        =="lepi_ad",e^-2.607*(Biomass_All$Length*Biomass_All$Width)^1.457,
                                                     ifelse(Biomass_All$Order =="lepi_la",e^-3.138*(Biomass_All$Length*Biomass_All$Width)^1.483,
                                                            NA  )))))


IndivBiomass_Gr=Data_Cleaning_Gr(Biomass_All)
IndivBiomass_Gr$Year=as.factor(IndivBiomass_Gr$Year)

#getting just orthopteran Weight_mg
Indiv_Orth_Gr=IndivBiomass_Gr%>%
  filter(Order=="orth")

Indiv_Aran_Gr=IndivBiomass_Gr%>%
  filter(Order=="aran")

Indiv_Cole_Gr=IndivBiomass_Gr%>%
  filter(Order=="cole_ad")

Indiv_Lepi_La_Gr=IndivBiomass_Gr%>%
  filter(Order=="lepi_la")

Indiv_Lepi_Ad_Gr=IndivBiomass_Gr%>%
  filter(Order=="lepi_ad")

summary(Indiv_Orth_Gr$Weight_mg)
summary(Indiv_Aran_Gr$Weight_mg)
summary(Indiv_Cole_Gr$Weight_mg)
summary(Indiv_Lepi_La_Gr$Weight_mg)
summary(Indiv_Lepi_Ad_Gr$Weight_mg)

#save(Indiv_Orth_Gr, file="Indiv_Orth_Gr.Rdata")
#load("Indiv_Orth.Rdata")
#save(Indiv_Aran_Gr, file="Indiv_Aran_Gr.Rdata")
#save(Indiv_Cole_Gr, file="Indiv_Cole_Gr.Rdata")
#save(Indiv_Lepi_La_Gr, file="Indiv_Lepi_La_Gr.Rdata")
#save(Indiv_Lepi_Ad_Gr, file="Indiv_Lepi_Ad_Gr.Rdata")

#....................................................................................#####
#3. Orthopteran (Orth) Abundance ----
#....................................................................................#

#>>Stage 1: Orth, Temperature+ OrdinalSamplingDAte + Temperature + SweepVac + (wind) ----
AICregress_Nuisance_mods(Indiv_Orth_Gr$Weight_mg,Indiv_Orth_Gr,gaussian)
          #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #Global               8 73823.59    0.0000 0.9999 0.9999 -36903.80
          #OrdDate_Method       5 73841.50   17.9110 0.0001 1.0000 -36915.75
          #SamplingCond_Method  7 74163.33  339.7357 0.0000 1.0000 -37074.66
          #SweepVac             4 74182.77  359.1727 0.0000 1.0000 -37087.38
          #SamplingCond_OrdDate 7 74184.93  361.3380 0.0000 1.0000 -37085.47
          #OrdDate              4 74205.95  382.3518 0.0000 1.0000 -37098.97
          #SamplingCond         6 74555.92  732.3260 0.0000 1.0000 -37271.96
          #StartTime            4 74560.54  736.9506 0.0000 1.0000 -37276.27
          #Winds                4 74574.47  750.8726 0.0000 1.0000 -37283.23
          #Temp                 4 74576.02  752.4216 0.0000 1.0000 -37284.01
          #Null                 3 74577.51  753.9182 0.0000 1.0000 -37285.76

Orth_Nuisance_mod_Gr = glmmTMB(Weight_mg~ Temperature+SweepVac+StartTime+OrdinalSamplingDate+Winds+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Orth_Gr)
confint(Orth_Nuisance_mod_Gr,level=0.85) #winds pretending

#>>Stage 2: HerbYesNo*Grazing, but null is very much competitive----
Orth_Graze_mods=function(y,df) { 
  Null                       = glmmTMB(y~Year+Temperature+SweepVac+OrdinalSamplingDate+StartTime+                               (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo                  = glmmTMB(y~Year+Temperature+SweepVac+OrdinalSamplingDate+StartTime+HerbYesNo_alltime+             (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo_v_Grazing        = glmmTMB(y~Year+Temperature+SweepVac+OrdinalSamplingDate+StartTime+HerbYesNo_alltime*GrazingTreat+(1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo_GrazingTreat     = glmmTMB(y~Year+Temperature+SweepVac+OrdinalSamplingDate+StartTime+HerbYesNo_alltime+GrazingTreat+(1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat               = glmmTMB(y~Year+Temperature+SweepVac+OrdinalSamplingDate+StartTime+GrazingTreat+                  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_Grazing,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*Grazing",  "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Orth_Graze_mods(Indiv_Orth_Gr$Weight_mg,Indiv_Orth_Gr)
          #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #HerbYesNo*Grazing      14 73772.83    0.0000 0.2757 0.2757 -36872.42
          #Null                    9 73772.99    0.1561 0.2550 0.5307 -36877.50
          #HerbYesNo              10 73773.13    0.2981 0.2375 0.7683 -36876.57
          #HerbYesNo+GrazingTreat 12 73774.47    1.6367 0.1216 0.8899 -36875.24
          #GrazingTreat           11 73774.67    1.8359 0.1101 1.0000 -36876.33

#Top Model & Predicted Values #####
Orth_Graze_top  = glmmTMB(Weight_mg~Year+Temperature+SweepVac+OrdinalSamplingDate+StartTime+HerbYesNo_alltime*GrazingTreat+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Orth_Gr)
confint(Orth_Graze_top) #Herb pretending

#....................................................................................#####
#4. Araneae (Aran) Biomass ----
#....................................................................................#

#>>Stage 1: Aran, OrdinalSamplingDate+SweepVac----
AICregress_Nuisance_mods(Indiv_Aran_Gr$Weight_mg,Indiv_Aran_Gr,gaussian)
          #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #OrdDate_Method       5 28741.62    0.0000 0.9093 0.9093 -14365.81
          #Global               8 28746.23    4.6096 0.0907 1.0000 -14365.12
          #SweepVac             4 28785.57   43.9517 0.0000 1.0000 -14388.79
          #SamplingCond_Method  7 28787.49   45.8656 0.0000 1.0000 -14386.74
          #OrdDate              4 28836.60   94.9794 0.0000 1.0000 -14414.30
          #SamplingCond_OrdDate 7 28841.10   99.4804 0.0000 1.0000 -14413.55
          #Winds                4 28876.29  134.6720 0.0000 1.0000 -14434.15
          #Null                 3 28877.01  135.3939 0.0000 1.0000 -14435.51
          #Temp                 4 28878.59  136.9717 0.0000 1.0000 -14435.30
          #StartTime            4 28879.00  137.3803 0.0000 1.0000 -14435.50
          #SamplingCond         6 28880.04  138.4205 0.0000 1.0000 -14434.02

Aran_Nuisance_mod_Gr = glmmTMB(Weight_mg~OrdinalSamplingDate+SweepVac+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Aran_Gr)
confint(Aran_Nuisance_mod_Gr,level=0.85)
summary(Aran_Nuisance_mod_Gr)


#>>Stage 2: Aran, HerbYesNo----
Aran_Graze_mods=function(y,df) { 
  Null                       = glmmTMB(y~Year+OrdinalSamplingDate+SweepVac+                                   (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo                  = glmmTMB(y~Year+OrdinalSamplingDate+SweepVac+HerbYesNo_alltime+                 (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo_v_GrazingTreat   = glmmTMB(y~Year+OrdinalSamplingDate+SweepVac+HerbYesNo_alltime*GrazingTreat+    (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo_GrazingTreat     = glmmTMB(y~Year+OrdinalSamplingDate+SweepVac+HerbYesNo_alltime+GrazingTreat+    (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat               = glmmTMB(y~Year+OrdinalSamplingDate+SweepVac+GrazingTreat+                      (1|Pasture),REML="FALSE", family=gaussian, data=df)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*Grazing",  "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Aran_Graze_mods(Indiv_Aran_Gr$Weight_mg,Indiv_Aran_Gr) 
        #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
        #HerbYesNo               8 28734.02    0.0000 0.5967 0.5967 -14359.01
        #HerbYesNo+GrazingTreat 10 28735.30    1.2757 0.3153 0.9120 -14357.65
        #HerbYesNo*Grazing      12 28737.98    3.9562 0.0825 0.9945 -14356.99
        #Null                    7 28744.23   10.2069 0.0036 0.9981 -14365.11
        #GrazingTreat            9 28745.55   11.5239 0.0019 1.0000 -14363.77


#Top Model & Predicted Values----

####Should graph this out####
#....................................................................................#####
#5. Coleopteran Biomass ----
#....................................................................................#

#>>Stage 1: Cole, Winds+Method+(StartTime)+(Temp)----
AICregress_Nuisance_mods(Indiv_Cole_Gr$Weight_mg,Indiv_Cole_Gr,gaussian)
        #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
        #SamplingCond_Method  7 22416.41    0.0000 0.4222 0.4222 -11201.21
        #Global               8 22416.84    0.4307 0.3404 0.7626 -11200.42
        #OrdDate_Method       5 22418.71    2.3017 0.1336 0.8962 -11204.36
        #SweepVac             4 22419.22    2.8053 0.1038 1.0000 -11205.61
        #Winds                4 22532.98  116.5737 0.0000 1.0000 -11262.49
        #SamplingCond         6 22536.65  120.2402 0.0000 1.0000 -11262.33
        #SamplingCond_OrdDate 7 22537.48  121.0681 0.0000 1.0000 -11261.74
        #OrdDate              4 22540.26  123.8454 0.0000 1.0000 -11266.13
        #Null                 3 22540.79  124.3765 0.0000 1.0000 -11267.39
        #StartTime            4 22542.76  126.3470 0.0000 1.0000 -11267.38
        #Temp                 4 22542.76  126.3529 0.0000 1.0000 -11267.38

Cole_Nuisance_mod_Gr = glmmTMB(Weight_mg~ StartTime+Winds+Temperature+SweepVac+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Cole_Gr)
confint(Cole_Nuisance_mod_Gr,level=0.85)
summary(Cole_Nuisance_mod_Gr) 

#>>Stage 2: Cole, HerbYesNo, null competitive-----
Cole_Graze_mods=function(y,df) { 
  Null                       = glmmTMB(y~Year+SweepVac+Winds+                                 (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo                  = glmmTMB(y~Year+SweepVac+Winds+HerbYesNo_alltime+               (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo_v_GrazingTreat   = glmmTMB(y~Year+SweepVac+Winds+HerbYesNo_alltime*GrazingTreat+  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo_GrazingTreat     = glmmTMB(y~Year+SweepVac+Winds+HerbYesNo_alltime+GrazingTreat+  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat               = glmmTMB(y~Year+SweepVac+Winds+GrazingTreat+                    (1|Pasture),REML="FALSE", family=gaussian, data=df)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*Grazing", "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

####This changed, check table####
Cole_Graze_mods(Indiv_Cole_Gr$Weight_mg,Indiv_Cole_Gr) 
          #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #Null                    7 22373.64    0.0000 0.5257 0.5257 -11179.82
          #HerbYesNo               8 22375.46    1.8217 0.2114 0.7371 -11179.73
          #GrazingTreat            9 22375.84    2.2054 0.1745 0.9116 -11178.92
          #HerbYesNo+GrazingTreat 10 22377.70    4.0637 0.0689 0.9806 -11178.85
          #HerbYesNo*Grazing      12 22380.23    6.5952 0.0194 1.0000 -11178.12

#....................................................................................#####
#6. Lepidopteran (Lepi) Larval Biomass ----
#....................................................................................#

#>>Stage 1: Lepi_La, SweepVac----
AICregress_Nuisance_mods(Indiv_Lepi_La_Gr$Weight_mg,Indiv_Lepi_La_Gr,gaussian)
            #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
            #SweepVac             4 11358.57    0.0000 0.4360 0.4360 -5675.284
            #OrdDate_Method       5 11358.84    0.2692 0.3811 0.8172 -5674.419
            #SamplingCond_Method  7 11361.66    3.0908 0.0930 0.9102 -5673.829
            #Global               8 11361.83    3.2657 0.0852 0.9953 -5672.917
            #StartTime            4 11370.06   11.4963 0.0014 0.9967 -5681.032
            #Null                 3 11370.24   11.6726 0.0013 0.9980 -5682.120
            #OrdDate              4 11371.84   13.2734 0.0006 0.9986 -5681.921
            #Temp                 4 11372.01   13.4419 0.0005 0.9991 -5682.005
            #Winds                4 11372.10   13.5354 0.0005 0.9996 -5682.052
            #SamplingCond         6 11373.35   14.7856 0.0003 0.9999 -5680.677
            #SamplingCond_OrdDate 7 11374.87   16.3029 0.0001 1.0000 -5680.435

Lepi_La_Nuisance_mod_Gr = glmmTMB(Weight_mg~SweepVac+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Lepi_La_Gr)
confint(Lepi_La_Nuisance_mod_Gr,level=0.85)
summary(Lepi_La_Nuisance_mod_Gr)

#>>Stage 2: Lepi_La, ----
Lepi_La_Graze_mods=function(y,df) { 
  Null                        = glmmTMB(y~Year+SweepVac+                                 (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo                   = glmmTMB(y~Year+SweepVac+HerbYesNo_alltime+               (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo_v_GrazingTreat    = glmmTMB(y~Year+SweepVac+HerbYesNo_alltime*GrazingTreat+  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo_GrazingTreat      = glmmTMB(y~Year+SweepVac+HerbYesNo_alltime+GrazingTreat+  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat                = glmmTMB(y~Year+SweepVac+GrazingTreat+                    (1|Pasture),REML="FALSE", family=gaussian, data=df)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*Grazing", "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Lepi_La_Graze_mods(Indiv_Lepi_La_Gr$Weight_mg,Indiv_Lepi_La_Gr) 
        #                         K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # HerbYesNo               7 6709.176    0.0000 0.4257 0.4257 -3347.588
        # HerbYesNo+GrazingTreat  9 6709.754    0.5776 0.3190 0.7447 -3345.877
        # Null                    6 6711.737    2.5607 0.1183 0.8630 -3349.869
        # GrazingTreat            8 6712.357    3.1810 0.0868 0.9498 -3348.179
        # HerbYesNo*Grazing      11 6713.452    4.2754 0.0502 1.0000 -3345.726

#....................................................................................#####
#7. Lepidopteran (Lepi) Adult Biomass ----
#....................................................................................#

#>>Stage 1: Lepi_Ad, SweepVac+OrdinalSamplingDate+(Winds)+StartTime+(Temp)----
AICregress_Nuisance_mods(Indiv_Lepi_Ad_Gr$Weight_mg,Indiv_Lepi_Ad_Gr,gaussian)
          #                       K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #  Global               8 6489.503    0.0000 0.4646 0.4646 -3236.751
          #  SamplingCond_OrdDate 7 6491.034    1.5314 0.2160 0.6807 -3238.517
          #  OrdDate_Method       5 6492.060    2.5566 0.1294 0.8101 -3241.030
          #  OrdDate              4 6493.387    3.8835 0.0666 0.8767 -3242.693
          #  StartTime            4 6493.450    3.9469 0.0646 0.9413 -3242.725
          #  SamplingCond_Method  7 6494.946    5.4431 0.0306 0.9718 -3240.473
          #  SamplingCond         6 6496.972    7.4688 0.0111 0.9829 -3242.486
          #  SweepVac             4 6497.223    7.7196 0.0098 0.9927 -3244.611
          #  Null                 3 6499.050    9.5469 0.0039 0.9967 -3246.525
          #  Winds                4 6500.518   11.0145 0.0019 0.9986 -3246.259
          #  Temp                 4 6501.043   11.5397 0.0014 1.0000 -3246.521

Lepi_Ad_Nuisance_mod_Gr = glmmTMB(Weight_mg~SweepVac+OrdinalSamplingDate+Winds+Temperature+StartTime+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Lepi_Ad_Gr)
confint(Lepi_Ad_Nuisance_mod_Gr,level=0.85)
summary(Lepi_Ad_Nuisance_mod_Gr) #Temp pretending

#>>Stage 2: Lepi_Ad, GrazingTreat+TSH---- #CHANGED THIS, CHECK TABLE####

Lepi_Ad_Graze_mods=function(y,df) { 
  Null                        = glmmTMB(y~Year+StartTime+SweepVac+OrdinalSamplingDate+                                 (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo                   = glmmTMB(y~Year+StartTime+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime+               (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo_v_GrazingTreat    = glmmTMB(y~Year+StartTime+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime*GrazingTreat+  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbYesNo_GrazingTreat      = glmmTMB(y~Year+StartTime+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime+GrazingTreat+  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat                = glmmTMB(y~Year+StartTime+SweepVac+OrdinalSamplingDate+GrazingTreat+                    (1|Pasture),REML="FALSE", family=gaussian, data=df)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*Grazing", "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

#>>Lepi: HerbYesNo----
Lepi_Ad_Graze_mods(Indiv_Lepi_Ad_Gr$Weight_mg,Indiv_Lepi_Ad_Gr) 
          #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #HerbYesNo               9 6484.392    0.0000 0.5159 0.5159 -3233.196
          #Null                    8 6485.462    1.0698 0.3022 0.8181 -3234.731
          #HerbYesNo+GrazingTreat 11 6487.529    3.1362 0.1075 0.9257 -3232.764
          #GrazingTreat           10 6488.912    4.5191 0.0539 0.9795 -3234.456
          #HerbYesNo*Grazing      13 6490.846    6.4538 0.0205 1.0000 -3232.423

Lepi_Ad_Top_Gr = glmmTMB(Weight_mg~HerbYesNo_alltime+Year+SweepVac+OrdinalSamplingDate+StartTime+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Lepi_Ad_Gr)
confint(Lepi_Ad_Top_Gr,level=0.85)
summary(Lepi_Ad_Nuisance_mod_Gr)