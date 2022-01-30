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

theme_bar_SnS_leg <- function () { 
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=8, color="black"),
        legend.title=element_text(size=8, color="black",face="bold"),
        panel.grid=element_blank(),
        legend.background=element_blank(),
        plot.title=element_text(size=11, face="bold",hjust=0))
}

Data_Cleaning = function (df) {
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
    filter(!(Past_PatTrans_Year=="STE_N2_2016"))%>%
    filter(!(Past_PatTrans_Year=="STE_N2_2017"))%>%
    filter(!(Past_PatTrans_Year=="STE_N2_2018"))%>%
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


#filter(!(Pasture=="STE")) ##for use if we want to completely filter STE out. 

IndivBiomass_Herb=Data_Cleaning(Biomass_All)
IndivBiomass_Herb$Year=as.factor(IndivBiomass_Herb$Year)
#view(IndivBiomass_Herb)

#getting just orthopteran Weight_mg
Indiv_Orth=IndivBiomass_Herb%>%
  filter(Order=="orth")

Indiv_Aran=IndivBiomass_Herb%>%
  filter(Order=="aran")

Indiv_Cole=IndivBiomass_Herb%>%
  filter(Order=="cole_ad")

Indiv_Lepi_La=IndivBiomass_Herb%>%
  filter(Order=="lepi_la")

Indiv_Lepi_Ad=IndivBiomass_Herb%>%
  filter(Order=="lepi_ad")

summary(Indiv_Orth$Weight_mg)
summary(Indiv_Aran$Weight_mg)
summary(Indiv_Cole$Weight_mg)
summary(Indiv_Lepi_La$Weight_mg)
summary(Indiv_Lepi_Ad$Weight_mg)

#saving data
#save(Indiv_Orth, file="Indiv_Orth.Rdata")
#load("Indiv_Orth.Rdata")
#save(Indiv_Aran, file="Indiv_Aran.Rdata")
#save(Indiv_Cole, file="Indiv_Cole.Rdata")
#save(Indiv_Lepi_La, file="Indiv_Lepi_La.Rdata")
#save(Indiv_Lepi_Ad, file="Indiv_Lepi_Ad.Rdata")

#....................................................................................#####
#3. Orthopteran (Orth) Abundance ----
#....................................................................................#

#>>Stage 1: Orth, OrdinalSamplingDate+SweepVac+Winds+(Temp)+(StartTime) ----
AICregress_Nuisance_mods(Indiv_Orth$Weight_mg,Indiv_Orth,gaussian)
          #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #Global               8 26428.66    0.0000 0.7729 0.7729 -13206.33
          #OrdDate_Method       5 26431.11    2.4495 0.2271 1.0000 -13210.56
          #SamplingCond_Method  7 26570.76  142.0984 0.0000 1.0000 -13278.38
          #SweepVac             4 26571.65  142.9907 0.0000 1.0000 -13281.83
          #SamplingCond_OrdDate 7 26629.01  200.3525 0.0000 1.0000 -13307.51
          #OrdDate              4 26630.21  201.5451 0.0000 1.0000 -13311.10
          #Winds                4 26769.52  340.8609 0.0000 1.0000 -13380.76
          #SamplingCond         6 26772.21  343.5451 0.0000 1.0000 -13380.10
          #Null                 3 26775.30  346.6439 0.0000 1.0000 -13384.65
          #Temp                 4 26775.64  346.9837 0.0000 1.0000 -13383.82
          #StartTime            4 26776.86  348.1967 0.0000 1.0000 -13384.43

Orth_Nuisance_mod = glmmTMB(Weight_mg~SweepVac+OrdinalSamplingDate+Temperature+StartTime+Winds+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Orth)
confint(Orth_Nuisance_mod,level=0.85) #temperature pretending, starttime

#>>Stage 2: Orth, TSH----
Orth_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~OrdinalSamplingDate+SweepVac+Winds+                                   (1|Pasture),REML="FALSE", family=gaussian, data=df)
  TSH                        = glmmTMB(y~OrdinalSamplingDate+SweepVac+Winds+TSH+                               (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat                  = glmmTMB(y~OrdinalSamplingDate+SweepVac+Winds+HerbTreat_14.18+                   (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat               = glmmTMB(y~OrdinalSamplingDate+SweepVac+Winds+GrazingTreat+                      (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_TSH              = glmmTMB(y~OrdinalSamplingDate+SweepVac+Winds+HerbTreat_14.18*TSH+               (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat_TSH           = glmmTMB(y~OrdinalSamplingDate+SweepVac+Winds+GrazingTreat+TSH+                  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~OrdinalSamplingDate+SweepVac+Winds+GrazingTreat+HerbTreat_14.18+      (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~OrdinalSamplingDate+SweepVac+Winds+HerbTreat_14.18*TSH+GrazingTreat+  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Orth_Herb_mods(Indiv_Orth$Weight_mg,Indiv_Orth)
          #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #TSH                         8 23992.89    0.0000 0.5831 0.5831 -11988.44
          #GrazingTreat_TSH           10 23995.09    2.2021 0.1939 0.7771 -11987.54
          #HerbTreat_TSH              14 23995.37    2.4854 0.1683 0.9454 -11983.69
          #HerbTreat_TSH_GrazingTreat 16 23997.62    4.7351 0.0546 1.0000 -11982.81
          #Null                        6 26427.17 2434.2863 0.0000 1.0000 -13207.59
          #HerbTreat                   8 26427.40 2434.5160 0.0000 1.0000 -13205.70
          #GrazingTreat                8 26429.75 2436.8637 0.0000 1.0000 -13206.88
          #HerbTreat_GrazingTreat     10 26430.06 2437.1688 0.0000 1.0000 -13205.03

#>>Top Model & Predicted Values----

#....................................................................................#####
#4. Araneae (Aran) Abundance ----
#....................................................................................#

#>>Stage 1: Aran,  OrdinalSamplingDate+SweepVac----
AICregress_Nuisance_mods(Indiv_Aran$Weight_mg,Indiv_Aran,gaussian)

          #                      K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # OrdDate_Method       5 11117.50    0.0000 0.8571 0.8571 -5553.750
          # Global               8 11121.08    3.5841 0.1428 0.9999 -5552.542
          # SweepVac             4 11137.13   19.6327 0.0000 1.0000 -5564.566
          # SamplingCond_Method  7 11138.12   20.6194 0.0000 1.0000 -5562.060
          # OrdDate              4 11158.56   41.0566 0.0000 1.0000 -5575.279
          # SamplingCond_OrdDate 7 11162.23   44.7280 0.0000 1.0000 -5574.114
          # StartTime            4 11174.38   56.8826 0.0000 1.0000 -5583.191
          # Null                 3 11177.37   59.8733 0.0000 1.0000 -5585.687
          # SamplingCond         6 11178.22   60.7210 0.0000 1.0000 -5583.111
          # Temp                 4 11179.23   61.7307 0.0000 1.0000 -5585.615
          # Winds                4 11179.37   61.8722 0.0000 1.0000 -5585.686

Aran_Nuisance_mod = glmmTMB(Weight_mg~SweepVac+OrdinalSamplingDate+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Aran)
confint(Aran_Nuisance_mod,level=0.85)
summary(Aran_Nuisance_mod)


#>>Stage 2: Aran, TSH----
Aran_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~OrdinalSamplingDate+SweepVac+                                   (1|Pasture),REML="FALSE", family=gaussian, data=df)
  TSH                        = glmmTMB(y~OrdinalSamplingDate+SweepVac+TSH+                               (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat                  = glmmTMB(y~OrdinalSamplingDate+SweepVac+HerbTreat_14.18+                   (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat               = glmmTMB(y~OrdinalSamplingDate+SweepVac+GrazingTreat+                      (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_TSH              = glmmTMB(y~OrdinalSamplingDate+SweepVac+HerbTreat_14.18*TSH+               (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat_TSH           = glmmTMB(y~OrdinalSamplingDate+SweepVac+GrazingTreat+TSH+                  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~OrdinalSamplingDate+SweepVac+GrazingTreat+HerbTreat_14.18+      (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~OrdinalSamplingDate+SweepVac+HerbTreat_14.18*TSH+GrazingTreat+  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Aran_Herb_mods(Indiv_Aran$Weight_mg,Indiv_Aran) 
          #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #HerbTreat_TSH              13 10152.17    0.0000 0.8177 0.8177 -5063.086
          #HerbTreat_TSH_GrazingTreat 15 10155.19    3.0227 0.1804 0.9981 -5062.597
          #TSH                         7 10164.76   12.5850 0.0015 0.9996 -5075.378
          #GrazingTreat_TSH            9 10167.33   15.1535 0.0004 1.0000 -5074.663
          #HerbTreat                   7 11108.72  956.5510 0.0000 1.0000 -5547.361
          #HerbTreat_GrazingTreat      9 11112.58  960.4111 0.0000 1.0000 -5547.291
          #Null                        5 11117.50  965.3287 0.0000 1.0000 -5553.750
          #GrazingTreat                7 11120.98  968.8133 0.0000 1.0000 -5553.492

#Top Model & Predicted Values----
Aran_Top = glmmTMB(Weight_mg~OrdinalSamplingDate+SweepVac+HerbTreat_14.18*TSH+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Aran)
ggpredict(Aran_Top,c("HerbTreat_14.18", "TSH","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Aran_Pred = as.data.frame(ggpredict(Aran_Top,c("HerbTreat_14.18", "TSH","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
colnames(Aran_Pred)=c("HerbTreat", "Predicted","SE","Lower","Upper","TSH","SweepVac") #renames columns
#View(Aran_Pred) 

Aran_Pred_Sum= Aran_Pred %>% 
  group_by(TSH, HerbTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
View(Aran_Pred_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot Aran!!
Aran_Pred_Sum$HerbTreat=factor(Aran_Pred_Sum$HerbTreat,levels=c("Con","Spr","SnS"))
Aran_Pred_Sum$TSH=as.factor(Aran_Pred_Sum$TSH)
Aran_Pred_Sum$TSH=factor(Aran_Pred_Sum$TSH,levels=c("a","b","c"))

Aran_Plot_Biomass=ggplot(data=Aran_Pred_Sum, y=Weight_mg, x=TSH)+  
  geom_bar(aes(x=TSH, y=Predicted,fill=HerbTreat), position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme()+
  theme_bar_SnS_leg()+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(breaks=c("a","b","c"),labels=c("1","2","3"))+
  scale_y_continuous(limits = c(0,6), expand = c(0, 0)) +
  geom_errorbar(aes(x = TSH, ymin = Lower, ymax = Upper,group = HerbTreat),position = dodge, width = 0.2)+
  labs(y = "Average mg/Specimen", x="Years Since Herbicide", fill = "Herbicide Treatment")+ 
  ggtitle("Aran")

print(Aran_Plot_Biomass)
#....................................................................................#####
#5. Coleopteran Abundance ----
#....................................................................................#

#>>Stage 1: Cole, Winds+Method+(StartTime)+(Temp)----
AICregress_Nuisance_mods(Indiv_Cole$Weight_mg,Indiv_Cole,gaussian)
          #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #SamplingCond_Method  7 9297.211    0.0000 0.4521 0.4521 -4641.606
          #SweepVac             4 9298.466    1.2541 0.2415 0.6936 -4645.233
          #Global               8 9299.207    1.9953 0.1667 0.8603 -4641.603
          #OrdDate_Method       5 9299.560    2.3487 0.1397 1.0000 -4644.780
          #Winds                4 9344.404   47.1920 0.0000 1.0000 -4668.202
          #SamplingCond         6 9348.266   51.0545 0.0000 1.0000 -4668.133
          #Null                 3 9349.205   51.9938 0.0000 1.0000 -4671.603
          #OrdDate              4 9350.134   52.9228 0.0000 1.0000 -4671.067
          #SamplingCond_OrdDate 7 9350.266   53.0542 0.0000 1.0000 -4668.133
          #Temp                 4 9350.562   53.3505 0.0000 1.0000 -4671.281
          #StartTime            4 9351.127   53.9151 0.0000 1.0000 -4671.563

Cole_Nuisance_mod = glmmTMB(Weight_mg~StartTime+Winds+Temperature+SweepVac+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Cole)
confint(Cole_Nuisance_mod,level=0.85)
summary(Cole_Nuisance_mod) #ordinal sampling date, starttime, temperature are uninformative
#>>Stage 2: Cole, TSH----
Cole_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~SweepVac+Winds+                                  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  TSH                        = glmmTMB(y~SweepVac+Winds+TSH+                              (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat                  = glmmTMB(y~SweepVac+Winds+HerbTreat_14.18+                  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat               = glmmTMB(y~SweepVac+Winds+GrazingTreat+                     (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_TSH              = glmmTMB(y~SweepVac+Winds+HerbTreat_14.18*TSH+              (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat_TSH           = glmmTMB(y~SweepVac+Winds+GrazingTreat+TSH+                 (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~SweepVac+Winds+GrazingTreat+HerbTreat_14.18+     (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~SweepVac+Winds+HerbTreat_14.18*TSH+GrazingTreat+ (1|Pasture),REML="FALSE", family=gaussian, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Cole_Herb_mods(Indiv_Cole$Weight_mg,Indiv_Cole) 
          #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #TSH                         7 8362.514    0.0000 0.4401 0.4401 -4174.257
          #GrazingTreat_TSH            9 8363.379    0.8646 0.2857 0.7258 -4172.689
          #HerbTreat_TSH              13 8364.382    1.8683 0.1729 0.8987 -4169.191
          #HerbTreat_TSH_GrazingTreat 15 8365.452    2.9379 0.1013 1.0000 -4167.726
          #Null                        5 9293.360  930.8458 0.0000 1.0000 -4641.680
          #HerbTreat                   7 9293.541  931.0272 0.0000 1.0000 -4639.771
          #GrazingTreat                7 9294.997  932.4831 0.0000 1.0000 -4640.499
          #HerbTreat_GrazingTreat      9 9295.276  932.7624 0.0000 1.0000 -4638.638

#....................................................................................#####
#6. Lepidopteran (Lepi) Larval Abundance ----
#....................................................................................#

#>>Stage 1: Lepi_La, SweepVac----
#>>Lepi_la: Sweepvac----
AICregress_Nuisance_mods(Indiv_Lepi_La$Weight_mg,Indiv_Lepi_La,gaussian)
          #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #SweepVac             4 4385.300    0.0000 0.6039 0.6039 -2188.650
          #OrdDate_Method       5 4387.232    1.9323 0.2298 0.8338 -2188.616
          #SamplingCond_Method  7 4388.924    3.6242 0.0986 0.9324 -2187.462
          #Global               8 4390.884    5.5848 0.0370 0.9694 -2187.442
          #Null                 3 4393.650    8.3505 0.0093 0.9787 -2193.825
          #Winds                4 4393.834    8.5342 0.0085 0.9872 -2192.917
          #StartTime            4 4395.439   10.1391 0.0038 0.9909 -2193.719
          #Temp                 4 4395.511   10.2116 0.0037 0.9946 -2193.756
          #OrdDate              4 4395.604   10.3048 0.0035 0.9981 -2193.802
          #SamplingCond         6 4397.533   12.2332 0.0013 0.9994 -2192.766
          #SamplingCond_OrdDate 7 4399.247   13.9474 0.0006 1.0000 -2192.624

Lepi_La_Nuisance_mod = glmmTMB(Weight_mg~SweepVac+Temperature+Winds+StartTime+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Lepi_La)
confint(Lepi_La_Nuisance_mod,level=0.85)
summary(Lepi_La_Nuisance_mod)

#>>Stage 2: Lepi_La, TSH----
Lepi_La_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~SweepVac+                                  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  TSH                        = glmmTMB(y~SweepVac+TSH+                              (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat                  = glmmTMB(y~SweepVac+HerbTreat_14.18+                  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat               = glmmTMB(y~SweepVac+GrazingTreat+                     (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_TSH              = glmmTMB(y~SweepVac+HerbTreat_14.18*TSH+              (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat_TSH           = glmmTMB(y~SweepVac+GrazingTreat+TSH+                 (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~SweepVac+GrazingTreat+HerbTreat_14.18+     (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~SweepVac+HerbTreat_14.18*TSH+GrazingTreat+ (1|Pasture),REML="FALSE", family=gaussian, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Lepi_La_Herb_mods(Indiv_Lepi_La$Weight_mg,Indiv_Lepi_La) 
          #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #TSH                         6 4106.427    0.0000 0.6979 0.6979 -2047.213
          #GrazingTreat_TSH            8 4109.627    3.1998 0.1409 0.8388 -2046.813
          #HerbTreat_TSH              12 4109.701    3.2744 0.1358 0.9746 -2042.851
          #HerbTreat_TSH_GrazingTreat 14 4113.052    6.6253 0.0254 1.0000 -2042.526
          #Null                        4 4385.300  278.8728 0.0000 1.0000 -2188.650
          #HerbTreat                   6 4386.885  280.4582 0.0000 1.0000 -2187.443
          #GrazingTreat                6 4387.787  281.3601 0.0000 1.0000 -2187.893
          #HerbTreat_GrazingTreat      8 4389.845  283.4184 0.0000 1.0000 -2186.923

#....................................................................................#####
#7. Lepidopteran (Lepi) Adult Abundance ----
#....................................................................................#

#>>Stage 1: Lepi_Ad, Sweepvac+StartTime+(Temp)+(Winds)----
AICregress_Nuisance_mods(Indiv_Lepi_Ad$Weight_mg,Indiv_Lepi_Ad,gaussian) #CHANGED CHECK TABLE####
          #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #Global               8 2073.815    0.0000 0.6266 0.6266 -1028.908
          #SamplingCond_OrdDate 7 2075.072    1.2564 0.3343 0.9609 -1030.536
          #OrdDate_Method       5 2081.538    7.7225 0.0132 0.9741 -1035.769
          #OrdDate              4 2082.215    8.3998 0.0094 0.9835 -1037.108
          #StartTime            4 2082.866    9.0505 0.0068 0.9903 -1037.433
          #SamplingCond_Method  7 2083.310    9.4948 0.0054 0.9957 -1034.655
          #SamplingCond         6 2084.528   10.7125 0.0030 0.9987 -1036.264
          #SweepVac             4 2087.887   14.0716 0.0006 0.9992 -1039.944
          #Null                 3 2088.824   15.0082 0.0003 0.9996 -1041.412
          #Winds                4 2089.803   15.9874 0.0002 0.9998 -1040.901
          #Temp                 4 2089.931   16.1157 0.0002 1.0000 -1040.966
Lepi_Ad_Nuisance_mod = glmmTMB(Weight_mg~SweepVac+Temperature+Winds+StartTime+OrdinalSamplingDate+(1|Pasture),REML="FALSE", family=gaussian, data=Indiv_Lepi_Ad)
confint(Lepi_Ad_Nuisance_mod,level=0.85)
summary(Lepi_Ad_Nuisance_mod) #temperature and winds uninformative

#>>Stage 2: Lepi_Ad, GrazingTreat+TSH---- #CHANGED THIS, CHECK TABLE####
Lepi_Ad_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~OrdinalSamplingDate+SweepVac+StartTime+                                  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  TSH                        = glmmTMB(y~OrdinalSamplingDate+SweepVac+StartTime+TSH+                              (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat                  = glmmTMB(y~OrdinalSamplingDate+SweepVac+StartTime+HerbTreat_14.18+                  (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat               = glmmTMB(y~OrdinalSamplingDate+SweepVac+StartTime+GrazingTreat+                     (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_TSH              = glmmTMB(y~OrdinalSamplingDate+SweepVac+StartTime+HerbTreat_14.18*TSH+              (1|Pasture),REML="FALSE", family=gaussian, data=df)
  GrazingTreat_TSH           = glmmTMB(y~OrdinalSamplingDate+SweepVac+StartTime+GrazingTreat+TSH+                 (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~OrdinalSamplingDate+SweepVac+StartTime+GrazingTreat+HerbTreat_14.18+     (1|Pasture),REML="FALSE", family=gaussian, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~OrdinalSamplingDate+SweepVac+StartTime+HerbTreat_14.18*TSH+GrazingTreat+ (1|Pasture),REML="FALSE", family=gaussian, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Lepi_Ad_Herb_mods(Indiv_Lepi_Ad$Weight_mg,Indiv_Lepi_Ad) 
          #                            K      AIC Delta_AIC  AICWt Cum.Wt         LL
          #GrazingTreat_TSH           10 1897.034    0.0000 0.4907 0.4907  -938.5168
          #TSH                         8 1898.843    1.8090 0.1986 0.6893  -941.4213
          #HerbTreat_TSH_GrazingTreat 16 1898.864    1.8308 0.1964 0.8857  -933.4323
          #HerbTreat_TSH              14 1899.948    2.9143 0.1143 1.0000  -935.9740
          #GrazingTreat                8 2068.977  171.9434 0.0000 1.0000 -1026.4885
          #Null                        6 2069.825  172.7915 0.0000 1.0000 -1028.9126
          #HerbTreat_GrazingTreat     10 2071.911  174.8770 0.0000 1.0000 -1025.9553
          #HerbTreat                   8 2072.338  175.3048 0.0000 1.0000 -1028.1693

