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

library(AICcmodavg)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(dplyr)
library(gridExtra)
library(multcomp)
library(sjstats)
library(lsr)
library(lme4)
library(statmod)
library(tidyverse)
library(cowplot)
library(glmmTMB)
library(ggeffects)
library(effects)
library(MASS)
library(patchwork)

#....................................................................................#####
#1. Load Data & Functions----
#....................................................................................#

#>>Functions etc----
dodge <- position_dodge(width=0.9) #(this is dumb, but not too hard to get around)

theme_bar_Graze_leg <- function () { 
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=8, color="black"),
        legend.title=element_text(size=8, color="black", face="bold"),
        panel.grid=element_blank(),
        plot.title=element_text(size=11, face="bold",hjust=0))
}

Data_Cleaning_Gr = function (df) {
  #setting NAs from visits to 0
  df$Orth[is.na(df$Orth)] <- 0
  df$Hemipterans[is.na(df$Hemipterans)] <- 0
  df$Cole_Ad[is.na(df$Cole_Ad)] <- 0
  df$Aran[is.na(df$Aran)] <- 0
  df$Lepi_La[is.na(df$Lepi_La)] <- 0
  df$Lepi_Ad[is.na(df$Lepi_Ad)] <- 0
  
  df$Lepi<-df$Lepi_La+df$Lepi_Ad
  
  df = mutate(df, Birdfood=df$Orth + df$Lepi + df$Aran + df$Cole_Ad+df$Hemipterans)%>%
    mutate(Temp=case_when(
      Temp %in% 30:35 ~ 32,
      Temp %in% 35:40 ~ 37,
      Temp %in% 40:45 ~ 42,
      Temp %in% 45:50 ~ 47,
      Temp %in% 50:55 ~ 52,
      Temp %in% 55:60 ~ 57,
      Temp %in% 60:65 ~ 62,
      Temp %in% 65:70 ~ 67,
      Temp %in% 70:75 ~ 72,
      Temp %in% 75:80 ~ 77,
      Temp %in% 80:85 ~ 82,
      Temp %in% 85:90 ~ 87,
      Temp %in% 90:95 ~ 92,
      Temp %in% 95:100 ~ 97,
      Temp %in% 100:105 ~ 102,
      Temp %in% 105:110 ~ 107,
      Temp %in% 110:115 ~ 112,
      Temp %in% 115:120 ~ 117))
  
  #replacing missing covariates to the mean
  df$Temp               [is.na(df$Clouds_1)]            <- mean(df$Temp,na.rm=T)
  df$Winds              [is.na(df$Winds)]               <- mean(df$Winds,na.rm=T)
  df$OrdinalSamplingDate[is.na(df$OrdinalSamplingDate)] <- mean(df$OrdinalSamplingDate,na.rm=T)
  df$StartTime          [is.na(df$StartTime)]           <- mean(df$StartTime,na.rm=T)
  
  #rounding and and rescaling large numbers by dividing by 100
  df$Temp <- round((df$Temp/100),2)
  df$OrdinalSamplingDate <- round((df$OrdinalSamplingDate/100),2)
  df$StartTime <- round(df$StartTime,4)
  
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
}

AICregress_Nuisance_mods <- function(y,df,f) { #NB = negative binomial
  Null                   = glmmTMB(y~1+                                                (1|Pasture),REML="FALSE", family=f, data=df)
  SweepVac               = glmmTMB(y~SweepVac+                                         (1|Pasture),REML="FALSE", family=f, data=df)
  Winds                  = glmmTMB(y~Winds+                                            (1|Pasture),REML="FALSE", family=f, data=df)
  OrdDate                = glmmTMB(y~OrdinalSamplingDate+                              (1|Pasture),REML="FALSE", family=f, data=df)
  Temp                   = glmmTMB(y~Temp+                                             (1|Pasture),REML="FALSE", family=f, data=df)
  StartTime              = glmmTMB(y~StartTime+                                        (1|Pasture),REML="FALSE", family=f, data=df)
  SamplingCond           = glmmTMB(y~Winds+Temp+StartTime+                             (1|Pasture),REML="FALSE", family=f, data=df)
  SamplingCond_OrdDate   = glmmTMB(y~OrdinalSamplingDate+StartTime+Winds+Temp+         (1|Pasture),REML="FALSE", family=f, data=df)
  SamplingCond_Method    = glmmTMB(y~Winds+Temp+StartTime+SweepVac+                    (1|Pasture),REML="FALSE", family=f, data=df)
  OrdDate_Method         = glmmTMB(y~OrdinalSamplingDate+SweepVac+                     (1|Pasture),REML="FALSE", family=f, data=df)
  Global                 = glmmTMB(y~SweepVac+Winds+OrdinalSamplingDate+Temp+StartTime+(1|Pasture),REML="FALSE", family=f, data=df)
  
  mods=list(Null, SweepVac,  Winds,  OrdDate,  Temp,  StartTime,  SamplingCond,  SamplingCond_OrdDate,  SamplingCond_Method,  OrdDate_Method,  Global)
  names=c("Null","SweepVac","Winds","OrdDate","Temp","StartTime","SamplingCond","SamplingCond_OrdDate","SamplingCond_Method","OrdDate_Method","Global")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 
AICregress_Nuisance_mods_ZIP <- function(y,df,f) { #NB = negative binomial
  Null                   = glmmTMB(y~1+                                                (1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  SweepVac               = glmmTMB(y~SweepVac+                                         (1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  Winds                  = glmmTMB(y~Winds+                                            (1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  OrdDate                = glmmTMB(y~OrdinalSamplingDate+                              (1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  Temp                   = glmmTMB(y~Temp+                                             (1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  StartTime              = glmmTMB(y~StartTime+                                        (1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  SamplingCond           = glmmTMB(y~Winds+Temp+StartTime+                             (1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  SamplingCond_OrdDate   = glmmTMB(y~OrdinalSamplingDate+StartTime+Winds+Temp+         (1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  SamplingCond_Method    = glmmTMB(y~Winds+Temp+StartTime+SweepVac+                    (1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  OrdDate_Method         = glmmTMB(y~OrdinalSamplingDate+SweepVac+                     (1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  Global                 = glmmTMB(y~SweepVac+Winds+OrdinalSamplingDate+Temp+StartTime+(1|Pasture),REML="FALSE", family=poisson, ziformula=~1, data=df)
  
  mods=list(Null, SweepVac,  Winds,  OrdDate,  Temp,  StartTime,  SamplingCond,  SamplingCond_OrdDate,  SamplingCond_Method,  OrdDate_Method,  Global)
  names=c("Null","SweepVac","Winds","OrdDate","Temp","StartTime","SamplingCond","SamplingCond_OrdDate","SamplingCond_Method","OrdDate_Method","Global")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 
#Filtering to just get the herbicide Pastures, minus STE 2015 (includes BSH, DUN, GIL, LTR, RC2, could add RCH2014 depending)


#>>Importing the data----
ArthPatch_All=read.csv("/cloud/project/Arth_Abund/PatchArthData_Landscape_9.10.2019.csv")
#View(ArthPatch_All)
ArthPatch_Graze=Data_Cleaning_Gr (ArthPatch_All)
#View(ArthPatch_Graze)

save(ArthPatch_Graze, file="/cloud/project/Arth_Abund/ArthPatch_Graze.Rdata")


#....................................................................................#####
#2. Dealing with Extreme Outliers ----
#....................................................................................#


#    
# boxplot_outliers          =function(df,y,title){
#    ggplot(df, aes(x=Year, y=y))+ 
#       geom_boxplot(outlier.colour="dark red",outlier.shape=19,outlier.size=2)+
#       ggtitle(title)+
#       xlab("Study Years")+
#       ylab("Abundance")+
#       scale_x_discrete(labels=(c("2015","2016",'2017')))+
#       theme_classic()+
#       theme(text=element_text(size=10),axis.title=element_text(face="bold", size=10),
#             axis.text=element_text(size=10,color="black"),
#             axis.line=element_line(color="black",size=1),
#             panel.grid=NULL,
#             panel.background=element_rect("snow2"),
#             plot.title = element_text(hjust = 0.5),
#             title=element_text(size=14,color="black"))
#   }
#     Orth_Boxplot_Graze     =boxplot_outliers(ArthPatch_Graze,(ArthPatch_Graze$Orth),        "Orth")
#     Aran_Boxplot_Graze     =boxplot_outliers(ArthPatch_Graze,(ArthPatch_Graze$Aran),        "Aran")
#     Cole_Boxplot_Graze     =boxplot_outliers(ArthPatch_Graze,(ArthPatch_Graze$Cole_Ad),     "Cole")
#     Lepi_Boxplot_Graze     =boxplot_outliers(ArthPatch_Graze,(ArthPatch_Graze$Lepi),        "Lepi")
#     Hemi_Boxplot_Graze     =boxplot_outliers(ArthPatch_Graze,(ArthPatch_Graze$Hemipterans), "Hemi")
#     Birdfood_Boxplot_Graze =boxplot_outliers(ArthPatch_Graze,(ArthPatch_Graze$Birdfood),    "Total")
#     
#     all.Graze.boxplot <- plot_grid(
#       Orth_Boxplot_Graze   + theme(legend.position="none"),
#       Aran_Boxplot_Graze   + theme(legend.position="none"),
#       Cole_Boxplot_Graze   + theme(legend.position="none"),
#       Lepi_Boxplot_Graze   + theme(legend.position="none"),
#       Hemi_Boxplot_Graze   + theme(legend.position="none"),
#       Birdfood_Boxplot_Graze+ theme(legend.position="none"),
#       
#       align   = 'vh',
#       #labels = c("A", "B", "C"),
#       hjust   = -1,
#       nrow    = 3
#     )
#     print(all.Graze.boxplot)   

cap_outliers = function (y){
  qnt  <- quantile(y, probs=c(.25, .75), na.rm = T) #don't worry about the particulars of this code, just replace the variable names you want to work with. (e.g., replace Orth with Hoppers, etc.)
  caps <- quantile(y, probs=c(.01, .99), na.rm = T)
  H <- 3 * IQR    (y,na.rm = T)
  y[y < (qnt[1] - H)] <- caps[1]
  y[y > (qnt[2] + H)] <- caps[2]
  y=round(y, 0)
}

ArthPatch_Graze$Orth_NO_Gr     =cap_outliers(ArthPatch_Graze$Orth)
ArthPatch_Graze$Aran_NO_Gr     =cap_outliers(ArthPatch_Graze$Aran)
ArthPatch_Graze$Cole_NO_Gr     =cap_outliers(ArthPatch_Graze$Cole_Ad)
ArthPatch_Graze$Lepi_NO_Gr     =cap_outliers(ArthPatch_Graze$Lepi)
ArthPatch_Graze$Hemi_NO_Gr     =cap_outliers(ArthPatch_Graze$Hemipterans)
# 
# boxplot_no_outliers    =function     (df,y,title){
#     ggplot(df, aes(x=Year, y=y))+ 
#      geom_boxplot(outlier.colour="dark red",outlier.shape=19,outlier.size=2)+
#      ggtitle(title)+
#       xlab("Study Years")+
#       ylab("Abundance")+
#       scale_x_discrete(labels=(c("2015","2016",'2017')))+
#       theme_classic()+
#       theme(text=element_text(size=10),axis.title=element_text(face="bold", size=10),
#             axis.text=element_text(size=10,color="black"),
#             axis.line=element_line(color="black",size=1),
#             panel.grid=NULL,
#             panel.background=element_rect("snow2"),
#             plot.title = element_text(hjust = 0.5),
#             title=element_text(size=14,color="black"))
#     }
# Orth_NO_Boxplot_Graze       =boxplot_no_outliers(ArthPatch_Graze,ArthPatch_Graze$Orth_NO,       "Orth")
# Aran_NO_Boxplot_Graze       =boxplot_no_outliers(ArthPatch_Graze,ArthPatch_Graze$Aran_NO,       "Aran")
# Cole_NO_Boxplot_Graze       =boxplot_no_outliers(ArthPatch_Graze,ArthPatch_Graze$Cole_NO,       "Cole")
# Lepi_NO_Boxplot_Graze       =boxplot_no_outliers(ArthPatch_Graze,ArthPatch_Graze$Lepi_NO,       "Lepi")
# Hemi_NO_Boxplot_Graze       =boxplot_no_outliers(ArthPatch_Graze,ArthPatch_Graze$Hemi_NO,       "Hemi")
# Birdfood_NO_Boxplot_Graze   =boxplot_no_outliers(ArthPatch_Graze,ArthPatch_Graze$Birdfood_NO,   "Total")
# 
# all.GrazeNO.boxplot <- plot_grid(
#   Orth_NO_Boxplot_Graze       + theme(legend.position="none"),
#   Aran_NO_Boxplot_Graze       + theme(legend.position="none"),
#   Cole_NO_Boxplot_Graze       + theme(legend.position="none"),
#   Lepi_NO_Boxplot_Graze       + theme(legend.position="none"),
#   Hemi_NO_Boxplot_Graze       + theme(legend.position="none"),
#   Birdfood_NO_Boxplot_Graze   + theme(legend.position="none"),
#   align   = 'vh',
#   #labels = c("A", "B", "C"),
#   hjust   = -1,
#   nrow    = 3
# )
# print(all.GrazeNO.boxplot)   


#....................................................................................#####
#3. Orthopteran (Orth) Abundance ----
#....................................................................................#

#>>Stage 1: Orth, (Winds)+Temp+(StartTime)+SweepVac+ ----

AICregress_Nuisance_mods(ArthPatch_Graze$Orth_NO_Gr,ArthPatch_Graze,nbinom1)
          #                      K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # SamplingCond_Method  7 9678.921    0.0000 0.3964 0.3964 -4832.460
          # Global               8 9679.602    0.6815 0.2819 0.6783 -4831.801
          # OrdDate_Method       5 9680.230    1.3096 0.2060 0.8843 -4835.115
          # SweepVac             4 9681.385    2.4648 0.1156 0.9999 -4836.693
          # Temp                 4 9696.037   17.1166 0.0001 1.0000 -4844.019
          # SamplingCond         6 9698.780   19.8598 0.0000 1.0000 -4843.390
          # SamplingCond_OrdDate 7 9700.139   21.2180 0.0000 1.0000 -4843.069
          # Null                 3 9700.686   21.7657 0.0000 1.0000 -4847.343
          # OrdDate              4 9700.956   22.0356 0.0000 1.0000 -4846.478
          # Winds                4 9702.125   23.2044 0.0000 1.0000 -4847.062
          # StartTime            4 9702.528   23.6076 0.0000 1.0000 -4847.264
Orth_Nuisance_mod_Gr = glmmTMB(Orth_NO_Gr~Winds+Temp+StartTime+SweepVac+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
confint(Orth_Nuisance_mod_Gr,level=0.85)

#>>Stage 2: Orth, HerbYesNo + GrazingTreat----
Orth_Graze_mods=function(y,df) { 
  Null                       = glmmTMB(y~Year+Temp+SweepVac+                               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo                  = glmmTMB(y~Year+Temp+SweepVac+HerbYesNo_alltime+             (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_v_Grazing        = glmmTMB(y~Year+Temp+SweepVac+HerbYesNo_alltime*GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_GrazingTreat     = glmmTMB(y~Year+Temp+SweepVac+HerbYesNo_alltime+GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~Year+Temp+SweepVac+GrazingTreat+                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_Grazing,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*Grazing",  "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 
Orth_Graze_mods(ArthPatch_Graze$Orth_NO_Gr,ArthPatch_Graze)
        #                         K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # HerbYesNo+GrazingTreat 10 9455.387    0.0000 0.4230 0.4230 -4717.694
        # HerbYesNo               8 9455.535    0.1478 0.3929 0.8159 -4719.768
        # HerbYesNo*Grazing      12 9457.056    1.6687 0.1836 0.9995 -4716.528
        # Null                    7 9470.068   14.6803 0.0003 0.9998 -4728.034
        # GrazingTreat            9 9470.476   15.0884 0.0002 1.0000 -4726.238


#>>Top Model & Predicted Values----
Orth_Top_Gr = glmmTMB(Orth_NO_Gr~SweepVac+Year+Temp+HerbYesNo_alltime+GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
ggpredict(Orth_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","Year","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Orth_Pred_Gr = as.data.frame(ggpredict(Orth_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","Year","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))

colnames(Orth_Pred_Gr)=c("HerbYesNo", "Predicted","SE","Lower","Upper","GrazingTreat","Year","SweepVac") #renames columns
#View(Orth_Pred_Gr) 

Orth_Pred_Gr_Sum= Orth_Pred_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
#View(Orth_Pred_Gr_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot orths!!
Orth_Pred_Gr_Sum$GrazingTreat=factor(Orth_Pred_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None"))
Orth.Plot_Gr=ggplot(data=Orth_Pred_Gr_Sum, y=Predicted, x=GrazingTreat)+  
  geom_bar(aes(x=GrazingTreat, y=Predicted,fill=HerbYesNo), position=dodge, stat="identity")+
  scale_fill_manual(values=c("darkseagreen4","goldenrod3"))+
  theme( axis.title.x=element_blank(),
         axis.title.y=element_blank())+
  theme_bar_Graze_leg()+  
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(labels=c("Early-\nIntensive","Season-\nLong","None"))+
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_errorbar(aes(x = GrazingTreat, ymin = Lower, ymax = Upper,group = HerbYesNo),position = dodge, width = 0.2)+
  labs(y = "Orthoptera/sample",x="Grazing Treatment",fill="Herbicide Applied?")

print(Orth.Plot_Gr)

#....................................................................................#####
#3. Hemipteran (Hemi) Abundance ----
#....................................................................................#


#>>Stage 1: Hemi, StartTime+SweepVac+(Winds)+(Temp)+OrdinalSamplingDate----
AICregress_Nuisance_mods(ArthPatch_Graze$Hemi_NO_Gr,ArthPatch_Graze,nbinom1)
          #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # Global               8 11903.85    0.0000 0.8940 0.8940 -5943.925
          # SamplingCond_Method  7 11908.31    4.4638 0.0959 0.9899 -5947.157
          # OrdDate_Method       5 11913.39    9.5355 0.0076 0.9975 -5951.693
          # SweepVac             4 11915.63   11.7798 0.0025 1.0000 -5953.815
          # StartTime            4 11933.88   30.0339 0.0000 1.0000 -5962.942
          # SamplingCond_OrdDate 7 11935.16   31.3056 0.0000 1.0000 -5960.578
          # SamplingCond         6 11936.87   33.0227 0.0000 1.0000 -5962.436
          # OrdDate              4 11942.29   38.4382 0.0000 1.0000 -5967.144
          # Null                 3 11942.49   38.6423 0.0000 1.0000 -5968.246
          # Temp                 4 11943.79   39.9361 0.0000 1.0000 -5967.893
          # Winds                4 11944.05   40.2023 0.0000 1.0000 -5968.026
Hemi_Nuisance_mod_Gr = glmmTMB(Hemi_NO_Gr~StartTime+Winds+Temp+SweepVac+OrdinalSamplingDate+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
confint(Hemi_Nuisance_mod_Gr,level=0.85)
summary(Hemi_Nuisance_mod_Gr)

#>>Stage 2: Hemi, HerbYesNo+GrazingTreat---
Hemi_Graze_mods=function(y,df) { 
  Null                       = glmmTMB(y~Year+StartTime+SweepVac+OrdinalSamplingDate+                                   (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo                  = glmmTMB(y~Year+StartTime+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime+                 (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_v_GrazingTreat   = glmmTMB(y~Year+StartTime+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime*GrazingTreat+    (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_GrazingTreat     = glmmTMB(y~Year+StartTime+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime+GrazingTreat+    (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~Year+StartTime+SweepVac+OrdinalSamplingDate+GrazingTreat+                      (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*GrazingTreat",    "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Hemi_Graze_mods(ArthPatch_Graze$Hemi_NO_Gr,ArthPatch_Graze)
          #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
          #HerbYesNo+GrazingTreat 11 11872.91    0.0000 0.3935 0.3935 -5925.453
          #HerbYesNo               9 11872.95    0.0431 0.3851 0.7786 -5927.475
          #HerbYesNo*GrazingTreat 13 11874.06    1.1503 0.2214 1.0000 -5924.029
          #Null                    8 11892.39   19.4785 0.0000 1.0000 -5938.193
          #GrazingTreat           10 11893.37   20.4582 0.0000 1.0000 -5936.683


#>>Top Model & Predicted Values----

Hemi_Top_Gr = glmmTMB(Hemi_NO_Gr~Year+StartTime+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime+GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
#ggpredict(Hemi_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","GrazingTreat","Year"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Hemi_Pred_Gr = as.data.frame(ggpredict(Hemi_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","SweepVac","Year"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
colnames(Hemi_Pred_Gr)=c("HerbYesNo", "Predicted","SE","Lower","Upper","GrazingTreat","SweepVac","Year") #renames columns
#View(Hemi_Pred) 

Hemi_Pred_Gr_Sum= Hemi_Pred_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
#View(Hemi_Pred_Gr_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot Hemis!!
Hemi_Pred_Gr_Sum$GrazingTreat=factor(Hemi_Pred_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None"))
Hemi.Plot_Gr=ggplot(data=Hemi_Pred_Gr_Sum, y=Predicted, x=GrazingTreat)+  
  geom_bar(aes(x=GrazingTreat, y=Predicted,fill=HerbYesNo), position=dodge, stat="identity")+
  scale_fill_manual(values=c("darkseagreen4","goldenrod3"))+
  theme( axis.title.x=element_blank(),
         axis.title.y=element_blank())+
  theme_bar_Graze_leg()+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(labels=c("Early-\nIntensive","Season-\nLong","None"))+
  scale_y_continuous(limits = c(0,85), expand = c(0, 0)) +
  geom_errorbar(aes(x = GrazingTreat, ymin = Lower, ymax = Upper,group = HerbYesNo),position = dodge, width = 0.2)+
  labs(y = "Hemiptera/sample",x="Grazing Treatment",fill="Herbicide Applied?")

print(Hemi.Plot_Gr)


#....................................................................................#####
#5. Araneae (Aran) Abundance ----
#....................................................................................#

#>>Stage 1: Aran, OrdinalSamplingDate----

AICregress_Nuisance_mods(ArthPatch_Graze$Aran_NO_Gr,ArthPatch_Graze,nbinom1)
          #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # OrdDate              4 6932.711    0.0000 0.5492 0.5492 -3462.356
          # OrdDate_Method       5 6934.711    1.9999 0.2020 0.7512 -3462.356
          # SamplingCond_OrdDate 7 6934.922    2.2105 0.1819 0.9331 -3460.461
          # Global               8 6936.922    4.2103 0.0669 1.0000 -3460.461
          # SamplingCond         6 7018.317   85.6061 0.0000 1.0000 -3503.159
          # Temp                 4 7019.019   86.3077 0.0000 1.0000 -3505.510
          # SamplingCond_Method  7 7020.303   87.5922 0.0000 1.0000 -3503.152
          # Winds                4 7031.255   98.5440 0.0000 1.0000 -3511.628
          # StartTime            4 7032.536   99.8243 0.0000 1.0000 -3512.268
          # Null                 3 7033.571  100.8601 0.0000 1.0000 -3513.786
          # SweepVac             4 7035.564  102.8522 0.0000 1.0000 -3513.782

Aran_Nuisance_mod_Gr = glmmTMB(Aran_NO_Gr~OrdinalSamplingDate+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
confint(Aran_Nuisance_mod_Gr,level=0.85)
summary(Aran_Nuisance_mod_Gr)

#>>Stage 2: null -->could check HerbYesNo----

Aran_Graze_mods=function(y,df) { 
  Null                       = glmmTMB(y~Year+OrdinalSamplingDate+                                   (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo                  = glmmTMB(y~Year+OrdinalSamplingDate+HerbYesNo_alltime+                 (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_v_GrazingTreat   = glmmTMB(y~Year+OrdinalSamplingDate+HerbYesNo_alltime*GrazingTreat+    (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_GrazingTreat     = glmmTMB(y~Year+OrdinalSamplingDate+HerbYesNo_alltime+GrazingTreat+    (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~Year+OrdinalSamplingDate+GrazingTreat+                      (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*Grazing",  "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Aran_Graze_mods(ArthPatch_Graze$Aran_NO_Gr,ArthPatch_Graze) 
        #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # Null                    6 6864.991    0.0000 0.4595 0.4595 -3426.495
        # HerbYesNo               7 6866.077    1.0866 0.2669 0.7265 -3426.039
        # GrazingTreat            8 6867.332    2.3408 0.1426 0.8690 -3425.666
        # HerbYesNo+GrazingTreat  9 6868.409    3.4184 0.0832 0.9522 -3425.205
        # HerbYesNo*Grazing      11 6869.518    4.5270 0.0478 1.0000 -3423.759

#....................................................................................#####
#6. Coleopteran Abundance ----
#....................................................................................#

#>>Stage 1: Cole, StartTime+OrdinalSamplingDate+Winds+Temp+SweepVac----

AICregress_Nuisance_mods(ArthPatch_Graze$Cole_NO_Gr,ArthPatch_Graze,nbinom1)
          #                      K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # Global               8 6704.772    0.0000 0.9952 0.9952 -3344.386
          # OrdDate_Method       5 6715.450   10.6781 0.0048 1.0000 -3352.725
          # SamplingCond_Method  7 6773.465   68.6933 0.0000 1.0000 -3379.733
          # SweepVac             4 6796.535   91.7637 0.0000 1.0000 -3394.268
          # SamplingCond_OrdDate 7 6846.434  141.6621 0.0000 1.0000 -3416.217
          # OrdDate              4 6849.265  144.4928 0.0000 1.0000 -3420.632
          # Temp                 4 6893.280  188.5080 0.0000 1.0000 -3442.640
          # SamplingCond         6 6894.815  190.0433 0.0000 1.0000 -3441.407
          # Null                 3 6908.642  203.8704 0.0000 1.0000 -3451.321
          # Winds                4 6908.819  204.0470 0.0000 1.0000 -3450.409
          # StartTime            4 6909.714  204.9427 0.0000 1.0000 -3450.857
Cole_Nuisance_mod_Gr = glmmTMB(Cole_NO_Gr~OrdinalSamplingDate+StartTime+Winds+Temp+SweepVac+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
confint(Cole_Nuisance_mod_Gr,level=0.85)
summary(Cole_Nuisance_mod_Gr)

#>>Stage 2: Cole, HerbYesNo*Grazing----

Cole_Graze_mods=function(y,df) { 
  Null                       = glmmTMB(y~Year+StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+                                 (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo                  = glmmTMB(y~Year+StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+HerbYesNo_alltime+               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_v_GrazingTreat   = glmmTMB(y~Year+StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+HerbYesNo_alltime*GrazingTreat+               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_GrazingTreat     = glmmTMB(y~Year+StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+HerbYesNo_alltime+GrazingTreat+  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~Year+StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+GrazingTreat+                    (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*Grazing", "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

#>>Cole: Null, HerbYesNo*Grazing----
Cole_Graze_mods(ArthPatch_Graze$Cole_NO_Gr,ArthPatch_Graze) 
          #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # HerbYesNo*Grazing      15 6655.510    0.0000 0.6323 0.6323 -3312.755
          # GrazingTreat           12 6657.544    2.0338 0.2287 0.8610 -3316.772
          # HerbYesNo+GrazingTreat 13 6658.920    3.4101 0.1149 0.9759 -3316.460
          # Null                   10 6662.903    7.3933 0.0157 0.9916 -3321.452
          # HerbYesNo              11 6664.143    8.6331 0.0084 1.0000 -3321.072

#>>Top Model & Predicted Values: something to see here!----
Cole_Top_Gr = glmmTMB(Cole_NO_Gr~Year+StartTime+OrdinalSamplingDate+Temp+Winds+SweepVac+HerbYesNo_alltime*GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
#ggpredict(Cole_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","Year","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Cole_Pred_Gr = as.data.frame(ggpredict(Cole_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","Year","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
colnames(Cole_Pred_Gr)=c("HerbYesNo", "Predicted","SE","Lower","Upper","GrazingTreat","Year","SweepVac") #renames columns
#View(Cole_Pred_Gr) 

Cole_Pred_Gr_Sum= Cole_Pred_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
#View(Cole_Pred_Gr_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot Coles!!
Cole_Pred_Gr_Sum$GrazingTreat=factor(Cole_Pred_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None"))
Cole.Plot_Gr=ggplot(data=Cole_Pred_Gr_Sum, y=Predicted, x=GrazingTreat)+  
  geom_bar(aes(x=GrazingTreat, y=Predicted,fill=HerbYesNo), position=dodge, stat="identity")+
  scale_fill_manual(values=c("darkseagreen4","goldenrod3"))+
  theme()+
  theme_bar_Graze_leg()+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(labels=c("Early-\nIntensive","Season-\nLong","None"))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10),limits = c(0,10), expand = c(0, 0)) +
  geom_errorbar(aes(x = GrazingTreat, ymin = Lower, ymax = Upper,group = HerbYesNo),position = dodge, width = 0.2)+
  labs(y = "Coleoptera/sample", x="Grazing Treatment", fill = "Herbicide Applied?")+
  ggtitle("D. Coleoptera")

print(Cole.Plot_Gr)


library(egg)

#....................................................................................#####
#7. Lepidopteran (Lepi) Abundance ----
#....................................................................................#

#>>Stage 1: Lepi, SweepVac+OrdinalSamplingDate----

AICregress_Nuisance_mods(ArthPatch_Graze$Lepi_NO_Gr,ArthPatch_Graze,nbinom1)
        #                      K      AIC Delta_AIC  AICWt Cum.Wt        LL
        # OrdDate_Method       5 4655.942    0.0000 0.4014 0.4014 -2322.971
        # SweepVac             4 4656.842    0.9004 0.2559 0.6574 -2324.421
        # Global               8 4657.290    1.3483 0.2046 0.8619 -2320.645
        # SamplingCond_Method  7 4658.076    2.1345 0.1381 1.0000 -2322.038
        # StartTime            4 4684.938   28.9963 0.0000 1.0000 -2338.469
        # OrdDate              4 4685.207   29.2659 0.0000 1.0000 -2338.604
        # Null                 3 4686.307   30.3653 0.0000 1.0000 -2340.153
        # SamplingCond_OrdDate 7 4686.945   31.0038 0.0000 1.0000 -2336.473
        # Temp                 4 4687.331   31.3896 0.0000 1.0000 -2339.666
        # SamplingCond         6 4687.777   31.8354 0.0000 1.0000 -2337.889
        # Winds                4 4688.307   32.3652 0.0000 1.0000 -2340.153
Lepi_Nuisance_mod_Gr = glmmTMB(Lepi_NO_Gr~SweepVac+OrdinalSamplingDate+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
confint(Lepi_Nuisance_mod_Gr,level=0.85)
summary(Lepi_Nuisance_mod_Gr)

#>>Stage 2: Lepi, HerbYesNo+GrazingTreat----

Lepi_Graze_mods=function(y,df) { 
  Null                        = glmmTMB(y~Year+SweepVac+OrdinalSamplingDate+                                 (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo                   = glmmTMB(y~Year+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime+               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_v_GrazingTreat    = glmmTMB(y~Year+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime*GrazingTreat+  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_GrazingTreat      = glmmTMB(y~Year+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime+GrazingTreat+  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat                = glmmTMB(y~Year+SweepVac+OrdinalSamplingDate+GrazingTreat+                    (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*Grazing", "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Lepi_Graze_mods(ArthPatch_Graze$Lepi_NO_Gr,ArthPatch_Graze) 
      #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbYesNo+GrazingTreat 10 4642.030    0.0000 0.4776 0.4776 -2311.015
      # HerbYesNo               8 4642.202    0.1726 0.4381 0.9158 -2313.101
      # HerbYesNo*Grazing      12 4645.542    3.5123 0.0825 0.9983 -2310.771
      # Null                    7 4654.587   12.5573 0.0009 0.9992 -2320.294
      # GrazingTreat            9 4654.721   12.6910 0.0008 1.0000 -2318.360

#>>Top Model & Predicted Values----

Lepi_Top_Gr = glmmTMB(Lepi_NO_Gr~Year+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime+GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
#ggpredict(Lepi_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","SweepVac","Year"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Lepi_Pred_Gr = as.data.frame(ggpredict(Lepi_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","SweepVac","Year"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
colnames(Lepi_Pred_Gr)=c("HerbYesNo", "Predicted","SE","Lower","Upper","GrazingTreat","SweepVac","Year") #renames columns
#View(Lepi_Pred_Gr) 

Lepi_Pred_Gr_Sum= Lepi_Pred_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
#View(Lepi_Pred_Gr_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot Lepis!!
Lepi_Pred_Gr_Sum$GrazingTreat=factor(Lepi_Pred_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None"))
Lepi.Plot_Gr=ggplot(data=Lepi_Pred_Gr_Sum, y=Predicted, x=GrazingTreat)+  
  geom_bar(aes(x=GrazingTreat, y=Predicted,fill=HerbYesNo), position=dodge, stat="identity")+
  scale_fill_manual(values=c("darkseagreen4","goldenrod3"))+
  theme( axis.title.x=element_blank(),
         axis.title.y=element_blank())+
  theme_bar_Graze_leg()+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(labels=c("Early-\nIntensive","Season-\nLong","None"))+
  scale_y_continuous(limits = c(0,4), expand = c(0, 0)) +
  geom_errorbar(aes(x = GrazingTreat, ymin = Lower, ymax = Upper,group = HerbYesNo),position = dodge, width = 0.2)+
  labs(y = "Lepidoptera/sample", x="Grazing Treatment", fill = "Herbicide Applied?")


print(Lepi.Plot_Gr)

#....................................................................................#####
#8. Creating Multipaneled and Summary Figures ----
#....................................................................................#

ArthGrazeHerbFig2 = (  Hemi.Plot + Hemi.Plot_Gr +
                  Orth.Plot + Orth.Plot_Gr +
                  Lepi.Plot + Lepi.Plot_Gr +
                  guide_area()+Cole.Plot_Gr +
                  plot_layout(guides="collect",ncol=2))&
  theme(
    legend.justification = "left",
    #    legend.spacing.y=unit(-.1,"cm"),
        legend.box.margin=margin( 34)
  )
ArthGrazeHerbFig2

ggsave(filename="Arth_Herb_and_Graze_Fig.jpg", plot = ArthGrazeHerbFig2,
       scale = 1, width = 6.5, height = 9, units = c("in"),dpi = 300,path="/cloud/project/Figs")


#__________________________####
#9. Creating the summary figure----


Orth_None= Orth_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
Orth_SLS= Orth_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
Orth_IES= Orth_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
Orth_comb_IES <- cbind(Species="Orthoptera",Orth_IES[1],Orth_IES[2],round(Orth_IES[,3]/Orth_None[,3],4))
Orth_comb_SLS <- cbind(Species="Orthoptera",Orth_SLS[1],Orth_SLS[2],round(Orth_SLS[,3]/Orth_None[,3],4))

Hemi_None= Hemi_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
Hemi_SLS= Hemi_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
Hemi_IES= Hemi_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
Hemi_comb_IES <- cbind(Species="Hemiptera",Hemi_IES[1],Hemi_IES[2],round(Hemi_IES[,3]/Hemi_None[,3],4))
Hemi_comb_SLS <- cbind(Species="Hemiptera",Hemi_SLS[1],Hemi_SLS[2],round(Hemi_SLS[,3]/Hemi_None[,3],4))


Lepi_None= Lepi_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
Lepi_SLS= Lepi_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
Lepi_IES= Lepi_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
Lepi_comb_IES <- cbind(Species="Lepidoptera",Lepi_IES[1],Lepi_IES[2],round(Lepi_IES[,3]/Lepi_None[,3],4))
Lepi_comb_SLS <- cbind(Species="Lepidoptera",Lepi_SLS[1],Lepi_SLS[2],round(Lepi_SLS[,3]/Lepi_None[,3],4))


Cole_None= Cole_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="None")
Cole_SLS= Cole_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="SLS") 
Cole_IES= Cole_Pred_Gr_Sum %>% 
  group_by(GrazingTreat) %>% 
  filter(GrazingTreat =="IES")
Cole_comb_IES <- cbind(Species="Coleoptera",Cole_IES[1],Cole_IES[2],round(Cole_IES[,3]/Cole_None[,3],4))
Cole_comb_SLS <- cbind(Species="Coleoptera",Cole_SLS[1],Cole_SLS[2],round(Cole_SLS[,3]/Cole_None[,3],4))

HerbYesNo=c("Yes","No")

#Because Aran top model was null, filling in all 1's since Con and Treatment are equal
Aran_comb_IES <- data.frame(Species="Araneae",GrazingTreat = "IES", 
                            HerbYesNo = HerbYesNo, 
                        Predicted = 1, stringsAsFactors = FALSE)
Aran_comb_SLS <- data.frame(Species="Araneae",GrazingTreat = "SLS", 
                            HerbYesNo = HerbYesNo, 
                            Predicted = 1, stringsAsFactors = FALSE)

All_Arths_Gr=rbind(Orth_comb_IES,Orth_comb_SLS,
                   Hemi_comb_IES,Hemi_comb_SLS,
                   Lepi_comb_IES,Lepi_comb_SLS,
                   Cole_comb_IES,Cole_comb_SLS,
                   Aran_comb_IES,Aran_comb_SLS)

#All_Arths$Arth_Treatment=factor(All_Arths$Arth_Treatment,) #in case ordering needs to be changed

# Making the heatmap 
ArthSummaryFig_Gr =ggplot(All_Arths_Gr, aes(HerbYesNo, GrazingTreat, fill= Predicted)) + 
  geom_tile()+
  scale_fill_gradientn(colours = c("darkorange3","gray95","deepskyblue3"), 
                       values = rescale(c(0,1,2)),
                       guide = "colorbar")+
  facet_wrap(Species~., strip.position="left",ncol=1)+
  scale_y_discrete(expand=c(0,0),position = "left")+
  scale_x_discrete(expand=c(0,0))+
  labs(y= "Grazing", x =  "Herbicide",fill="Grazing Treatment:No Grazing")+
  theme(text=element_text(size=10),
        axis.title.x=element_text(face="bold", size=10),
        axis.title.y=element_blank(),
        axis.text=element_text(size=10,color="black"),
        axis.line.x = element_line(color="black",size=0.5),
        axis.line.y = element_line(color="black",size=0.5,),
        legend.text=element_text(size=7, color="black"),
        strip.placement = "outside",
        legend.position = ("top"),
        legend.title = element_text(vjust=0.85,size=8,color="black",face="bold"),
        plot.title = element_text(size=11,color="black",face="bold"))+
  ggtitle("B. Arthropods")+ 
  guides(fill = guide_colorbar(title.position = "bottom"))


ArthSummaryFig_Gr
ggsave(filename="ArthSummaryFig_Graze.jpg", plot = ArthSummaryFig_Gr,
       scale = 1, width = 3, height = 8, units = c("in"),dpi = 300,path="Figs")


####Making Giant Composite Figures####

#>>Grazing----
GrazeCombined = (  BirdSummaryFig_Gr + ArthSummaryFig_Gr)&
  theme(
    legend.position = "bottom")
GrazeCombined

ggsave(filename="GrazeCombined.jpg", plot = GrazeCombined,
       scale = 1, width = 5, height = 7, units = c("in"),dpi = 300,path="/cloud/project/Figs")

#>>Herbicide----
HerbCombined = (  BirdSummaryFig + ArthSummaryFig)&
  theme(
    legend.position = "bottom")
HerbCombined

ggsave(filename="HerbCombined.jpg", plot = HerbCombined,
       scale = 1, width = 5, height = 7, units = c("in"),dpi = 300,path="/cloud/project/Figs")

#>>Birds----
BirdCombined = (  BirdSummaryFig_Gr + BirdSummaryFig )&
  theme(
    legend.position = "bottom")
BirdCombined

ggsave(filename="BirdCombined.jpg", plot = BirdCombined,
       scale = 1, width = 6.5, height = 9, units = c("in"),dpi = 300,path="/cloud/project/Figs")

#>>Arthropods----
ArthCombined = (  ArthSummaryFig_Gr + ArthSummaryFig)&
  theme(
    legend.position = "bottom")
HerbCombined

ggsave(filename="ArthCombined.jpg", plot = ArthCombined,
       scale = 1, width = 6.5, height = 9, units = c("in"),dpi = 300,path="/cloud/project/Figs")

