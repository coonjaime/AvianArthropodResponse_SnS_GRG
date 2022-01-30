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

#Load libraries----
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
library (MASS)

#....................................................................................#####
#1. Load Data & Functions----
#....................................................................................#
#>>Functions etc----
dodge <- position_dodge(width=0.9) #(this is dumb, but not too hard to get around)

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
    filter(!(Past_Pat_Yr=="STE_N2_2016"))%>%
    filter(!(Past_Pat_Yr=="STE_N2_2017"))%>%
    filter(!(Past_Pat_Yr=="STE_N2_2018"))
  #filter(!(Pasture=="STE")) ##for use if we want to completely filter STE out. 
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

#Importing the data.
ArthPatch_All=read.csv("/cloud/project/Arth_Abund/PatchArthData_Landscape_9.10.2019.csv")
#View(ArthPatch_All)

#Filtering to just get the herbicide Pastures, minus STE 2015 (includes BSH, DUN, GIL, LTR, RC2, and STE post 2015)
ArthPatch_Herb=Data_Cleaning (ArthPatch_All)
#View(ArthPatch_Herb)

#saving data file
save(ArthPatch_Herb, file="/cloud/project/Arth_Abund/ArthPatch_Herb.Rdata")

load("ArthPatch_Herb.Rdata")
#view(ArthPatch_Herb.RData)



#....................................................................................#####
#2. Dealing with Extreme Outliers ----
#....................................................................................#

##optional - look at boxplot of data
# boxplot_outliers    =function        (df,             y,                                title){
#   ggplot(df, aes(x=Year, y=y))+ 
#     geom_boxplot(outlier.colour="dark red",outlier.shape=19,outlier.size=2)+
#     ggtitle(title)+
#     xlab("Study Years")+
#     ylab("Abundance")+
#     scale_x_discrete(labels=(c("2015","2016",'2017')))+
#     theme_classic()+
#     theme(text=element_text(size=10),axis.title=element_text(face="bold", size=10),
#           axis.text=element_text(size=10,color="black"),
#           axis.line=element_line(color="black",size=1),
#           panel.grid=NULL,
#           panel.background=element_rect("snow2"),
#           plot.title = element_text(hjust = 0.5),
#           title=element_text(size=14,color="black"))
# }
# Orth_Boxplot_Herb   =boxplot_outliers(ArthPatch_Herb,(ArthPatch_Herb$Orth),        "Orth")
# Aran_Boxplot_Herb   =boxplot_outliers(ArthPatch_Herb,(ArthPatch_Herb$Aran),        "Aran")
# Cole_Boxplot_Herb   =boxplot_outliers(ArthPatch_Herb,(ArthPatch_Herb$Cole_Ad),     "Cole")
# Lepi_Boxplot_Herb   =boxplot_outliers(ArthPatch_Herb,(ArthPatch_Herb$Lepi),        "Lepi")
# Hemi_Boxplot_Herb   =boxplot_outliers(ArthPatch_Herb,(ArthPatch_Herb$Hemipterans), "Hemi")
# Birdfood_Boxplot_Herb   =boxplot_outliers(ArthPatch_Herb,(ArthPatch_Herb$Birdfood),"Total")
# 
# all.Herb.boxplot <- plot_grid(
#   Orth_Boxplot_Herb   + theme(legend.position="none"),
#   Aran_Boxplot_Herb   + theme(legend.position="none"),
#   Cole_Boxplot_Herb   + theme(legend.position="none"),
#   Lepi_Boxplot_Herb   + theme(legend.position="none"),
#   Hemi_Boxplot_Herb   + theme(legend.position="none"),
#   
#   align   = 'vh',
#   #labels = c("A", "B", "C"),
#   hjust   = -1,
#   nrow    = 4
# )
#print(all.Herb.boxplot)   

#This nextcode replaces all *extreme* outliers higher than 3*IQR.
#It also sets up a new variable "Orth_NO" (Orth No Outliers) that we can use in analysis. 

cap_outliers = function (y){
  qnt  <- quantile(y, probs=c(.25, .75), na.rm = T) #don't worry about the particulars of this code, just replace the variable names you want to work with. (e.g., replace Orth with Hoppers, etc.)
  caps <- quantile(y, probs=c(.01, .99), na.rm = T)
  H <- 3 * IQR    (y,na.rm = T)
  y[y < (qnt[1] - H)] <- caps[1]
  y[y > (qnt[2] + H)] <- caps[2]
  y=round(y, 0)
}

ArthPatch_Herb$Orth_NO=cap_outliers(ArthPatch_Herb$Orth)
ArthPatch_Herb$Aran_NO=cap_outliers(ArthPatch_Herb$Aran)
ArthPatch_Herb$Cole_NO=cap_outliers(ArthPatch_Herb$Cole_Ad)
ArthPatch_Herb$Lepi_NO=cap_outliers(ArthPatch_Herb$Lepi)
ArthPatch_Herb$Hemi_NO=cap_outliers(ArthPatch_Herb$Hemipterans)
ArthPatch_Herb$Birdfood_NO=cap_outliers(ArthPatch_Herb$Birdfood)
#summary(ArthPatch_Herb$Hemipterans)

#....................................................................................#####
#3. Orthopteran (Orth) Abundance ----
#....................................................................................#

#>>Stage 1: Orth, null ----
AICregress_Nuisance_mods(ArthPatch_Herb$Orth_NO,ArthPatch_Herb,nbinom1)
          #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # Null                 3 3694.717    0.0000 0.2199 0.2199 -1844.359
          # Temp                 4 3695.043    0.3253 0.1869 0.4068 -1843.521
          # Winds                4 3695.838    1.1206 0.1256 0.5324 -1843.919
          # OrdDate              4 3695.925    1.2077 0.1202 0.6527 -1843.963
          # StartTime            4 3696.629    1.9111 0.0846 0.7372 -1844.314
          # SweepVac             4 3696.637    1.9198 0.0842 0.8215 -1844.319
          # SamplingCond         6 3697.717    2.9996 0.0491 0.8705 -1842.859
          # OrdDate_Method       5 3697.779    3.0615 0.0476 0.9181 -1843.889
          # SamplingCond_OrdDate 7 3697.908    3.1906 0.0446 0.9627 -1841.954
          # SamplingCond_Method  7 3699.619    4.9018 0.0190 0.9817 -1842.810
          # Global               8 3699.691    4.9733 0.0183 1.0000 -1841.845

Orth_Nuisance_mod = glmmTMB(~1+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
confint(Orth_Nuisance_mod,level=0.85)


Orth_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~                                   (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  TSH                        = glmmTMB(y~TSH+                               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat                  = glmmTMB(y~HerbTreat_14.18+                   (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~GrazingTreat+                      (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH              = glmmTMB(y~HerbTreat_14.18*TSH+               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat_TSH           = glmmTMB(y~GrazingTreat+TSH+                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~GrazingTreat+HerbTreat_14.18+      (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~HerbTreat_14.18*TSH+GrazingTreat+  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 


#>>Stage 2: Orth, HerbTreat_TSH----
  Orth_Herb_mods(ArthPatch_Herb$Orth_NO,ArthPatch_Herb)
          #                             K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # HerbTreat_TSH              11 3590.321    0.0000 0.7077 0.7077 -1784.161
          # HerbTreat_TSH_GrazingTreat 13 3592.106    1.7843 0.2900 0.9978 -1783.053
          # TSH                         5 3602.598   12.2767 0.0015 0.9993 -1796.299
          # GrazingTreat_TSH            7 3604.100   13.7786 0.0007 1.0000 -1795.050
          # Null                        3 3694.717  104.3962 0.0000 1.0000 -1844.359
          # GrazingTreat                5 3695.302  104.9805 0.0000 1.0000 -1842.651
          # HerbTreat                   5 3695.574  105.2525 0.0000 1.0000 -1842.787
          # HerbTreat_GrazingTreat      7 3696.298  105.9772 0.0000 1.0000 -1841.149

#>>Top Model & Predicted Values----

Orth_Top = glmmTMB(Orth_NO~HerbTreat_14.18*TSH+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
#ggpredict(Orth_Top,c("HerbTreat_14.18", "TSH"),ci.lvl=0.85, back.transform=TRUE, append=TRUE) #if you want to look at predicted values before arranging in dataframe
Orth_Pred = as.data.frame(ggpredict(Orth_Top,c("HerbTreat_14.18", "TSH"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)) #turns predictions into a dataframe that we can more easily manipulate
colnames(Orth_Pred)=c("HerbTreat", "Predicted","SE","Lower","Upper","TSH") #renames columns
#View(Orth_Pred) 
confint(Orth_Top, level = 0.85)

# Orth_Pred_Sum= Orth_Pred %>% 
#   group_by(TSH, HerbTreat) %>% 
#   summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
# View(Orth_Pred_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot orths!!
Orth_Pred$HerbTreat=factor(Orth_Pred$HerbTreat,levels=c("Con","Spr","SnS"))

Orth.Plot=ggplot(data=Orth_Pred, y=Predicted, x=TSH)+  
  geom_bar(aes(x=TSH, y=Predicted,fill=HerbTreat), position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"), labels=c("Control", "Spray", "Spray-and-Seed"))+
  theme_bar_SnS_leg()+
  theme( axis.title.x=element_blank())+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(breaks=c("a","b","c"),labels=c("1","2","3"))+
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_errorbar(aes(x = TSH, ymin = Lower, ymax = Upper,group = HerbTreat),position = dodge, width = 0.2)+
  labs(y = "Orthoptera/sample", x="Years Since Herbicide", fill = "Herbicide Treatment")+
  ggtitle("B. Orthoptera")

print(Orth.Plot)

#....................................................................................#####
#4. Hemipteran (Hemi) Abundance ----
#....................................................................................#


#>>Stage 1: Hemi, StartTime+SweepVac+(Winds)+(Temp)----
AICregress_Nuisance_mods(ArthPatch_Herb$Hemi_NO,ArthPatch_Herb,nbinom1)
          #                      K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # SamplingCond_Method  7 4656.055    0.0000 0.5229 0.5229 -2321.027
          # Global               8 4658.029    1.9745 0.1948 0.7177 -2321.015
          # SweepVac             4 4658.247    2.1928 0.1747 0.8924 -2325.124
          # OrdDate_Method       5 4659.217    3.1622 0.1076 1.0000 -2324.608
          # StartTime            4 4676.331   20.2769 0.0000 1.0000 -2334.166
          # SamplingCond         6 4678.913   22.8581 0.0000 1.0000 -2333.456
          # Null                 3 4680.733   24.6780 0.0000 1.0000 -2337.366
          # SamplingCond_OrdDate 7 4680.833   24.7788 0.0000 1.0000 -2333.417
          # OrdDate              4 4681.207   25.1524 0.0000 1.0000 -2336.604
          # Winds                4 4681.569   25.5142 0.0000 1.0000 -2336.784
          # Temp                 4 4682.733   26.6780 0.0000 1.0000 -2337.366

Hemi_Nuisance_mod = glmmTMB(Hemi_NO~StartTime+Winds+Temp+SweepVac+         (1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
confint(Hemi_Nuisance_mod,level=0.85)
summary(Hemi_Nuisance_mod)

Hemi_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~StartTime+SweepVac+                                   (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  TSH                        = glmmTMB(y~StartTime+SweepVac+TSH+                               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat                  = glmmTMB(y~StartTime+SweepVac+HerbTreat_14.18+                   (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~StartTime+SweepVac+GrazingTreat+                      (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH              = glmmTMB(y~StartTime+SweepVac+HerbTreat_14.18*TSH+               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat_TSH           = glmmTMB(y~StartTime+SweepVac+GrazingTreat+TSH+                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~StartTime+SweepVac+GrazingTreat+HerbTreat_14.18+      (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~StartTime+SweepVac+HerbTreat_14.18*TSH+GrazingTreat + (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

#
#>>Stage 2: Hemi, HerbTreat_TSH----
Hemi_Herb_mods(ArthPatch_Herb$Hemi_NO,ArthPatch_Herb)
          #                             K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # HerbTreat_TSH              13 4641.920    0.0000 0.4860 0.4860 -2307.960
          # HerbTreat                   7 4643.141    1.2216 0.2639 0.7499 -2314.571
          # HerbTreat_TSH_GrazingTreat 15 4644.417    2.4974 0.1394 0.8893 -2307.209
          # HerbTreat_GrazingTreat      9 4645.082    3.1621 0.1000 0.9893 -2313.541
          # TSH                         7 4650.667    8.7474 0.0061 0.9954 -2318.334
          # Null                        5 4652.805   10.8852 0.0021 0.9975 -2321.403
          # GrazingTreat_TSH            9 4653.196   11.2762 0.0017 0.9993 -2317.598
          # GrazingTreat                7 4654.946   13.0259 0.0007 1.0000 -2320.473

#>>Top Model & Predicted Values----

Hemi_Top = glmmTMB(Hemi_NO~StartTime+SweepVac+HerbTreat_14.18*TSH+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
#ggpredict(Hemi_Top,c("HerbTreat_14.18", "TSH","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Hemi_Pred = as.data.frame(ggpredict(Hemi_Top,c("HerbTreat_14.18", "TSH","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
colnames(Hemi_Pred)=c("HerbTreat", "Predicted","SE","Lower","Upper","TSH","SweepVac") #renames columns
#View(Hemi_Pred) 
confint(Hemi_Top, level = 0.85)

Hemi_Pred_Sum= Hemi_Pred %>% 
  group_by(TSH, HerbTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
#View(Hemi_Pred_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot Hemis!!
Hemi_Pred_Sum$HerbTreat=factor(Hemi_Pred_Sum$HerbTreat,levels=c("Con","Spr","SnS"))
Hemi.Plot=ggplot(data=Hemi_Pred_Sum, y=Predicted, x=TSH)+  
  geom_bar(aes(x=TSH, y=Predicted,fill=HerbTreat), position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"), labels=c("Control", "Spray", "Spray-and-Seed"))+
  theme_bar_SnS_leg()+
  theme( axis.title.x=element_blank())+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(breaks=c("a","b","c"),labels=c("1","2","3"))+
  scale_y_continuous(limits = c(0,85), expand = c(0, 0)) +
  geom_errorbar(aes(x = TSH, ymin = Lower, ymax = Upper,group = HerbTreat),position = dodge, width = 0.2)+
  labs(y = "Hemiptera/sample", x="Years Since Treatment", fill = "Herbicide Treatment")+
  ggtitle("A. Hemiptera")

print(Hemi.Plot)

#....................................................................................#####
#5. Araneae (Aran) Abundance ----
#....................................................................................#

#>>Stage 1: Aran, (StartTime)+OrdinalSamplingDate+(Winds)+Temp----
AICregress_Nuisance_mods(ArthPatch_Herb$Aran_NO,ArthPatch_Herb,nbinom1)
          #                      K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # SamplingCond_OrdDate 7 2656.806    0.0000 0.6744 0.6744 -1321.403
          # Global               8 2658.803    1.9969 0.2485 0.9229 -1321.401
          # OrdDate              4 2661.776    4.9705 0.0562 0.9791 -1326.888
          # OrdDate_Method       5 2663.752    6.9462 0.0209 1.0000 -1326.876
          # StartTime            4 2679.592   22.7859 0.0000 1.0000 -1335.796
          # SamplingCond         6 2680.334   23.5286 0.0000 1.0000 -1334.167
          # SamplingCond_Method  7 2682.312   25.5062 0.0000 1.0000 -1334.156
          # Winds                4 2685.622   28.8162 0.0000 1.0000 -1338.811
          # Null                 3 2685.762   28.9559 0.0000 1.0000 -1339.881
          # Temp                 4 2686.982   30.1763 0.0000 1.0000 -1339.491
          # SweepVac             4 2687.697   30.8908 0.0000 1.0000 -1339.848
Aran_Nuisance_mod = glmmTMB(Aran_NO~OrdinalSamplingDate+StartTime+Winds+Temp+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
confint(Aran_Nuisance_mod,level=0.85)
summary(Aran_Nuisance_mod)

#>>Stage 2: Aran, Null----
Aran_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~OrdinalSamplingDate+Temp+                                   (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  TSH                        = glmmTMB(y~OrdinalSamplingDate+Temp+TSH+                               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat                  = glmmTMB(y~OrdinalSamplingDate+Temp+HerbTreat_14.18+                   (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~OrdinalSamplingDate+Temp+GrazingTreat+                      (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH              = glmmTMB(y~OrdinalSamplingDate+Temp+HerbTreat_14.18*TSH+               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat_TSH           = glmmTMB(y~OrdinalSamplingDate+Temp+GrazingTreat+TSH+                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~OrdinalSamplingDate+Temp+GrazingTreat+HerbTreat_14.18+      (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~OrdinalSamplingDate+Temp+HerbTreat_14.18*TSH+GrazingTreat+  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

Aran_Herb_mods(ArthPatch_Herb$Aran_NO,ArthPatch_Herb) 
          #                             K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # TSH                         7 2608.037    0.0000 0.6586 0.6586 -1297.019
          # GrazingTreat_TSH            9 2609.430    1.3925 0.3283 0.9868 -1295.715
          # HerbTreat_TSH              13 2616.682    8.6448 0.0087 0.9956 -1295.341
          # HerbTreat_TSH_GrazingTreat 15 2618.036    9.9987 0.0044 1.0000 -1294.018
          # Null                        5 2654.571   46.5338 0.0000 1.0000 -1322.286
          # GrazingTreat                7 2656.470   48.4327 0.0000 1.0000 -1321.235
          # HerbTreat                   7 2658.191   50.1532 0.0000 1.0000 -1322.095
          # HerbTreat_GrazingTreat      9 2660.078   52.0408 0.0000 1.0000 -1321.039

#....................................................................................#####
#6. Coleopteran Abundance ----
#....................................................................................#

#>>Stage 1: Cole, StartTime+OrdinalSamplingDate+Winds+Temp+SweepVac----
AICregress_Nuisance_mods(ArthPatch_Herb$Cole_NO,ArthPatch_Herb,nbinom1)
          #                      K      AIC Delta_AIC AICWt Cum.Wt        LL
          # Global               8 2501.818    0.0000     1      1 -1242.909
          # SamplingCond_Method  7 2539.210   37.3915     0      1 -1262.605
          # OrdDate_Method       5 2543.884   42.0660     0      1 -1266.942
          # SweepVac             4 2582.171   80.3534     0      1 -1287.086
          # SamplingCond_OrdDate 7 2594.791   92.9730     0      1 -1290.396
          # Temp                 4 2611.758  109.9403     0      1 -1301.879
          # SamplingCond         6 2613.543  111.7246     0      1 -1300.771
          # OrdDate              4 2615.680  113.8623     0      1 -1303.840
          # Null                 3 2637.223  135.4049     0      1 -1315.612
          # Winds                4 2638.901  137.0823     0      1 -1315.450
          # StartTime            4 2639.211  137.3924     0      1 -1315.605

Cole_Nuisance_mod = glmmTMB(Cole_NO~OrdinalSamplingDate+StartTime+Winds+Temp+SweepVac+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
confint(Cole_Nuisance_mod,level=0.85)
summary(Cole_Nuisance_mod)

Cole_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+                                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  TSH                        = glmmTMB(y~StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+TSH+                              (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat                  = glmmTMB(y~StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+HerbTreat_14.18+                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+GrazingTreat+                     (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH              = glmmTMB(y~StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+HerbTreat_14.18*TSH+              (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat_TSH           = glmmTMB(y~StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+GrazingTreat+TSH+                 (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+GrazingTreat+HerbTreat_14.18+     (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~StartTime+OrdinalSamplingDate+SweepVac+Winds+Temp+HerbTreat_14.18*TSH+GrazingTreat+ (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

#>>Stage 2: Cole, Null----
Cole_Herb_mods(ArthPatch_Herb$Cole_NO,ArthPatch_Herb) 
          #                             K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # Null                        8 2501.818    0.0000 0.3223 0.3223 -1242.909
          # HerbTreat_TSH              16 2503.045    1.2266 0.1746 0.4969 -1235.522
          # TSH                        10 2503.186    1.3678 0.1627 0.6595 -1241.593
          # GrazingTreat               10 2504.090    2.2721 0.1035 0.7630 -1242.045
          # HerbTreat                  10 2504.194    2.3760 0.0983 0.8613 -1242.097
          # HerbTreat_TSH_GrazingTreat 18 2505.341    3.5230 0.0554 0.9166 -1234.671
          # GrazingTreat_TSH           12 2505.494    3.6761 0.0513 0.9679 -1240.747
          # HerbTreat_GrazingTreat     12 2506.433    4.6147 0.0321 1.0000 -1241.216    

#>>Top Model & Predicted Values: checked second top model, but CIs indicate no strong relationships and confirms AIC result----
Cole_Top = glmmTMB(Cole_NO~StartTime+OrdinalSamplingDate+Temp+HerbTreat_14.18*TSH+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
ggpredict(Cole_Top,c("HerbTreat_14.18", "TSH"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Cole_Pred = as.data.frame(ggpredict(Cole_Top,c("HerbTreat_14.18", "TSH"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
colnames(Cole_Pred)=c("HerbTreat", "Predicted","SE","Lower","Upper","TSH") #renames columns
#View(Cole_Pred) 

confint(Cole_Top, level=0.85)

Cole_Pred_Sum= Cole_Pred %>% 
  group_by(TSH, HerbTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
#View(Cole_Pred_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot Coles!!
Cole_Pred_Sum$HerbTreat=factor(Cole_Pred_Sum$HerbTreat,levels=c("Con","Spr","SnS"))
Cole.Plot=ggplot(data=Cole_Pred_Sum, y=Predicted, x=TSH)+  
  geom_bar(aes(x=TSH, y=Predicted,fill=HerbTreat), position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"), labels=c("Control", "Spray", "Spray-and-Seed"))+
  theme()+
  theme_bar_SnS_leg()+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(breaks=c("a","b","c"),labels=c("1","2","3"))+
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
  geom_errorbar(aes(x = TSH, ymin = Lower, ymax = Upper,group = HerbTreat),position = dodge, width = 0.2)+
  labs(y = "Coleoptera/sample",x="Grazing Treatment", fill="Herbicide Treatment")

print(Cole.Plot)
#....................................................................................#####
#7. Lepidopteran (Lepi) Abundance ----
#....................................................................................#

#>>Stage 1: Lepi, SweepVac----
AICregress_Nuisance_mods(ArthPatch_Herb$Lepi_NO,ArthPatch_Herb,nbinom1)
          #                      K      AIC Delta_AIC  AICWt Cum.Wt        LL
          # SweepVac             4 1846.922    0.0000 0.6082 0.6082 -919.4609
          # OrdDate_Method       5 1848.872    1.9501 0.2294 0.8376 -919.4360
          # SamplingCond_Method  7 1850.201    3.2790 0.1180 0.9556 -918.1004
          # Global               8 1852.163    5.2411 0.0443 0.9999 -918.0815
          # Null                 3 1866.550   19.6282 0.0000 0.9999 -930.2750
          # Winds                4 1867.338   20.4161 0.0000 0.9999 -929.6690
          # Temp                 4 1867.834   20.9124 0.0000 1.0000 -929.9171
          # StartTime            4 1868.277   21.3552 0.0000 1.0000 -930.1385
          # OrdDate              4 1868.440   21.5183 0.0000 1.0000 -930.2201
          # SamplingCond         6 1870.389   23.4671 0.0000 1.0000 -929.1945
          # SamplingCond_OrdDate 7 1872.382   25.4604 0.0000 1.0000 -929.1911

Lepi_Nuisance_mod = glmmTMB(Lepi_NO~SweepVac+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
confint(Lepi_Nuisance_mod,level=0.85)
summary(Lepi_Nuisance_mod)

Lepi_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~SweepVac+                                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  TSH                        = glmmTMB(y~SweepVac+TSH+                              (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat                  = glmmTMB(y~SweepVac+HerbTreat_14.18+                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~SweepVac+GrazingTreat+                     (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH              = glmmTMB(y~SweepVac+HerbTreat_14.18*TSH+              (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat_TSH           = glmmTMB(y~SweepVac+GrazingTreat+TSH+                 (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~SweepVac+GrazingTreat+HerbTreat_14.18+     (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~SweepVac+HerbTreat_14.18*TSH+GrazingTreat+ (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

#>>Stage 2: Lepi, HerbTreat_TSH----
Lepi_Herb_mods(ArthPatch_Herb$Lepi_NO,ArthPatch_Herb) 
#                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
# HerbTreat_TSH              12 1830.138    0.0000 0.7358 0.7358 -903.0692
# HerbTreat_TSH_GrazingTreat 14 1832.317    2.1790 0.2475 0.9833 -902.1587
# HerbTreat                   6 1838.343    8.2051 0.0122 0.9954 -913.1718
# HerbTreat_GrazingTreat      8 1840.664   10.5256 0.0038 0.9992 -912.3320
# TSH                         6 1845.213   15.0746 0.0004 0.9996 -916.6065
# Null                        4 1846.922   16.7834 0.0002 0.9998 -919.4609
# GrazingTreat_TSH            8 1847.176   17.0373 0.0001 0.9999 -915.5879
# GrazingTreat                6 1849.037   18.8982 0.0001 1.0000 -918.5183

#>>Top Model & Predicted Values----
Lepi_Top = glmmTMB(Lepi_NO~SweepVac+Temp+HerbTreat_14.18*TSH+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
#ggpredict(Lepi_Top,c("HerbTreat_14.18", "TSH","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Lepi_Pred = as.data.frame(ggpredict(Lepi_Top,c("HerbTreat_14.18", "TSH","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
colnames(Lepi_Pred)=c("HerbTreat", "Predicted","SE","Lower","Upper","TSH","SweepVac") #renames columns
#View(Lepi_Pred) 

confint(Lepi_Top, level=0.85)

Lepi_Pred_Sum= Lepi_Pred %>% 
  group_by(TSH, HerbTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
#View(Lepi_Pred_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot Lepis!!
Lepi_Pred_Sum$HerbTreat=factor(Lepi_Pred_Sum$HerbTreat,levels=c("Con","Spr","SnS"))
Lepi.Plot=ggplot(data=Lepi_Pred_Sum, y=Predicted, x=TSH)+  
  geom_bar(aes(x=TSH, y=Predicted,fill=HerbTreat), position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"), labels=c("Control", "Spray", "Spray-and-Seed"))+
  theme()+
  theme_bar_SnS_leg()+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(breaks=c("a","b","c"),labels=c("1","2","3"))+
  scale_y_continuous(limits = c(0,4), expand = c(0, 0)) +
  geom_errorbar(aes(x = TSH, ymin = Lower, ymax = Upper,group = HerbTreat),position = dodge, width = 0.2)+
  labs(y = "Lepidoptera/sample",x="Years Since Treatment", fill = "Herbicide Treatment")+
  ggtitle("C. Lepidoptera")

print(Lepi.Plot)

#bar graphs will be in combination with graze results, see 2_Arth_Abund_Graze

#__________________________####
#98. Creating the summary figure----

library("scales")

#extracting the data for the heatmap
Orth_Con= Orth_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Con")
Orth_SnS= Orth_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="SnS") 
Orth_Spr= Orth_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Spr")
Orth_comb <- cbind(Species="Orthoptera",Arth_Treatment="SnS",Orth_SnS[6],round(Orth_SnS[,2]/Orth_Con[,2],4))
Orth_comb_Spr <- cbind(Species="Orthoptera",Arth_Treatment="Spr",Orth_Spr[6],round(Orth_Spr[,2]/Orth_Con[,2],4))

Hemi_Con= Hemi_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Con")
Hemi_SnS= Hemi_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="SnS")
Hemi_Spr= Hemi_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Spr")
Hemi_comb <- cbind(Species="Hemiptera",Arth_Treatment="SnS",Hemi_SnS[6],round(Hemi_SnS[,2]/Hemi_Con[,2],4))
Hemi_comb_Spr <- cbind(Species="Hemiptera",Arth_Treatment="Spr",Hemi_Spr[6],round(Hemi_Spr[,2]/Hemi_Con[,2],4))

Lepi_Con= Lepi_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Con")
Lepi_SnS= Lepi_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="SnS")
Lepi_Spr= Lepi_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Spr")
Lepi_comb <- cbind(Species="Lepidoptera",Arth_Treatment="SnS",Lepi_SnS[6],round(Lepi_SnS[,2]/Lepi_Con[,2],4))
Lepi_comb_Spr <- cbind(Species="Lepidoptera",Arth_Treatment="Spr",Lepi_Spr[6],round(Lepi_Spr[,2]/Lepi_Con[,2],4))

Cole_Con= Cole_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Con")
Cole_SnS= Cole_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="SnS")
Cole_Spr= Cole_Pred %>% 
  group_by(HerbTreat) %>% 
  filter(HerbTreat =="Spr")
Cole_comb <- cbind(Species="Coleoptera",Arth_Treatment="SnS",Cole_SnS[6],round(Cole_SnS[,2]/Cole_Con[,2],4))
Cole_comb_Spr <- cbind(Species="Coleoptera",Arth_Treatment="Spr",Cole_Spr[6],round(Cole_Spr[,2]/Cole_Con[,2],4))


TSH=c("a","b","c")

#Because Aran top model was null, filling in all 1's since Con and Treatment are equal
Aran_comb <- data.frame(Species="Araneae",Arth_Treatment = "SnS", 
                        TSH = TSH, 
                        Predicted = 1, stringsAsFactors = FALSE)
Aran_comb_Spr <- data.frame(Species="Araneae",Arth_Treatment = "Spr", 
                            TSH = TSH, 
                            Predicted = 1, stringsAsFactors = FALSE)

All_Arths=rbind(Orth_comb,Orth_comb_Spr,
                Hemi_comb,Hemi_comb_Spr,
                Lepi_comb,Lepi_comb_Spr,
                Cole_comb,Cole_comb_Spr,
                Aran_comb,Aran_comb_Spr)

#All_Arths$Arth_Treatment=factor(All_Arths$Arth_Treatment,) #in case ordering needs to be changed

# Making the heatmap 
ArthSummaryFig =ggplot(All_Arths, aes(TSH, Arth_Treatment, fill= Predicted)) + 
  geom_tile()+
  scale_fill_gradientn(colours = c("darkorange3","gray95","deepskyblue3"), 
                       values = rescale(c(0,.5,2)),
                       guide = "colorbar")+
  facet_grid(Species~., space="free_x", scales="free_y", switch="y")+
  scale_y_discrete(expand=c(0,0),position = "left")+
  scale_x_discrete(expand=c(0,0),breaks=c("a","b","c"),labels=c("1","2","3"))+
  #coord_cartesian(clip="off") +
  #annotate("segment", x = 0.5, xend=0.5,y = 0.5, yend = 2.5, size=1) +
  labs(y= "", x =  "Years-Since-Treatment",fill="Treatment:Control")+
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

#panel.grid=element_blank()
#plot.title=element_text(hjust=0.5)
ArthSummaryFig
ggsave(filename="ArthSummaryFig.jpg", plot = ArthSummaryFig,
       scale = 1, width = 6, height = 8, units = c("in"),dpi = 300,path="/cloud/project/Figs")

