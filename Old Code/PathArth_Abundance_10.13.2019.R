#Code adapted from: Timothy Swartz; timothy.swartz@temple.edu;Date: 30 June, 2019
#Edited by: Jaime Coon, Contact: jjcoon2@illinois.edu. 

#Start by loading libraries that contain all the functions/procedures we will use. 
    #I may have a few extras here that we don't need, but okay to load them all anyway. 
    #Note: these libraries must be installed first, and then you can load them.

#two levels of analysis: one with just herbicide sprayed pastures, and one across all pastures (n=18)

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
#1. Load Data----
#....................................................................................#

#Importing the data.
ArthPatch_All=read.csv("/cloud/project/Arth_Abund/PatchArthData_Landscape_9.10.2019.csv")
#View(ArthPatch_All)

#....................................................................................#####
#2.[Herb] Filtering and Preparation----
#....................................................................................#

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
#Filtering to just get the herbicide Pastures, minus STE 2015 (includes BSH, DUN, GIL, LTR, RC2, could add RCH2014 depending)
ArthPatch_Herb=Data_Cleaning (ArthPatch_All)
View(ArthPatch_Herb)


#Saving ArthPatch_Herb for Rachel

save(ArthPatch_Herb, file="ArthPatch_Herb.Rdata")

load("ArthPatch_Herb.Rdata")

view(ArthPatch_Herb.RData)
#....................................................................................#####
#3. Outlier analysis ----
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
#   Birdfood_Boxplot_Herb   + theme(legend.position="none"),
#   
#   align   = 'vh',
#   #labels = c("A", "B", "C"),
#   hjust   = -1,
#   nrow    = 4
# )
 #print(all.Herb.boxplot)   
    
#Outliers are often a problem with arthropod data, so I have beecapping experimenting with "capping"
  #This codereplaces all *extreme* outliers higher than 3*IQR. For less extreme outliers, could do 1.5*IQR
  #This code sets up a new variable "Orth_NO" (Orth No Outliers) that we can use in analysis. 
  #Needs to be done for all arthropod orders

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

# #also optional
# boxplot_no_outliers    =function     (df,y,title){
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
# Orth_NO_Boxplot_Herb   =boxplot_no_outliers(ArthPatch_Herb,ArthPatch_Herb$Orth_NO,       "Orth")
# Aran_NO_Boxplot_Herb   =boxplot_no_outliers(ArthPatch_Herb,ArthPatch_Herb$Aran_NO,       "Aran")
# Cole_NO_Boxplot_Herb   =boxplot_no_outliers(ArthPatch_Herb,ArthPatch_Herb$Cole_NO,       "Cole")
# Lepi_NO_Boxplot_Herb   =boxplot_no_outliers(ArthPatch_Herb,ArthPatch_Herb$Lepi_NO,       "Lepi")
# Hemi_NO_Boxplot_Herb   =boxplot_no_outliers(ArthPatch_Herb,ArthPatch_Herb$Hemi_NO,       "Hemi")
# Birdfood_NO_Boxplot_Herb   =boxplot_no_outliers(ArthPatch_Herb,ArthPatch_Herb$Birdfood_NO,  "Total")
# 
# all.HerbNO.boxplot <- plot_grid(
#   Orth_NO_Boxplot_Herb   + theme(legend.position="none"),
#   Aran_NO_Boxplot_Herb   + theme(legend.position="none"),
#   Cole_NO_Boxplot_Herb   + theme(legend.position="none"),
#   Lepi_NO_Boxplot_Herb   + theme(legend.position="none"),
#   Hemi_NO_Boxplot_Herb   + theme(legend.position="none"),
#   Birdfood_NO_Boxplot_Herb   + theme(legend.position="none"),
#   align   = 'vh',
#   #labels = c("A", "B", "C"),
#   hjust   = -1,
#   nrow    = 3
# )
# print(all.HerbNO.boxplot)

#....................................................................................####
#4.Choosing distributions----
#....................................................................................#

#optional - looking at how many detections we have
#Orth_det=sum(ArthPatch_Graze$Orth_NO_Gr)
#Aran_det=sum(ArthPatch_Graze$Aran_NO_Gr)
#Cole_det=sum(ArthPatch_Graze$Cole_NO_Gr)
#Lepi_det=sum(ArthPatch_Graze$Lepi_NO_Gr)
#Hemi_det=sum(ArthPatch_Graze$Hemi_NO_Gr)

#Det_list_Arth=c(Orth_det,Aran_det,Cole_det,Lepi_det,Hemi_det)

#examining plots
# norm1=ggdensity(ArthPatch_Herb$Aran_NO, 
#                 main = "Density plot",
#                 xlab = "bug")
# norm1
# norm2=ggqqplot(ArthPatch_Herb$Lepi_NO)
# norm2

AICregress_dist_mods <- function(y,df) { #AIC regression for distribution models
  
  Normal  = glmmTMB(y~1+(1|Past_Pat_Yr),REML="FALSE", family=gaussian, data=df) #normal distribution
  Poisson = glmmTMB(y~1+(1|Past_Pat_Yr),REML="FALSE", family=poisson, data=df) #poisson
  NegBin  = glmmTMB(y~1+(1|Past_Pat_Yr),REML="FALSE", family=nbinom1, data=df) #negative binomial
  ZIP     = glmmTMB(y~1+(1|Past_Pat_Yr),REML="FALSE", family=poisson, ziformula = ~1, data=df) #zero-inflated poisson

  mods=list(Normal,Poisson,NegBin, ZIP)
  names=c("Normal","Poisson","NegBin","ZIP")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 
#>>Orth: normal?----
AICregress_dist_mods(ArthPatch_Herb$Orth_NO,ArthPatch_Herb) #chosen dist = negbin or normal
    #         K      AIC Delta_AIC AICWt Cum.Wt        LL
    # NegBin  3 3616.084    0.0000     1      1 -1805.042
    # Normal  3 4138.806  522.7225     0      1 -2066.403
    # ZIP     3 6683.303 3067.2199     0      1 -3338.652
    # Poisson 2 6807.574 3191.4899     0      1 -3401.787


#>>Hemi: normal? ----
AICregress_dist_mods(ArthPatch_Herb$Hemi_NO,ArthPatch_Herb) #chosen dist =normal?
    #         K       AIC  Delta_AIC AICWt Cum.Wt        LL
    # NegBin  3  4674.820     0.0000     1      1 -2334.410
    # Normal  3  4987.539   312.7186     0      1 -2490.769
    # Poisson 2 14771.381 10096.5606     0      1 -7383.690
    # ZIP     3 14773.381 10098.5606     0      1 -7383.690

#>>Aran: normal?----
AICregress_dist_mods(ArthPatch_Herb$Aran_NO,ArthPatch_Herb) #chosen dist
      #         K      AIC Delta_AIC AICWt Cum.Wt        LL
      # NegBin  3 2678.122    0.0000     1      1 -1336.061
      # Normal  3 2895.573  217.4503     0      1 -1444.786
      # ZIP     3 3061.360  383.2379     0      1 -1527.680
      # Poisson 2 3072.725  394.6025     0      1 -1534.362

#>>Cole:normal----
AICregress_dist_mods(ArthPatch_Herb$Cole_NO,ArthPatch_Herb)
    #        K      AIC Delta_AIC AICWt Cum.Wt        LL
    # NegBin  3 2645.437    0.0000     1      1 -1319.718
    # Normal  3 3450.347  804.9101     0      1 -1722.173
    # ZIP     3 3766.235 1120.7984     0      1 -1880.118
    # Poisson 2 3867.519 1222.0823     0      1 -1931.759
#>>Lepi: ZIP----
AICregress_dist_mods(ArthPatch_Herb$Lepi_NO,ArthPatch_Herb)
      #         K      AIC Delta_AIC AICWt Cum.Wt         LL
      # NegBin  3 1841.694    0.0000     1      1  -917.8471
      # ZIP     3 1872.116   30.4216     0      1  -933.0579
      # Poisson 2 1907.010   65.3158     0      1  -951.5050
      # Normal  3 2065.361  223.6670     0      1 -1029.6806
#>>bird food:normal----
AICregress_dist_mods(ArthPatch_Herb$Birdfood_NO,ArthPatch_Herb) #chosen dist =
      #         K       AIC Delta_AIC AICWt Cum.Wt        LL
      # NegBin  3  5017.964     0.000     1      1 -2505.982
      # Normal  3  5239.670   221.706     0      1 -2616.835
      # Poisson 2 16914.190 11896.227     0      1 -8455.095
      # ZIP     3 16916.190 11898.227     0      1 -8455.095




#....................................................................................#

#....................................................................................#####
#5.Nuisance parameters----
#....................................................................................#

#test for correlations
#myvars1 = c("Winds", "Temp", "OrdinalSamplingDate","Temp","Round")
#check_correlations = ArthPatch_Herb[myvars1]
#cor(check_correlations)

#Rachel - can skip to here

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

AICregress_Nuisance_mods_ZIP <- function(y,df,f) { 
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
#>>Orthopterans: null ----
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

#>>Hemi: StartTime+SweepVac+(Winds)+(Temp)----
AICregress_Nuisance_mods(ArthPatch_Herb$Hemi_NO,ArthPatch_Herb,nbinom1)
      #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
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

#>>Aran: (StartTime)+OrdinalSamplingDate+(Winds)+Temp----
AICregress_Nuisance_mods(ArthPatch_Herb$Aran_NO,ArthPatch_Herb,nbinom1)
    #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
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

#>>Cole: StartTime+OrdinalSamplingDate+Winds+Temp+SweepVac----
AICregress_Nuisance_mods(ArthPatch_Herb$Cole_NO,ArthPatch_Herb,nbinom1)
      #                     K      AIC Delta_AIC AICWt Cum.Wt        LL
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

#>>Lepi: SweepVac----
AICregress_Nuisance_mods(ArthPatch_Herb$Lepi_NO,ArthPatch_Herb,nbinom1)
      #                     K      AIC Delta_AIC  AICWt Cum.Wt        LL
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


#....................................................................................#####
#6.Main effects models----
#....................................................................................#
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
Birdfood_Herb_mods=function(y,df) { 
  Null                       = glmmTMB(y~OrdinalSamplingDate+Temp+SweepVac+                                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  TSH                        = glmmTMB(y~OrdinalSamplingDate+Temp+SweepVac+TSH+                              (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat                  = glmmTMB(y~OrdinalSamplingDate+Temp+SweepVac+HerbTreat_14.18+                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~OrdinalSamplingDate+Temp+SweepVac+GrazingTreat+                     (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH              = glmmTMB(y~OrdinalSamplingDate+Temp+SweepVac+HerbTreat_14.18*TSH+              (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat_TSH           = glmmTMB(y~OrdinalSamplingDate+Temp+SweepVac+GrazingTreat+TSH+                 (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_GrazingTreat     = glmmTMB(y~OrdinalSamplingDate+Temp+SweepVac+GrazingTreat+HerbTreat_14.18+     (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbTreat_TSH_GrazingTreat = glmmTMB(y~OrdinalSamplingDate+Temp+SweepVac+HerbTreat_14.18*TSH+GrazingTreat+ (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  
  mods=list(Null, TSH,  HerbTreat,  GrazingTreat,  HerbTreat_TSH,  GrazingTreat_TSH,  HerbTreat_GrazingTreat,  HerbTreat_TSH_GrazingTreat)
  names=c("Null","TSH","HerbTreat","GrazingTreat","HerbTreat_TSH","GrazingTreat_TSH","HerbTreat_GrazingTreat","HerbTreat_TSH_GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} 

#>>Orthopterans:HerbTreat_TSH----
   Orth_Herb_mods(ArthPatch_Herb$Orth_NO,ArthPatch_Herb)
      #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbTreat_TSH              11 3590.321    0.0000 0.7077 0.7077 -1784.161
      # HerbTreat_TSH_GrazingTreat 13 3592.106    1.7843 0.2900 0.9978 -1783.053
      # TSH                         5 3602.598   12.2767 0.0015 0.9993 -1796.299
      # GrazingTreat_TSH            7 3604.100   13.7786 0.0007 1.0000 -1795.050
      # Null                        3 3694.717  104.3962 0.0000 1.0000 -1844.359
      # GrazingTreat                5 3695.302  104.9805 0.0000 1.0000 -1842.651
      # HerbTreat                   5 3695.574  105.2525 0.0000 1.0000 -1842.787
      # HerbTreat_GrazingTreat      7 3696.298  105.9772 0.0000 1.0000 -1841.149
#>>Hemi:HerbTreat_TSH----
   Hemi_Herb_mods(ArthPatch_Herb$Hemi_NO,ArthPatch_Herb)
      #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbTreat_TSH              13 4641.920    0.0000 0.4860 0.4860 -2307.960
      # HerbTreat                   7 4643.141    1.2216 0.2639 0.7499 -2314.571
      # HerbTreat_TSH_GrazingTreat 15 4644.417    2.4974 0.1394 0.8893 -2307.209
      # HerbTreat_GrazingTreat      9 4645.082    3.1621 0.1000 0.9893 -2313.541
      # TSH                         7 4650.667    8.7474 0.0061 0.9954 -2318.334
      # Null                        5 4652.805   10.8852 0.0021 0.9975 -2321.403
      # GrazingTreat_TSH            9 4653.196   11.2762 0.0017 0.9993 -2317.598
      # GrazingTreat                7 4654.946   13.0259 0.0007 1.0000 -2320.473
#>>Aran:TSH----
   Aran_Herb_mods(ArthPatch_Herb$Aran_NO,ArthPatch_Herb) 
      #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # TSH                         7 2608.037    0.0000 0.6586 0.6586 -1297.019
      # GrazingTreat_TSH            9 2609.430    1.3925 0.3283 0.9868 -1295.715
      # HerbTreat_TSH              13 2616.682    8.6448 0.0087 0.9956 -1295.341
      # HerbTreat_TSH_GrazingTreat 15 2618.036    9.9987 0.0044 1.0000 -1294.018
      # Null                        5 2654.571   46.5338 0.0000 1.0000 -1322.286
      # GrazingTreat                7 2656.470   48.4327 0.0000 1.0000 -1321.235
      # HerbTreat                   7 2658.191   50.1532 0.0000 1.0000 -1322.095
      # HerbTreat_GrazingTreat      9 2660.078   52.0408 0.0000 1.0000 -1321.039
#>>Cole: Null, but look at HerbTreat_TSH----
   Cole_Herb_mods(ArthPatch_Herb$Cole_NO,ArthPatch_Herb) 
      #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # Null                        8 2501.818    0.0000 0.3223 0.3223 -1242.909
      # HerbTreat_TSH              16 2503.045    1.2266 0.1746 0.4969 -1235.522
      # TSH                        10 2503.186    1.3678 0.1627 0.6595 -1241.593
      # GrazingTreat               10 2504.090    2.2721 0.1035 0.7630 -1242.045
      # HerbTreat                  10 2504.194    2.3760 0.0983 0.8613 -1242.097
      # HerbTreat_TSH_GrazingTreat 18 2505.341    3.5230 0.0554 0.9166 -1234.671
      # GrazingTreat_TSH           12 2505.494    3.6761 0.0513 0.9679 -1240.747
      # HerbTreat_GrazingTreat     12 2506.433    4.6147 0.0321 1.0000 -1241.216    

#>>Lepi: HerbTreat_TSH----
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
#>>Birdfood----
   Birdfood_Herb_mods(ArthPatch_Herb$Birdfood_NO,ArthPatch_Herb) 
      #                            K      AIC Delta_AIC  AICWt Cum.Wt        LL
      # HerbTreat_TSH              14 4978.713    0.0000 0.8083 0.8083 -2475.356
      # HerbTreat_TSH_GrazingTreat 16 4981.597    2.8842 0.1911 0.9994 -2474.798
      # TSH                         8 4993.799   15.0859 0.0004 0.9999 -2488.899
      # GrazingTreat_TSH           10 4996.567   17.8538 0.0001 1.0000 -2488.283
      # HerbTreat                   8 4999.858   21.1453 0.0000 1.0000 -2491.929
      # HerbTreat_GrazingTreat     10 5002.987   24.2745 0.0000 1.0000 -2491.494
      # Null                        6 5009.332   30.6194 0.0000 1.0000 -2498.666
      # GrazingTreat                8 5012.332   33.6195 0.0000 1.0000 -2498.166

#7.Final models and coefs
    #a. Orth
    HerbTreat_TSH_YesNo_Orth = glmmTMB(Orth_NO~StartTime+OrdinalSamplingDate_1+Round+HerbTreat*TSH_1+GrazingYesNo,REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
    #b. Aran (etc)

  #coefficients and confidence intervals in log scale

    #a. Orth
    summary(HerbTreat_TSH_GrazingTreat_Orth)#can examine some info about the model here, not required
    Orth_Coef_ConInt = confint(HerbTreat_TSH_GrazingTreat_Orth, level = 0.85) #Orthopteran coefficients and confidence intervals at alpha=0.15
    print(Orth_Coef_ConInt)
    #b. Aran


#....................................................................................#####
#7. Get predicted values from top models for graphing of results----
#....................................................................................#
dodge <- position_dodge(width=0.9) #(this is dumb, but not too hard to get around)

#>>orthopterans----
    
Orth_Top = glmmTMB(Orth_NO~HerbTreat_14.18*TSH+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
    ggpredict(Orth_Top,c("HerbTreat_14.18", "TSH"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
        Orth_Pred = as.data.frame(ggpredict(Orth_Top,c("HerbTreat_14.18", "TSH"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)) #turns predictions into a dataframe that we can more easily manipulate
        colnames(Orth_Pred)=c("HerbTreat", "Predicted","SE","Lower","Upper","TSH") #renames columns
        #View(Orth_Pred) 
        confint(Orth_Top, level = 0.85)
      
    # Orth_Pred_Sum= Orth_Pred %>% 
    #   group_by(TSH, HerbTreat) %>% 
    #   summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
    # View(Orth_Pred_Sum) #optional; viewing it so we can make sure things are working 

  #now time to plot orths!!
  Orth_Pred$HerbTreat_14.18=factor(Orth_Pred$HerbTreat,levels=c("Con","Spr","SnS"))
  Orth_Plot=ggplot(data=Orth_Pred, y=Predicted, x=TSH)+  
      geom_bar(aes(x=TSH, y=Predicted,fill=HerbTreat), position=dodge, stat="identity")+
      scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
      theme(text=element_text(size=10),
              axis.title=element_text(face="bold", size=10),
              axis.text=element_text(size=10,color="black"),
              axis.line=element_line(color="black",size=1),
              panel.background=element_rect("snow2"),
              panel.grid=element_blank(),
              legend.text=element_text(size=10, color="black"))+
              #plot.title=element_text(hjust=0.5)
      scale_x_discrete(breaks=c("a","b","c"),labels=c("1","2","3"))+
      scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
      geom_errorbar(aes(x = TSH, ymin = Lower, ymax = Upper,group = HerbTreat),position = dodge, width = 0.2)+
      labs(y = "Orthoptera/sample", x="Years Since Herbicide", fill = "Herbicide Treatment")
    
    print(Orth_Plot)

#>>Hemipterans----
    
  Hemi_Top = glmmTMB(Hemi_NO~StartTime+SweepVac+HerbTreat_14.18*TSH+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
    ggpredict(Hemi_Top,c("HerbTreat_14.18", "TSH","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
    Hemi_Pred = as.data.frame(ggpredict(Hemi_Top,c("HerbTreat_14.18", "TSH","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
    colnames(Hemi_Pred)=c("HerbTreat", "Predicted","SE","Lower","Upper","TSH","SweepVac") #renames columns
    #View(Hemi_Pred) 
    confint(Hemi_Top, level = 0.85)

 Hemi_Pred_Sum= Hemi_Pred %>% 
     group_by(TSH, HerbTreat) %>% 
     summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
     #View(Hemi_Pred_Sum) #optional; viewing it so we can make sure things are working 
    
  #now time to plot Hemis!!
    Hemi_Pred_Sum$HerbTreat_14.18=factor(Hemi_Pred_Sum$HerbTreat,levels=c("Con","Spr","SnS"))
    Hemi_Plot=ggplot(data=Hemi_Pred_Sum, y=Predicted, x=TSH)+  
      geom_bar(aes(x=TSH, y=Predicted,fill=HerbTreat), position=dodge, stat="identity")+
      scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"), labels=c("Control", "Spray", "Spray-and-Seed"))+
      theme(text=element_text(size=10),
            axis.title=element_text(face="bold", size=10),
            axis.text=element_text(size=10,color="black"),
            axis.line=element_line(color="black",size=1),
            panel.background=element_rect("snow2"),
            panel.grid=element_blank(),
            legend.text=element_text(size=10, color="black"),
            legend.title=element_text(size=10,color="black",face="bold"))+
      #plot.title=element_text(hjust=0.5)
      scale_x_discrete(breaks=c("a","b","c"),labels=c("1","2","3"))+
      scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
      geom_errorbar(aes(x = TSH, ymin = Lower, ymax = Upper,group = HerbTreat),position = dodge, width = 0.2)+
      labs(y = "Hemiptera/sample", x="Years Since Treatment", fill = "Herbicide Treatment")
    
    print(Hemi_Plot)

    
#>>Cole: NOTHING TO SEE HERE----
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
  Cole_Pred_Sum$HerbTreat_14.18=factor(Cole_Pred_Sum$HerbTreat,levels=c("Con","Spr","SnS"))
    Cole_Plot=ggplot(data=Cole_Pred_Sum, y=Predicted, x=TSH)+  
      geom_bar(aes(x=TSH, y=Predicted,fill=HerbTreat), position=dodge, stat="identity")+
      scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
      theme(text=element_text(size=10),
            axis.title=element_text(face="bold", size=10),
            axis.text=element_text(size=10,color="black"),
            axis.line=element_line(color="black",size=1),
            panel.background=element_rect("snow2"),
            panel.grid=element_blank(),
            legend.text=element_text(size=10, color="black"))+
      #plot.title=element_text(hjust=0.5)
      scale_x_discrete(breaks=c("a","b","c"),labels=c("1","2","3"))+
      scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
      geom_errorbar(aes(x = TSH, ymin = Lower, ymax = Upper,group = HerbTreat),position = dodge, width = 0.2)+
      labs(y = "Coleoptera/sample",x="Grazing Treatment")
    
    print(Cole_Plot)
    
#>>Lepi----
Lepi_Top = glmmTMB(Lepi_NO~SweepVac+Temp+HerbTreat_14.18*TSH+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Herb)
    ggpredict(Lepi_Top,c("HerbTreat_14.18", "TSH","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
    Lepi_Pred = as.data.frame(ggpredict(Lepi_Top,c("HerbTreat_14.18", "TSH","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
    colnames(Lepi_Pred)=c("HerbTreat", "Predicted","SE","Lower","Upper","TSH","SweepVac") #renames columns
    #View(Lepi_Pred) 
    
    confint(Lepi_Top, level=0.85)
    
    Lepi_Pred_Sum= Lepi_Pred %>% 
      group_by(TSH, HerbTreat) %>% 
      summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
    #View(Lepi_Pred_Sum) #optional; viewing it so we can make sure things are working 
    
    #now time to plot Lepis!!
Lepi_Pred_Sum$HerbTreat_14.18=factor(Lepi_Pred_Sum$HerbTreat,levels=c("Con","Spr","SnS"))
    Lepi_Plot=ggplot(data=Lepi_Pred_Sum, y=Predicted, x=TSH)+  
      geom_bar(aes(x=TSH, y=Predicted,fill=HerbTreat), position=dodge, stat="identity")+
      scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
      theme(text=element_text(size=10),
            axis.title=element_text(face="bold", size=10),
            axis.text=element_text(size=10,color="black"),
            axis.line=element_line(color="black",size=1),
            panel.background=element_rect("snow2"),
            panel.grid=element_blank(),
            legend.text=element_text(size=10, color="black"))+
      #plot.title=element_text(hjust=0.5)
      scale_x_discrete(breaks=c("a","b","c"),labels=c("1","2","3"))+
      scale_y_continuous(limits = c(0,4), expand = c(0, 0)) +
      geom_errorbar(aes(x = TSH, ymin = Lower, ymax = Upper,group = HerbTreat),position = dodge, width = 0.2)+
      labs(y = "Lepidoptera/sample",x="Years Since Treatment", fill = "Herbicide Treatment")
    
print(Lepi_Plot)

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||####
#G R A Z I N G
    
#....................................................................................#####
#2.[Graze] Filtering and Preparation----
#....................................................................................#
    
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
    #Filtering to just get the herbicide Pastures, minus STE 2015 (includes BSH, DUN, GIL, LTR, RC2, could add RCH2014 depending)
    ArthPatch_Graze=Data_Cleaning_Gr (ArthPatch_All)
    
    #View(ArthPatch_Graze)
    
    
 save(ArthPatch_Graze, file="ArthPatch_Graze.Rdata")
    
    #....................................................................................#####
    #3. Outlier analysis ----
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
ArthPatch_Graze$Birdfood_NO_Gr =cap_outliers(ArthPatch_Graze$Birdfood)
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

#....................................................................................####
#4.Choosing distributions----
#....................................................................................#
    
#examining plots
# norm1=ggdensity(ArthPatch_Herb$Aran_NO, 
#                     main = "Density plot",
#                     xlab = "bug")
# norm1
# norm2=ggqqplot(ArthPatch_Graze$Lepi_NO_Gr)
# norm2
#     
# AICregress_dist_mods <- function(y,df) { #AIC regression for distribution models
#   Normal  = glmmTMB(y~1+(1|Pasture),REML="FALSE", family=gaussian, data=df) #normal distribution
#   Poisson = glmmTMB(y~1+(1|Pasture),REML="FALSE", family=poisson, data=df) #poisson
#   NegBin  = glmmTMB(y~1+(1|Pasture),REML="FALSE", family=nbinom1, data=df) #negative binomial
#   ZIP     = glmmTMB(y~1+(1|Pasture),REML="FALSE", family=poisson, ziformula = ~1, data=df) #zero-inflated poisson
#      
#   mods=list(Normal,Poisson,NegBin, ZIP)
#   names=c("Normal","Poisson","NegBin","ZIP")
#   print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
#   } 

#>>Orth: negbin?----
  AICregress_dist_mods(ArthPatch_Graze$Orth_NO,ArthPatch_Graze) #chosen dist = negbin or normal
    #         K       AIC Delta_AIC AICWt Cum.Wt        LL
    # NegBin  3  9540.054     0.000     1      1 -4767.027
    # Normal  3 11026.126  1486.072     0      1 -5510.063
    # ZIP     3 18289.450  8749.395     0      1 -9141.725
    # Poisson 2 18738.123  9198.069     0      1 -9367.062
    
    

#>>Hemi: normal? ----
AICregress_dist_mods(ArthPatch_Graze$Hemi_NO_Gr,ArthPatch_Graze) #chosen dist =normal?
    #         K      AIC  Delta_AIC AICWt Cum.Wt         LL
    # NegBin  3 11982.74     0.0000     1      1  -5988.368
    # Normal  3 12864.91   882.1749     0      1  -6429.456
    # Poisson 2 34757.77 22775.0381     0      1 -17376.887
    # ZIP     3 34759.77 22777.0381     0      1 -17376.887
    
#>>Aran: negbin?----
AICregress_dist_mods(ArthPatch_Graze$Aran_NO_Gr,ArthPatch_Graze) #chosen dist
    #         K      AIC Delta_AIC AICWt Cum.Wt        LL
    # NegBin  3 7037.568    0.0000     1      1 -3515.784
    # Normal  3 7617.903  580.3349     0      1 -3805.951
    # ZIP     3 8010.680  973.1122     0      1 -4002.340
    # Poisson 2 8084.877 1047.3094     0      1 -4040.439
    
#>>Cole:negbin----
AICregress_dist_mods(ArthPatch_Graze$Cole_NO_Gr,ArthPatch_Graze)
    #        K      AIC Delta_AIC AICWt Cum.Wt        LL
    # NegBin  3 2645.437    0.0000     1      1 -1319.718
    # Normal  3 3450.347  804.9101     0      1 -1722.173
    # ZIP     3 3766.235 1120.7984     0      1 -1880.118
    # Poisson 2 3867.519 1222.0823     0      1 -1931.759
#>>Lepi: negbin----
AICregress_dist_mods(ArthPatch_Graze$Lepi_NO_Gr,ArthPatch_Graze)
    #        K      AIC Delta_AIC AICWt Cum.Wt        LL
    # NegBin  3 4658.711    0.0000     1      1 -2326.356
    # ZIP     3 4764.431  105.7201     0      1 -2379.216
    # Poisson 2 4843.052  184.3403     0      1 -2419.526
    # Normal  3 5282.120  623.4087     0      1 -2638.060
#>>bird food:negbin----
AICregress_dist_mods(ArthPatch_Graze$Birdfood_NO_Gr,ArthPatch_Graze) #chosen dist =
    #        K      AIC  Delta_AIC AICWt Cum.Wt         LL
    # NegBin  3 13004.82     0.0000     1      1  -6499.411
    # Normal  3 13638.33   633.5062     0      1  -6816.164
    # Poisson 2 41686.37 28681.5434     0      1 -20841.183
    # ZIP     3 41688.37 28683.5434     0      1 -20841.183
    

   
#....................................................................................#####
#5.[GRAZE] Nuisance parameters----
#....................................................................................#

#test for correlations
    # myvars1 = c("Winds", "Temp", "OrdinalSamplingDate","Temp","Round")
    # check_correlations = ArthPatch_Herb[myvars1]
    # cor(check_correlations)
    
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
#>>Orthopterans: Temp+SweepVac ----
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
  Orth_Nuisance_mod_Gr = glmmTMB(Orth_NO_Gr~Temp+SweepVac+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
  confint(Orth_Nuisance_mod_Gr,level=0.85)
    
#>>Hemi: StartTime+SweepVac+(Winds)+(Temp)----
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
    
#>>Aran: OrdinalSamplingDate----
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
    Aran_Nuisance_mod_Gr = glmmTMB(Aran_NO_Gr~OrdinalSamplingDate+StartTime+Winds+Temp+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
    confint(Aran_Nuisance_mod_Gr,level=0.85)
    summary(Aran_Nuisance_mod_Gr)
    
#>>Cole: StartTime+OrdinalSamplingDate+Winds+Temp+SweepVac----
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
    
#>>Lepi: SweepVac+OrdinalSamplingDate----
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
    
#....................................................................................#####
#6. [GRAZE] Main effects models----
#....................................................................................#
Orth_Graze_mods=function(y,df) { 
  Null                       = glmmTMB(y~Year+Temp+SweepVac+                               (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo                  = glmmTMB(y~Year+Temp+SweepVac+HerbYesNo_alltime+             (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_v_Grazing        = glmmTMB(y~Year+Temp+SweepVac+HerbYesNo_alltime*GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_GrazingTreat     = glmmTMB(y~Year+Temp+SweepVac+HerbYesNo_alltime+GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~Year+Temp+SweepVac+GrazingTreat+                  (1|Pasture),REML="FALSE", family=nbinom1, data=df)
 
  mods=list(Null,  HerbYesNo,  HerbYesNo_v_Grazing,  HerbYesNo_GrazingTreat,  GrazingTreat)
  names=c( "Null","HerbYesNo","HerbYesNo*Grazing",  "HerbYesNo+GrazingTreat","GrazingTreat")
  print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
} ##STOPPED HERE####
Hemi_Graze_mods=function(y,df) { 
  Null                       = glmmTMB(y~Year+StartTime+SweepVac+                                   (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo                  = glmmTMB(y~Year+StartTime+SweepVac+HerbYesNo_alltime+                 (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_v_GrazingTreat   = glmmTMB(y~Year+StartTime+SweepVac+HerbYesNo_alltime*GrazingTreat+    (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  HerbYesNo_GrazingTreat     = glmmTMB(y~Year+StartTime+SweepVac+HerbYesNo_alltime+GrazingTreat+    (1|Pasture),REML="FALSE", family=nbinom1, data=df)
  GrazingTreat               = glmmTMB(y~Year+StartTime+SweepVac+GrazingTreat+                      (1|Pasture),REML="FALSE", family=nbinom1, data=df)
      
      mods=list(Null,  HerbYesNo,  HerbYesNo_v_GrazingTreat,  HerbYesNo_GrazingTreat,  GrazingTreat)
      names=c( "Null","HerbYesNo","HerbYesNo*GrazingTreat",    "HerbYesNo+GrazingTreat","GrazingTreat")
      print(aictab(cand.set = mods, modnames = names,second.ord = FALSE), digits = 4)
    } 
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
    
#>>Orthopterans: HerbYesNo+GrazingTreat----
Orth_Graze_mods(ArthPatch_Graze$Orth_NO_Gr,ArthPatch_Graze)
    #                         K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbYesNo+GrazingTreat 10 9455.387    0.0000 0.4230 0.4230 -4717.694
    # HerbYesNo               8 9455.535    0.1478 0.3929 0.8159 -4719.768
    # HerbYesNo*Grazing      12 9457.056    1.6687 0.1836 0.9995 -4716.528
    # Null                    7 9470.068   14.6803 0.0003 0.9998 -4728.034
    # GrazingTreat            9 9470.476   15.0884 0.0002 1.0000 -4726.238

#>>Hemi:HerbYesNo+GrazingTreat----
Hemi_Graze_mods(ArthPatch_Graze$Hemi_NO_Gr,ArthPatch_Graze)
    #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbYesNo+GrazingTreat 10 11875.88    0.0000 0.4023 0.4023 -5927.938
    # HerbYesNo               8 11876.19    0.3184 0.3431 0.7454 -5930.097
    # HerbYesNo*GrazingTreat 12 11876.79    0.9150 0.2546 1.0000 -5926.395
    # Null                    7 11896.90   21.0279 0.0000 1.0000 -5941.452
    # GrazingTreat            9 11897.78   21.8996 0.0000 1.0000 -5939.887 

#>>Aran:null -->could check HerbYesNo----
Aran_Graze_mods(ArthPatch_Graze$Aran_NO_Gr,ArthPatch_Graze) 
    #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # Null                    6 6864.991    0.0000 0.4595 0.4595 -3426.495
    # HerbYesNo               7 6866.077    1.0866 0.2669 0.7265 -3426.039
    # GrazingTreat            8 6867.332    2.3408 0.1426 0.8690 -3425.666
    # HerbYesNo+GrazingTreat  9 6868.409    3.4184 0.0832 0.9522 -3425.205
    # HerbYesNo*Grazing      11 6869.518    4.5270 0.0478 1.0000 -3423.759
#>>Cole: Null, HerbYesNo*Grazing----
Cole_Graze_mods(ArthPatch_Graze$Cole_NO_Gr,ArthPatch_Graze) 
    #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbYesNo*Grazing      15 6655.510    0.0000 0.6323 0.6323 -3312.755
    # GrazingTreat           12 6657.544    2.0338 0.2287 0.8610 -3316.772
    # HerbYesNo+GrazingTreat 13 6658.920    3.4101 0.1149 0.9759 -3316.460
    # Null                   10 6662.903    7.3933 0.0157 0.9916 -3321.452
    # HerbYesNo              11 6664.143    8.6331 0.0084 1.0000 -3321.072
#>>Lepi: HerbYesNo+GrazingTreat----
Lepi_Graze_mods(ArthPatch_Graze$Lepi_NO_Gr,ArthPatch_Graze) 
    #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbYesNo+GrazingTreat 10 4642.030    0.0000 0.4776 0.4776 -2311.015
    # HerbYesNo               8 4642.202    0.1726 0.4381 0.9158 -2313.101
    # HerbYesNo*Grazing      12 4645.542    3.5123 0.0825 0.9983 -2310.771
    # Null                    7 4654.587   12.5573 0.0009 0.9992 -2320.294
    # GrazingTreat            9 4654.721   12.6910 0.0008 1.0000 -2318.360
#>>Birdfood----
Birdfood_Graze_mods(ArthPatch_Graze$Birdfood_NO_G,ArthPatch_Graze) 
    #                        K      AIC Delta_AIC  AICWt Cum.Wt        LL
    # HerbYesNo               9 12858.43    0.0000 0.6103 0.6103 -6420.216
    # HerbYesNo+GrazingTreat 11 12860.02    1.5830 0.2766 0.8869 -6419.008
    # HerbYesNo*Grazing      13 12861.80    3.3708 0.1131 1.0000 -6417.902
    # Null                    8 12880.86   22.4313 0.0000 1.0000 -6432.432
    # GrazingTreat           10 12883.02   24.5853 0.0000 1.0000 -6431.509


#...............................................................................................####
#7.[GRAZE]nFinal models and coefs####
dodge <- position_dodge(width=0.9) #(this is dumb, but not too hard to get around)

#>>orthopterans----
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
Orth_Plot_Gr=ggplot(data=Orth_Pred_Gr_Sum, y=Predicted, x=GrazingTreat)+  
  geom_bar(aes(x=GrazingTreat, y=Predicted,fill=HerbYesNo), position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.text=element_text(size=10, color="black"))+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(labels=c("Early-\nIntensive","Season-\nLong","None"))+
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_errorbar(aes(x = GrazingTreat, ymin = Lower, ymax = Upper,group = HerbYesNo),position = dodge, width = 0.2)+
  labs(y = "Orthoptera/sample",x="Grazing Treatment",fill="Herbicide Treatment")
    
print(Orth_Plot_Gr)
    
#>>Hemipterans----
    
Hemi_Top_Gr = glmmTMB(Hemi_NO_Gr~Year+StartTime+SweepVac+HerbYesNo_alltime+GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
ggpredict(Hemi_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","GrazingTreat","Year"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Hemi_Pred_Gr = as.data.frame(ggpredict(Hemi_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","SweepVac","Year"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
colnames(Hemi_Pred_Gr)=c("HerbYesNo", "Predicted","SE","Lower","Upper","GrazingTreat","SweepVac","Year") #renames columns
#View(Hemi_Pred) 
    
Hemi_Pred_Gr_Sum= Hemi_Pred_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
#View(Hemi_Pred_Gr_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot Hemis!!
Hemi_Pred_Gr_Sum$GrazingTreat=factor(Hemi_Pred_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None"))
Hemi_Plot_Gr=ggplot(data=Hemi_Pred_Gr_Sum, y=Predicted, x=GrazingTreat)+  
  geom_bar(aes(x=GrazingTreat, y=Predicted,fill=HerbYesNo), position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.text=element_text(size=10, color="black"),
        legend.title=element_text(size=10, color="black", face="bold"))+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(labels=c("Early-\nIntensive","Season-\nLong","None"))+
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  geom_errorbar(aes(x = GrazingTreat, ymin = Lower, ymax = Upper,group = HerbYesNo),position = dodge, width = 0.2)+
  labs(y = "Hemiptera/sample",x="Grazing Treatment",fill="Herbicide Applied?")

print(Hemi_Plot_Gr)

    
#>>Cole: something to see here!----
Cole_Top_Gr = glmmTMB(Cole_NO_Gr~Year+StartTime+OrdinalSamplingDate+Temp+Winds+SweepVac+HerbYesNo_alltime*GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
ggpredict(Cole_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","Year","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Cole_Pred_Gr = as.data.frame(ggpredict(Cole_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","Year","SweepVac"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
colnames(Cole_Pred_Gr)=c("HerbYesNo", "Predicted","SE","Lower","Upper","GrazingTreat","Year","SweepVac") #renames columns
#View(Cole_Pred_Gr) 

Cole_Pred_Gr_Sum= Cole_Pred_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
#View(Cole_Pred_Gr_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot Coles!!
Cole_Pred_Gr_Sum$GrazingTreat=factor(Cole_Pred_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None"))
Cole_Plot_Gr=ggplot(data=Cole_Pred_Gr_Sum, y=Predicted, x=GrazingTreat)+  
  geom_bar(aes(x=GrazingTreat, y=Predicted,fill=HerbYesNo), position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=10),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.text=element_text(size=10, color="black"))+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(labels=c("Early-\nIntensive","Season-\nLong","None"))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10),limits = c(0,10), expand = c(0, 0)) +
  geom_errorbar(aes(x = GrazingTreat, ymin = Lower, ymax = Upper,group = HerbYesNo),position = dodge, width = 0.2)+
  labs(y = "Coleoptera/sample", x="Grazing Treatment", fill = "Herbicide Treatment")

print(Cole_Plot_Gr)
    
#>>Lepi----
Lepi_Top_Gr = glmmTMB(Lepi_NO_Gr~Year+SweepVac+OrdinalSamplingDate+HerbYesNo_alltime+GrazingTreat+(1|Pasture),REML="FALSE", family=nbinom1, data=ArthPatch_Graze)
ggpredict(Lepi_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","SweepVac","Year"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
Lepi_Pred_Gr = as.data.frame(ggpredict(Lepi_Top_Gr,c("HerbYesNo_alltime", "GrazingTreat","SweepVac","Year"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
colnames(Lepi_Pred_Gr)=c("HerbYesNo", "Predicted","SE","Lower","Upper","GrazingTreat","SweepVac","Year") #renames columns
#View(Lepi_Pred_Gr) 

Lepi_Pred_Gr_Sum= Lepi_Pred_Gr %>% 
  group_by(HerbYesNo, GrazingTreat) %>% 
  summarise_at(vars(Predicted, Lower, Upper), mean) #averages out the grazing treatment
#View(Lepi_Pred_Gr_Sum) #optional; viewing it so we can make sure things are working 

#now time to plot Lepis!!
Lepi_Pred_Gr_Sum$GrazingTreat=factor(Lepi_Pred_Gr_Sum$GrazingTreat,levels=c("IES","SLS","None"))
Lepi_Plot_Gr=ggplot(data=Lepi_Pred_Gr_Sum, y=Predicted, x=GrazingTreat)+  
  geom_bar(aes(x=GrazingTreat, y=Predicted,fill=HerbYesNo), position=dodge, stat="identity")+
  scale_fill_manual(values=c("goldenrod3","darkseagreen4","darkslategray"))+
  theme(text=element_text(size=10),
        axis.title.x=element_text(face="bold", size=10),
        axis.text=element_text(size=10,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.title=element_text(color="white"),
        axis.title.y=element_text(size=10, color="white"))+
  #plot.title=element_text(hjust=0.5)
  scale_x_discrete(labels=c("Early-\nIntensive","Season-\nLong","None"))+
  scale_y_continuous(limits = c(0,4), expand = c(0, 0)) +
  geom_errorbar(aes(x = GrazingTreat, ymin = Lower, ymax = Upper,group = HerbYesNo),position = dodge, width = 0.2)+
  labs(y = "Lepidoptera/sample", x="Grazing Treatment", fill = "Herbicide Treatment")


print(Lepi_Plot_Gr)

library(egg)
#put all plots together####

Hemi.all.plot=ggarrange(
  Hemi_Plot + 
    theme(legend.position="none",
          axis.title.x=element_blank()),
  
  Hemi_Plot_Gr+
    theme(legend.position  = "none",
              #axis.text.y  = element_blank(),
              #axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank()),

  nrow = 1)

Orth.all.plot=ggarrange(
  #top=grid::textGrob("Orthoptera", gp=gpar(fontsize=10),y=0.5, x=0.53),

  Orth_Plot + 
    theme(legend.position="none",
          axis.title.x = element_blank()), 
  Orth_Plot_Gr+
    theme(legend.position = "none",
          #axis.text.y  = element_blank(),
          #axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank()),
                        nrow = 1)
Lepi.all.plot=ggarrange(
  
  Lepi_Plot + 
    theme(legend.position="none"),
          #axis.title.x = element_blank()),
  
          
  Lepi_Plot_Gr+
    theme(legend.position = "none",
          axis.title.x = element_blank()),
          #axis.text.y  = element_blank(),
          #axis.ticks.y = element_blank(),
          #axis.title.y = element_blank()),
          nrow = 1)

Cole_plot_arrange=ggarrange(
  Cole_Plot_Gr+
    theme(legend.position = "none"),
  nrow = 1)    

legend_1 <- get_legend(
  Hemi_Plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "left",
          legend.text=element_text(size=10)))
  
legend_2<-get_legend(Hemi_Plot_Gr + 
                       guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "right",
                             legend.text=element_text(size=10)))

Legends_arrange=grid.arrange(
  legend_1,
  legend_2,
  ncol=1
)

Cole_legends=
  grid.arrange(Legends_arrange,Cole_plot_arrange,
               nrow=1)


ArthAbund_All_Plot=plot_grid(Hemi.all.plot,Orth.all.plot,Lepi.all.plot,Cole_legends,
  nrow=4,ncol=1)
ArthAbund_All_Plot

ggsave(filename="ArthPatch_Result.jpg", plot = ArthAbund_All_Plot, path = Jaime_Mac_save_path,
       scale = 1, width = 6.5, height = 9, units = c("in"),dpi = 300)

    

