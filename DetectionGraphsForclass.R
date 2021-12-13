#detection graph for class
BOBO_Top_Gr=pcount(~Obs+DOY+Wind   ~Herb_Prop+Tree_Prop+Year+HerbYesNo*GrazingTreat+offset(log(Area_ha)), data=BOBO_PCount_Gr, mixture="ZIP",K=100)

newdata_det = data.frame(Obs=c("BSV", "BSV", "BSV", "BSV", "BSV", "BSV", "BSV", "BSV", "BSV", "BSV", "BSV", "BSV",
                               "JD",  "JD",  "JD",  "JD",  "JD",  "JD",  "JD",  "JD",  "JD",  "JD",  "JD",  "JD",
                               "JJC", "JJC", "JJC", "JJC", "JJC", "JJC", "JJC", "JJC", "JJC", "JJC", "JJC", "JJC",
                               "SBN", "SBN", "SBN", "SBN", "SBN", "SBN", "SBN", "SBN", "SBN", "SBN", "SBN", "SBN",
                               "SR",  "SR",  "SR",  "SR",  "SR",  "SR",  "SR",  "SR",  "SR",  "SR",  "SR",  "SR",
                               "TMS", "TMS", "TMS", "TMS", "TMS", "TMS", "TMS", "TMS", "TMS", "TMS", "TMS", "TMS",
                               "ALL", "ALL", "ALL", "ALL", "ALL", "ALL", "ALL", "ALL", "ALL", "ALL", "ALL", "ALL",
                               "ACW", "ACW", "ACW", "ACW", "ACW", "ACW", "ACW", "ACW", "ACW", "ACW", "ACW", "ACW"),
                         Robel=c(1,2,3,4,5,6,7,8,9,10,11,12)
)

det_pred=predict(EAME_Top_Gr, type = "det", newdata = newdata_det, level=0.85,appendData = T)

det_pred_sum=det_pred %>% 
  group_by(Robel) %>% 
  summarise_at(vars(Predicted, lower, upper), mean,na.rm=T)

GRSP.det.plot.obs <- ggplot(data = det_pred_sum)+ #set the data source for the plot
  geom_bar(aes(y=Predicted,  
               x=Obs, 
               fill=Obs), #what you want to use as your treatment to color the boxes
           position=dodge, # you can also leave position blank and it will stack them
           stat="identity")+ #identity makes it use value provided, it can do stats here if you want
  #scale_y_continuous(breaks=c(0,1,2,3,4),limits = c(0,4), expand = c(0, 0)) +
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5))+
  # scale_x_discrete(labels=c("Intensive-Early","Season-Long","None"))+
  geom_errorbar(aes(x = Obs, 
                    ymin = lower, 
                    ymax = upper), #you need to use group so it knows to distribute the bars across your groups
                position = dodge, 
                width = 0.2) #this is the width of the error bar ends
GRSP.det.plot.obs

GRSP.det.plot.DOY <- ggplot(data = det_pred_sum)+ #set the data source for the plot
  geom_smooth(aes(y=Predicted,  
                  x=Robel,ymin=lower,ymax=upper), #what you want to use as your treatment to color the boxes
              stat="identity"
  )+ #identity makes it use value provided, it can do stats here if you want
  #scale_y_continuous(breaks=c(0,1,2,3,4),limits = c(0,4), expand = c(0, 0)) +
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        legend.text=element_text(size=12, color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5))
# scale_x_discrete(labels=c("Intensive-Early","Season-Long","None"))+
GRSP.det.plot.DOY