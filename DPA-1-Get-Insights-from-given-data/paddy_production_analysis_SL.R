library(ggplot2)
#Load data files
df_yala=read.csv("paddy_production_in_yala_season_1952_2012.csv",header=TRUE)
df_maha=read.csv("paddy_production_in_maha_season_1952-2012.csv",header=TRUE)

#Fileter necessary columns
df_yala=df_yala[60,]
df_maha=df_maha[61,]
#df_yala
#df_maha

#Replace Columns 'Yala.Season' and 'Maha.Season' with Season 
colnames(df_yala)[1]<-"Season"
colnames(df_maha)[1]<-"Season"
df_yala$Season[1]<-"Yala"
df_maha$Season[1]<-"Maha"
#Conncet two dataframes into one dataframe
df=rbind(df_yala,df_maha)
df

#Analyse the dataset
pie_sown <- ggplot(df, aes(x="", y=Sown.000.Ha, fill=Season)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(Sown.000.Ha))), size=5, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Group"))+
  ggtitle("Yala Vs Maha Swon")
pie_sown

pie_harvast <- ggplot(df, aes(x="", y=Harvested.000.Ha., fill=Season)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(df$Harvested.000.Ha))), size=5, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Group"))+
  ggtitle("Yala Vs Maha Harvast")
pie_harvast

pie_yield <- ggplot(df, aes(x="", y=Average.Yield.Kg.Ha, fill=Season)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(Average.Yield.Kg.Ha))), size=5, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Group"))+
  ggtitle("Yala Vs Maha Yield")
pie_yield

pie_production <- ggplot(df, aes(x="", y=df$Production.000.Mt., fill=Season)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(df$Production.000.Mt))), size=5, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Group"))+
  ggtitle("Yala Vs Maha Production")
pie_production
