library(tidyverse)
library(tidyr)
library(reshape)
library(cowplot)

#set working directory
setwd("~/Documents/Documents - Molly’s MacBook Pro/Pubs/InPrep/AWeb_Report2020")

#Upload classifier data 
litdata<-read.csv("Datasets/LitSearch/AWebReport_2016to2020_LitSearch.csv")

jdata<-read.csv("AWebReport_2015to2020_JournalList.csv")


date<- "2021.10.5"


#percent change plot (if you want to reorder data in plot - x=reorder(X, Percent_Change))
p_change<-ggplot(data=litdata, aes(x=reorder(Category, Totals_CalculatedFromYears), y=(Percent_Change-100), fill = Percent_Change > 100)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("red","grey50")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(colour = "black", size =12), axis.text.x = element_text(colour = "black", size = 12, angle = 45, vjust = 0.95, hjust=0.95))
p_change<- p_change + labs(x ="", y ="Percent Change in Publications", colour="black")
p_change


mdata <- melt(litdata, id=c("Category","Percent_Change"))
mdata <-mdata[complete.cases(mdata), ]
#mdata <-mdata[which(mdata$value!=""), ]
mdata[] <- lapply(mdata, function(x) if(is.factor(x)) factor(x) else x)

mdata$value<- as.numeric(as.character(mdata$value))


#remove rows/species where ecology is unknown
mdata<-mdata[!(mdata$Category=="background pub rate"),]
mdata<-mdata[!(mdata$Category=="Amphibian_Base"),]
mdata<-mdata[!(mdata$variable=="Totals_CalculatedFromYears"),]
mdata<-mdata[!(mdata$variable=="Search.term"),]
mdata<-mdata[!(mdata$variable=="X5yr_searchlink"),]
mdata<-mdata[!(mdata$variable=="Percent_ModelOrganism"),]
mdata<-mdata[!(mdata$variable=="Total_ViaSearch"),]
mdata<-mdata[!(mdata$variable=="Totals_Including..xenopus..or..axolotl."),]
mdata[] <- lapply(mdata, function(x) if(is.factor(x)) factor(x) else x)

#2015 vs 2020 absolute number plot 
p_num<-ggplot(mdata, aes(fill=variable, y=value, x=reorder(Category, value))) +
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_viridis(direction=-1, discrete=TRUE) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(colour = "black", size =12), axis.text.x = element_text(colour = "black", size = 12, angle = 45, vjust = 0.95, hjust=0.95))
p_num

plot_grid(p_change, p_num, labels = c('A', 'B'), label_size = 12, ncol = 1)




##### PRINT LIT SEARCH FIG 1 #####
pdf(file = paste("LitSearchFigure",date ,".pdf", sep="_"), width = 12,  height = 8)
plot_grid(p_change, p_num, labels = c('A', 'B'), label_size = 12, ncol = 1)
dev.off()


##### MODEL SPECIES FIGURE #####
ndata <- melt(litdata, id=c("Category","Percent_Change"))
ndata<-ndata[!(ndata$Category=="background pub rate"),]
ndata<-ndata[!(ndata$Category=="Amphibian_Base"),]
ndata<-ndata[which(ndata$variable=="Totals_CalculatedFromYears" | ndata$variable=="Totals_Including..xenopus..or..axolotl."),]
ndata[] <- lapply(ndata, function(x) if(is.factor(x)) factor(x) else x)

ndata <-ndata[complete.cases(ndata$value), ]

ndata$value<- as.numeric(as.character(ndata$value))


q<-ggplot(ndata, aes(fill=variable, y=value, x=reorder(Category, value))) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("grey70","black")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(colour = "black", size =12), axis.text.x = element_text(colour = "black", size = 12, angle = 45, vjust = 0.95, hjust=0.95))
q<-q + labs(x ="", y ="Number of Publications", colour="black") 
q

#ndata<-ndata[order(ndata$value),]
p<-ggplot(ndata, aes(fill=variable, y=value, x=reorder(Category, value))) +
  geom_bar(position = "fill", stat="identity") + 
  scale_fill_manual(values=c("grey70","black")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(colour = "black", size =12), axis.text.x = element_text(colour = "black", size = 12, angle = 45, vjust = 0.95, hjust=0.95))
p<-p + labs(x ="", y ="Proportion of Publications", colour="black") 
p

##### PRINT LIT SEARCH FIG 1 #####
pdf(file = paste("PropModelOrgFigure",date ,".pdf", sep="_"), width = 10,  height = 10)
plot_grid(q, p, labels = c('A', 'B'), label_size = 12, ncol = 1)
dev.off()