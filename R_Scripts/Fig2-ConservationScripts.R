#State of the Amphibia Review Article#
#Conservation segment                #
#Date : 21 July 2021                 #
#Author : Emma Steigerwald           #
######################################

#Set up working environment
setwd("G:/My Drive/Outreach/AmphibiaWeb/AnnualReview")
library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)
library(cowplot)
library(extrafont)
###################################################################################################

IUCNColors <- c("DD" = "Grey", "In assessment"="white", "LC" = "Chartreuse4", "NT"="Greenyellow", "VU"="yellow", "EN"="Orange", "CR"="Red", "EW"="Darkmagenta", "EX"="black")


#PLOT NUMBER OF SPECIES IN EACH ORDER SUFFERING FROM EACH THREAT

df_1 <- read.csv("Aweb-threats_2021-09-21.csv")

#What proportion of amphibian species are logged as declining due to at least one of these factors?
length(unique(df_1$Scientific.Name))/2364
#53.3%

#There are currently 2364 species with accounts on amphibiaweb
#What proportion are declining due to climate?
length(unique(df_1$Scientific.Name[df_1$Category=="Climatic factors"]))/2364      
#7.2%
#Proportion of these endangered, critically endangered, or extinct?
length(unique(df_1$Scientific.Name[(df_1$IUCN.Red.List.Status=="EX/EW" | df_1$IUCN.Red.List.Status=="CR" | df_1$IUCN.Red.List.Status=="EN") & df_1$Category=="Climatic factors"]))/length(unique(df_1$Scientific.Name[df_1$Category=="Climatic factors"]))
#44.1%

#What proportion are declining due to disease/immune?
length(unique(df_1$Scientific.Name[df_1$Category=="Disease and immune functioning"]))/2364       
#6.3%
length(unique(df_1$Scientific.Name[(df_1$IUCN.Red.List.Status=="EX/EW" | df_1$IUCN.Red.List.Status=="CR" | df_1$IUCN.Red.List.Status=="EN") & df_1$Category=="Disease and immune functioning"]))/length(unique(df_1$Scientific.Name[df_1$Category=="Disease and immune functioning"]))
#59.1%

#What proportion are declining due to genetic degradation?
length(unique(df_1$Scientific.Name[df_1$Category=="Genetic degradation"]))/2364      
#2.1%
length(unique(df_1$Scientific.Name[(df_1$IUCN.Red.List.Status=="EX/EW" | df_1$IUCN.Red.List.Status=="CR" | df_1$IUCN.Red.List.Status=="EN") & df_1$Category=="Genetic degradation"]))/length(unique(df_1$Scientific.Name[df_1$Category=="Genetic degradation"]))
#56.0%

#What proportion are declining due to habitat alteration and loss?
length(unique(df_1$Scientific.Name[df_1$Category=="Habitat alteration and loss"]))/2364      
#46.6%
#Combined with resource exploitation, hydrology, urban?
length(unique(df_1$Scientific.Name[df_1$Category=="Intentional changes to hydrology" | df_1$Category=="Roads and urban development" | df_1$Category=="Habitat alteration and loss" | df_1$Category=="Resource exploitation"]))/2364      
#51.2%, 1211
length(unique(df_1$Scientific.Name[(df_1$IUCN.Red.List.Status=="EX/EW" | df_1$IUCN.Red.List.Status=="CR" | df_1$IUCN.Red.List.Status=="EN") & (df_1$Category=="Intentional changes to hydrology" | df_1$Category=="Roads and urban development" | df_1$Category=="Habitat alteration and loss")]))/length(unique(df_1$Scientific.Name[df_1$Category=="Intentional changes to hydrology" | df_1$Category=="Roads and urban development" | df_1$Category=="Habitat alteration and loss"]))
#34.8%

#What proportion are declining due to hydrology?
length(unique(df_1$Scientific.Name[df_1$Category=="Intentional changes to hydrology"]))/2364      
#5.9% 
length(unique(df_1$Scientific.Name[(df_1$IUCN.Red.List.Status=="EX/EW" | df_1$IUCN.Red.List.Status=="CR" | df_1$IUCN.Red.List.Status=="EN") & df_1$Category=="Intentional changes to hydrology"]))/length(unique(df_1$Scientific.Name[df_1$Category=="Intentional changes to hydrology"]))
#30.2%

#What proportion are declining due to intentional mortality?
length(unique(df_1$Scientific.Name[df_1$Category=="Intentional mortality"]))/2364       
#5.3%
length(unique(df_1$Scientific.Name[(df_1$IUCN.Red.List.Status=="EX/EW" | df_1$IUCN.Red.List.Status=="CR" | df_1$IUCN.Red.List.Status=="EN") & df_1$Category=="Intentional mortality"]))/length(unique(df_1$Scientific.Name[df_1$Category=="Intentional mortality"]))
#34.4%

#What proportion are declining due to pollutants?
length(unique(df_1$Scientific.Name[df_1$Category=="Pollutants"]))/2364       
#9.5% (225 species)
length(unique(df_1$Scientific.Name[(df_1$IUCN.Red.List.Status=="EX/EW" | df_1$IUCN.Red.List.Status=="CR" | df_1$IUCN.Red.List.Status=="EN") & df_1$Category=="Pollutants"]))/length(unique(df_1$Scientific.Name[df_1$Category=="Pollutants"]))
#29.3%

#What proportion are declining due to predators/competitors?
length(unique(df_1$Scientific.Name[df_1$Category=="Predators and competitors"]))/2364      
#5.0%
length(unique(df_1$Scientific.Name[(df_1$IUCN.Red.List.Status=="EX/EW" | df_1$IUCN.Red.List.Status=="CR" | df_1$IUCN.Red.List.Status=="EN") & df_1$Category=="Predators and competitors"]))/length(unique(df_1$Scientific.Name[df_1$Category=="Predators and competitors"]))
#47.1%

#What proportion are declining due to resource exploitation?
length(unique(df_1$Scientific.Name[df_1$Category=="Resource exploitation"]))/2364       
#38.2%
length(unique(df_1$Scientific.Name[(df_1$IUCN.Red.List.Status=="EX/EW" | df_1$IUCN.Red.List.Status=="CR" | df_1$IUCN.Red.List.Status=="EN") & df_1$Category=="Resource exploitation"]))/length(unique(df_1$Scientific.Name[df_1$Category=="Resource exploitation"]))
#34.6%

#What proportion are declining due to urban development?
length(unique(df_1$Scientific.Name[df_1$Category=="Roads and urban development"]))/2364       
#15.7%
length(unique(df_1$Scientific.Name[(df_1$IUCN.Red.List.Status=="EX/EW" | df_1$IUCN.Red.List.Status=="CR" | df_1$IUCN.Red.List.Status=="EN") & df_1$Category=="Roads and urban development"]))/length(unique(df_1$Scientific.Name[df_1$Category=="Roads and urban development"]))
#28.4%

df_sum <- df_1 %>%
  dplyr::group_by(Order, Category, IUCN.Red.List.Status) %>%
  dplyr::summarise(nSpecies=length(unique(Scientific.Name)))
#Configure to make stacked bar chart
df_sum$Category <- as.factor(df_sum$Category)
df_sum$IUCN.Red.List.Status <- df_sum$IUCN.Red.List.Status
#make numbers proportional
#anuran species:7404
#caudata: 766
#gymnophiona: 214

for (row in 1:nrow(df_sum)) {
     if (df_sum$Order=="Anura") {
          df_sum$propSpec <- df_sum$nSpecies/7404
          }
     else if (df_sum$Order=="Caudata") {
          df_sum$propSpec <- df_sum$nSpecies/766
          }
else if (df_sum$Order=="Gymnophiona") {
          df_sum$propSpec <- df_sum$nSpecies/214
          }
     }

#ordered by endangerement
df_sum$IUCN.Red.List.Status <- factor(df_sum$IUCN.Red.List.Status, levels = rev(c("EX/EW", "CR", "EN", "VU", "NT", "LC", "DD")))
#order chart by numbers
df_sum$Category = factor(df_sum$Category, levels = c('Habitat alteration and loss', 'Resource exploitation', 'Roads and urban development', 'Intentional changes to hydrology', 'Pollutants', 'Climatic factors', 'Disease and immune functioning', 'Predators and competitors', 'Intentional mortality', 'Genetic degradation'))

labels <- c('Anura\n\n7404 species\n(1901 accounts)', 'Caudata\n\n766 species\n(398 accounts)', 'Gymnophiona\n\n214 species\n(65 accounts)')
df_sum$Order <- factor(df_sum$Order, levels = c('Anura', 'Caudata', 'Gymnophiona'), labels = labels)                   
                   
tiff("Figure2_v2.tiff", units="in", width=7.5, height=4.8, res=400)
ggplot(data=df_sum, aes(x=Category, y=nSpecies, fill=IUCN.Red.List.Status)) +
  geom_bar(stat="identity", color="black", size=0.4, width=0.8)+
  labs(title="Major threats to amphibian orders") +
  scale_fill_manual(values=IUCNColors)+
  scale_y_continuous(name="Number of species", expand = c(0,0))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+ 
  theme_cowplot(font_size=24) + 
  coord_flip() +
  facet_wrap(~Order, scales='free_x') +
  theme(plot.title=element_blank(),
        text=element_text(size=9, family="Calibri", face="plain"),
        axis.title.y=element_blank(), 
        axis.text = element_text(size=9, family="Calibri", face="plain"),         
        plot.margin = unit(c(0.2, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.text=element_text(family="Calibri", face="plain"),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.3, 'cm'), #change legend key height
        legend.key.width = unit(0.3, 'cm'),
        legend.margin=margin(c(-1,0,-18,0))) #change legend key width)                   
dev.off()
                      
 
########################################################################################################################################

#Amphibiaweb disease portal data ...

disease <- read.csv("amphibian_disease_data_processed.csv")
      
#how has geographic representation of where we're sampling changed over time?
#Stacked histogram showing continental assignment by year

#Create data subset of just variables of interest
mycols <- c("continentOcean", "yearCollected", "individualCount", "diseaseTestedPositiveCount")
yrcon <- disease[mycols]
yrcon <- yrcon[yrcon$yearCollected != "Unknown", ]
yrcon$continentOcean <- as.factor(yrcon$continentOcean)
yrcon$individualCount <- as.numeric(yrcon$individualCount)
yrcon$yearCollected <- as.numeric(yrcon$yearCollected)
#remove incomplete rows from data frame
yrcon <- yrcon[complete.cases(yrcon), ]                                     
                      
range(yrcon$yearCollected)
yr_breaks <- seq(1700, 2020, by = 5)
#Create a new table, with columns that are year of declaration, avg size that year, then the interquartile range that year
yrcon$bins <- cut(yrcon$yearCollected, breaks=yr_breaks)

#Assign a single year to each of the samples based on bin location, for plotting along the x axis                      
my_year <- yrcon %>%
  # create numeric columns for the min and max of the age range for each age group
  separate(col = bins, into = c("yr_min", "yr_max"), sep = ",", remove = FALSE) %>%
  mutate(yr_min = as.numeric(gsub("\\(", "", yr_min))) %>%
  mutate(yr_max = as.numeric(gsub("]", "", yr_max))) %>%
  # calculate the mode
  mutate(year = (yr_max+ yr_min)/2)

yr_sum <- my_year %>%
  dplyr::group_by(year, continentOcean) %>%
  dplyr::summarise(totSamples = sum(individualCount))
yr_sum <- yr_sum[complete.cases(yr_sum), ]
yr_sum$prop <- NA
yr_sum$TotYr <- NA
for (row in 1:nrow(yr_sum)){
  yr <- yr_sum$year[row] #what's the year in question
  yr_sum$TotYr[row] <- sum(yr_sum$totSamples[yr_sum$year==yr]) #what's the total number of samples for that yr across continents
  yr_sum$prop[row] <-  yr_sum$totSamples[row]/yr_sum$TotYr[row]
}
                      
# function to increase vertical spacing between legend keys
# code snipped developed by @clauswilke
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

W <- ggplot(yr_sum, aes(x=year, y=log(1+ totSamples), width=5)) + 
  geom_bar(stat="identity", color="black", size=0.4, fill="black") +
  scale_y_continuous(name="Number of samples\n(log scale)", limits=c(log(1),log(121500)), expand = c(0,0), 
                     breaks=c(log(1), log(11), log(101), log(1001), log(10001), log(60001)), labels=c(0, 10, 100, 1000, 10000, 60000))+
  scale_x_continuous(name = "Year", breaks=seq(1830, 2020, by = 10), labels=seq(1830, 2020, by = 10), limits=c(1830,2025), 
                     expand = c(0,0)) +
  theme_cowplot() + 
  theme(text=element_text(size=12, family="Calibri", face="plain"), 
          axis.title=element_text(size=12, family="Calibri", face="plain"),
          legend.text=element_text(size=12, family="Calibri", face="plain"),
        axis.text.x = element_text(angle=35), 
        plot.margin = unit(c(0.1, .2, 0.2, 0.1), "cm"))
W1 <- ggplot(data=yr_sum, aes(x=year, y=prop, width=5, fill=continentOcean)) +
  geom_bar(stat="identity", color="black", size=0.4, key_glyph = "polygon3")+
  scale_fill_manual(values=c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#DDDDDD'))+
  scale_y_continuous(name="Proportion", limits=c(0,1.01), expand = c(0,0))+
  scale_x_continuous(name = "Year", breaks=seq(1830, 2020, by = 10), labels=seq(1830, 2020, by = 10), limits=c(1830,2025), 
                     expand = c(0,0)) +
  guides(fill=guide_legend(nrow=3, byrow=TRUE))+                    
  theme_cowplot() + 
  theme(text=element_text(size=12, family="Calibri", face="plain"), 
        axis.text.x = element_text(angle=35), 
        legend.text=element_text(size=12, family="Calibri", face="plain", margin = margin(r = 85, unit = "pt")),
        axis.title=element_text(size=12, family="Calibri", face="plain"),
        plot.margin = unit(c(0.1, 0.2, 0.1, 0.1), "cm"),       
        legend.position = "top", 
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.title = element_blank(),
        legend.margin=margin(c(0.2,0,0,0))) #change legend key width)                   
                


#Plot together, horizontally aligned    
tiff("Figure9.tiff", units="in", width=7.5, height=7, res=400)
plot_grid(W, W1, nrow=2, 
  align="hv", 
  labels = c("A", "B"),  
  label_size = 12,
  label_fontfamily ="Calibri",
  label_fontface = "plain", 
  rel_heights=c(0.3,0.5))                      
dev.off()       

############################################################################################################
                      
#Conservation data for phylogenetic heatmap   

#create new species column
disease$species <- paste(disease$genus, disease$specificEpithet)
#Create data subset of just variables of interest
mycols <- c("diseaseTested", "family", "species", "individualCount", "diseaseTestedPositiveCount")
fam <- disease[mycols]
#remove rows with only NA
fam <- fam[rowSums(is.na(fam)) != ncol(fam), ]
fam$individualCount <- as.numeric(fam$individualCount)
fam$diseaseTestedPositiveCount <- as.numeric(fam$diseaseTestedPositiveCount)
fam$diseaseTested <- as.factor(fam$diseaseTested)
fam$species <- as.factor(fam$species)                         

#Remove instances of "diseaseTestedPositiveCount" > "individualCount", because this should never be true
fam  <- fam[fam$diseaseTestedPositiveCount <= fam$individualCount,]                       
fam$family <- as.factor(fam$family) 
length(unique(fam$family))
#75     
                      
#Remove instances of diseaseTested = Bd+bsal, because what diseaseTestedPositiveCount means in this instance not clear
fam <- fam[fam$diseaseTested != "Bd+bsal", ]                        
fam_summary2 <- fam %>%
  dplyr::group_by(family, diseaseTested) %>%
  dplyr::summarise(BdTested=sum(individualCount, na.rm=T), BdPositive=sum(diseaseTestedPositiveCount, na.rm=T))
fam_summary2 <- fam_summary2[1:74,] #remove weird NA column that popped up                      
#fam_summary2Bd <- fam_summary2[fam_summary2$diseaseTested=="Bd",c(1,3,5)]
#names(fam_summary2Bd) <- c("family", "nBdTested", "nBdPositive")
#No Bsal data, so I cannot use this :/                      
#fam_summary2Bd$BdPrevalence <- fam_summary2Bd$nBdPositive/fam_summary2Bd$nBdTested                   
#fam_summary2Bsal <- fam_summary2[fam_summary2$diseaseTested=="Bsal",c(1,3)]
#names(fam_summary2Bsal) <- c("family", "nBsalTested")                      
##BRING THE BD AND BSAL INFO BACK TOGETHER WITH A JOIN FUNCTION
#fam_summary3 <- merge(fam_summary2Bd, fam_summary2Bsal, by=c("family"="family"))                                      
                    
#How many genera are represented in the database per family?          
fam_summary <- fam %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(CountSpeciesSampled= (length(unique(species))))   
#get rid of weird NA row that popped up
fam_summary <- fam_summary[1:74,]                      

final <- merge(fam_summary, fam_summary2, by=c("family"="family"))                    
write.csv(final,"TreeDiseaseData4Molly_species.csv", row.names = FALSE)                      
