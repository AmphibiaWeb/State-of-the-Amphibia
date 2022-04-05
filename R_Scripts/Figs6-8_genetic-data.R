setwd("/Users/rdtarvin/Google_Drive/Documents/Service_Outreach/Amphibia_Web/SotA/Supplementary_Files/")

library(dplyr)
library(ggplot2)
library(cowplot)
library(readxl)
library(rentrez)
library(tidyverse)
library(lubridate)
library(rentrez)
library(XML)


#############################################################################################
######################## Fig 3A: AWEB NEW SPECIES ###########################################

## this section was used to convert raw data into cumulative counts by year, order, region
spp<-read.csv("AmphibiaWeb_newspecies_2007-2020.csv")
taxa<-read.csv("/Users/rdtarvin/Google_Drive_old/Documents/Service_Outreach/Amphibia_Web/SotA/Supplementary_Files-20210911/ncbi_taxonomy-database.csv")

orders<-subset(taxa, order %in% c("Anura","Gymnophiona","Caudata")) %>% select("family","order") %>% 
  data.frame() 

orders<-droplevels(orders)

data<-merge(spp,orders,by.x="family",all.x=T)
d2<-distinct(data) %>% na.omit()
# d2$year_described<-as.factor(d2$year_described)

d2_byyr<-d2 %>% group_by(order, Region_PUB, year_described) %>% summarize(count=n()) %>% 
  summarize(cumspp=cumsum(count),order=order,year_described=year_described) %>% data.frame()

write.csv(d2_byyr,file = "newspp_byyear.csv")
# In the published version, I added in data for years with no change

d2f<-read.csv("newspp_byyear.csv")


ggplot(d2f, aes(x=year_described, y=cumspp, color=order,shape=order,fill=order)) + theme_cowplot()+ 
  annotate("rect",xmin = 2016, xmax = 2020, ymin = 0, ymax = Inf,alpha = .1,fill = "blue")+
  labs(x="Year",y="Cumulative Number of New Species") +scale_shape_manual(values=c(21,22,24))+
  geom_line()+  geom_point()+ theme(legend.position="top",legend.title = element_blank()) + 
    facet_grid(~Region_PUB)  +scale_color_manual(values = c("#02A144","grey","#E0A800")) +
  scale_fill_manual(values = c("#02A144","grey","#E0A800")) + scale_x_continuous(breaks=seq(2008, 2020, by = 4))


#############################################################################################
#############################################################################################


#############################################################################################
######################## Fig 3BCD: GENBANK TAXA #############################################


# Function  ########################
# Read data from AmphibiaWeb taxonomy archive files
# Bind all the rows of the taxonomy files into a tibble
Get_Taxonomy_Data <- function (){
Taxonomy <- tibble()
file_list <- list.files("AWeb-taxonomy-archive-master", full.names = TRUE)
  for (i in 1:length(file_list)) {
    file_date <- str_extract(file_list[i], "20\\d\\d\\d\\d\\d\\d")
    df <-  read_tsv(file_list[i], show_col_types = FALSE) %>% 
      select(order, genus, species, aweb_uid) %>% 
      rename(Taxon = order) %>%
      bind_cols(file_date = ymd(file_date))
    Taxonomy <- bind_rows(Taxonomy, df)
  }
return(Taxonomy)
}
###################################

# Get the names from AWeb taxonomy files
# These will be used for plots 1 and 2
AWeb_Taxonomy <- Get_Taxonomy_Data()
# use only the counts for December, the end of the year
AWeb_Taxonomy_by_year <- AWeb_Taxonomy %>% filter(month(file_date) == 12)

# Set size for geom_text for cumulative counts for all three plots
font_size_n <- 3.2

############## Plot 1: Cumulative numbers of species names in AmphibiaWeb by year

# Do the counts before graphing
my_Species_Counts <- count(AWeb_Taxonomy_by_year, Taxon, file_date) 
Total_Species <- my_Species_Counts %>% group_by(file_date) %>% summarize (n = sum(n)) %>% mutate(Taxon = "Total")
my_Species_Counts <- bind_rows(my_Species_Counts, Total_Species) %>% mutate(year = year(file_date)) %>% select(-file_date)

# Plot species numbers by year
Species_Count_Plot <- ggplot(data = my_Species_Counts, 
            aes(x = year, y = n, group = Taxon, 
                color = Taxon, 
                fill = Taxon, 
                shape = Taxon)) +
    geom_line() +
    geom_point(size = 3.0, color = "white") +
    scale_shape_manual(values=c(21, 22, 24, 25)) +
    scale_color_manual(values = c("#02A144", "gray", "#E0A800", "black")) +
    scale_fill_manual(values = c("#02A144", "grey", "#E0A800", "black")) +
    geom_text(data = my_Species_Counts, 
              aes(label = n, y = n), nudge_y = 380, color = "black", size = font_size_n) +
    theme_cowplot() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title = element_text(size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
          panel.grid.minor.x = element_blank(), 
          legend.position = c(0.07, 0.50), 
          legend.title = element_blank(),
          legend.text = element_text(size = 9),
          legend.key = element_rect(fill = 'white'),
          legend.key.size = unit(0.8, "lines"),
          plot.margin = margin(20, 0, 20, 20, "pt")) +
    ylab("Number of species in AmphibiaWeb")+ 
    xlab("Year") +
    scale_x_continuous(breaks=seq(2012, 2021, 1)) +
    scale_y_continuous(breaks = seq(0, 8000, 1000))
# Save to file
ggsave("AWeb-Total-Species-by-Year.pdf", Species_Count_Plot)

############## Plot 2: Cumulative numbers of genus names in AmphibiaWeb by year

# Get unique genus names for each year
genera_and_year <- distinct(AWeb_Taxonomy_by_year, genus, year = file_date, .keep_all = TRUE)
# Do the counts before graphing
my_Genus_Counts <- count(genera_and_year, Taxon, file_date) 
Total_Genera <- my_Genus_Counts %>% group_by(file_date) %>% summarize (n = sum(n)) %>% mutate(Taxon = "Total")
my_Genus_Counts <- bind_rows(my_Genus_Counts, Total_Genera) %>% mutate(year = year(file_date)) %>% select(-file_date)

# Plot genus numbers by year
Genus_Count_Plot <- ggplot(data = my_Genus_Counts, 
           aes(x = year, y = n, group = Taxon, color = Taxon, fill = Taxon, shape = Taxon)) +
    geom_line() +
    geom_point(size = 3.0, color = "white") +
    scale_shape_manual(values=c(21, 22, 24, 25)) +
    scale_color_manual(values = c("#02A144", "gray", "#E0A800", "black")) +
    scale_fill_manual(values = c("#02A144", "grey", "#E0A800", "black")) +
    coord_cartesian(ylim = c(0, 600)) +
    geom_text(aes(label = n, y = n), nudge_y = 25, color = "black", size = font_size_n) +
    ylab("Number of genera in AmphibiaWeb") +
    xlab("Year") +
    theme_cowplot() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title = element_text(size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
          panel.grid.minor.x = element_blank(), 
          legend.position = c(0.07, 0.50), 
          legend.title = element_blank(),
          legend.text = element_text(size = 9),
          legend.key = element_rect(fill = 'white'),
          legend.key.size = unit(0.8, "lines"),
          plot.margin = margin(20, 0, 20, 20, "pt")) +
    scale_x_continuous(breaks=seq(2012, 2021, 1)) +
    scale_y_continuous(breaks = seq(0, 600, 100))
# Save to file
ggsave("AWeb-Total-Genera-by-year.pdf", Genus_Count_Plot)


############## Plot 3: Cumulative numbers of species names in GenBank by year

## Search Genbank for cumulative numbers of species

# This function gets the COUNT only of GenBank species names 
# for formal taxa for a given year. 
search_year <- function(year, term){
    query <- paste(term, "[SubTree] AND ", year, "[edat] AND species[Rank] NOT 
    uncultured[prop] 
    NOT unspecified[prop]")
    entrez_search(db="taxonomy", term=query, retmax=2000)$count   # Get only the count!
}

# This function gets the COUNT only GenBank species names for 
# unspecified species taxa for a given year. 
search_unspecified_year <- function(year, term){
    query <- paste(term, "[SubTree] AND ", year, "[edat] AND species[Rank] 
    AND unspecified[prop]")
    entrez_search(db="taxonomy", term=query, retmax=2000)$count   # Get only the count!
}

# First GenBank records are in 1992
begin_Year <- 1992
end_Year <- 2020
year <- begin_Year:end_Year

# If no search results already exist (they should exist), then do the search
if (!file.exists("GenBank-Amphib.tsv"))    {
  # Return a vector of counts per year- 
  Anura_name_count                   <- sapply(year, search_year, term="Anura")
  Caudata_name_count                 <- sapply(year, search_year, term="Caudata")
  Gymnophiona_name_count             <- sapply(year, search_year, term="Gymnophiona")
  Anura_unspecified_name_count       <- sapply(year, search_unspecified_year,
                                               term="Anura")
  Caudata_unspecified_name_count     <- sapply(year, search_unspecified_year,
                                               term="Caudata")
  Gymnophiona_unspecified_name_count <- sapply(year, search_unspecified_year,
                                               term="Gymnophiona")
  
  # Assemble the six counts into a tibble
  my_Frogs                   <- data.frame(year, Anura_name_count, 
                                           cumsum(Anura_name_count),
                                           "Anura")
  my_Salamanders             <- data.frame(year, Caudata_name_count, 
                                           cumsum(Caudata_name_count),
                                           "Caudata")
  my_Caecilians              <- data.frame(year, Gymnophiona_name_count,
                                           cumsum(Gymnophiona_name_count), 
                                           "Gymnophiona")
  my_unspecified_Frogs       <- data.frame(year, Anura_unspecified_name_count,
                                           cumsum(Anura_unspecified_name_count),  
                                           "unspecified Anura")
  my_unspecified_Salamanders <- data.frame(year, Caudata_unspecified_name_count,
                                           cumsum(Caudata_unspecified_name_count),  
                                           "unspecified Caudata")
  my_unspecified_Caecilians  <- data.frame(year, Gymnophiona_unspecified_name_count,
                                           cumsum(Gymnophiona_unspecified_name_count), 
                                           "unspecified Gymnophiona")
  
  my_Col_Names <- c("Year", "RawCount", "CumulCount",  "Taxon")
  colnames(my_Frogs)                   <- my_Col_Names
  colnames(my_Salamanders)             <- my_Col_Names
  colnames(my_Caecilians)              <- my_Col_Names
  colnames(my_unspecified_Frogs)       <- my_Col_Names
  colnames(my_unspecified_Salamanders) <- my_Col_Names
  colnames(my_unspecified_Caecilians)  <- my_Col_Names
  
  # Bind the six dataframes into one
  Amphib_tibble <- bind_rows(my_Frogs, my_Salamanders, my_Caecilians,
                             my_unspecified_Frogs, my_unspecified_Salamanders,
                             my_unspecified_Caecilians) %>% 
    gather(RawCount, CumulCount, key = "CountType", value = "myCount")
  
  # Write tibble to a file to be used in future analyses
  write_tsv(Amphib_tibble, "GenBank-Amphib.tsv")
}

# Set first year to plot for x-axis, and omit earlier dates
First_year_to_Plot <- 2011
Amphib_tibble <- read_tsv("GenBank-Amphib.tsv", col_names = TRUE) %>% 
  filter(CountType == "CumulCount", Year > First_year_to_Plot) %>% 
  select(-CountType) %>% rename(n = myCount)

Totals_GenBank <- Amphib_tibble %>% group_by(Year) %>% 
  summarize (n = sum(n)) %>% mutate(Taxon = "Total")
Amphib_Cumul_tibble <- bind_rows(Amphib_tibble, Totals_GenBank) %>% 
  mutate(Taxon = fct_relevel(Taxon, 
            "Anura", "Caudata", "Gymnophiona", 
            "unspecified Anura", "unspecified Caudata", "unspecified Gymnophiona", 
            "Total"))

# Get a subset of point text (cumulative n) for plotting
Total_n_to_Plot <- c("Total", "Anura", "unspecified Anura", "Caudata", "Gymnophiona")
Amphib_Cumul_tibble_filtered <- Amphib_Cumul_tibble %>%
  mutate(plot_N = ifelse(Taxon %in% Total_n_to_Plot, n, ""))

# Do the plot!
Genbank_Plot <- ggplot(data = Amphib_Cumul_tibble_filtered, aes(Year, n, 
                                          color = Taxon, 
                                          fill = Taxon,
                                          shape = Taxon,
                                          group = Taxon)) +
    geom_line() +
    # Plot white point to from the "shadow margin of dots"
    geom_point(data = Amphib_Cumul_tibble_filtered, size = 3.5, 
               color = "white", fill = "white") +
    geom_point(data = Amphib_Cumul_tibble_filtered, size = 2.5) +
    geom_text(aes(label = plot_N, y = n), 
              nudge_y = 470, color = "black", size = font_size_n) +
    scale_shape_manual(values=c(21, 22, 24, 21, 22 ,24, 25)) +
    scale_color_manual(values = c("#02A144", "gray", "#E0A800", 
                                  "#02A144", "gray", "#E0A800", "black")) +
    scale_fill_manual(values = c("#02A144", "grey", "#E0A800",
                                 "white", "white", "white", "black")) +
    theme_cowplot() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title = element_text(size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
          panel.grid.minor.x = element_blank(), 
          legend.position=c(0.06, 0.86), 
          legend.title = element_blank(),
          legend.text = element_text(size = 9),
          legend.key = element_rect(fill = 'white'),
          legend.key.size = unit(0.8, "lines"),
          plot.margin = margin(20, 20, 20, 20, "pt")) +
    scale_y_continuous(breaks = seq(0, 12000, 2000), 
                       limits = c(NA, 12000)) +
    scale_x_continuous(breaks = seq(begin_Year, end_Year, 1)) +
    xlab("Year") +
    ylab("Number of species in GenBank") 

## Save to a file
#ggsave("Amphibia-GenBank-Recs.pdf", Genbank_Plot, width = 4, height = 3)

# Combine the Species, Genus, and GenBank plots
Combined_Plots <- plot_grid(Species_Count_Plot, Genus_Count_Plot, Genbank_Plot, ncol = 3,
                            labels = "AUTO", label_x = c(0.05, 0.05, 0.05), rel_widths = c(1, 1, 1.1))
## Save to a file
#ggsave("Combined-Species-Genus-Genbank-Plots-Cannatella.pdf", Combined_Plots, width = 13, height = 4)
#ggsave("Combined-Species-Genus-Genbank-Plots-Cannatella.jpg", Combined_Plots, width = 13, height = 4)

#############################################################################################
#############################################################################################


#############################################################################################
######################## Fig 4: GENOMES AVAILABLE ###########################################

# read in table of genome data, merge with taxa database
genome <- read_excel("TableS3_Amphibian-Genomes.xlsx") %>% as.data.frame()

## rename columns
colnames(genome)[colnames(genome)=="Species"]<-"species"
colnames(genome)[colnames(genome)=="Average Genome Size (Gb)"]<-"genome.size"
colnames(genome)[colnames(genome)=="Year of first version"]<-'year'

## summarize
genome %>% group_by(Order,year) %>% summarize(count=n()) %>% data.frame()-> gensum

## add 0s for years without data by creating a list of all expected data points
years<-as.data.frame(rep(c(2010:2021),3))
colnames(years)<-'year'
years$Order<-c(rep("Anura",12),rep("Caudata",12),rep("Gymnophiona",12))

## then merge with genome dataframe
gensum2<-left_join(years,gensum)
gensum2[is.na(gensum2)]<-0

## summarize and calculate cumulative numbers
gensum2 %>% group_by(Order,year) %>% summarize(gen_yr=sum(count)) %>% 
  summarize(cumgen=cumsum(gen_yr),year=year) %>% data.frame() -> gencum

## plot cumulative number of genomes
Fig4A<-ggplot(gencum, aes(year,cumgen,group=Order,fill=Order,shape=Order)) + 
  annotate("rect",xmin = 2016, xmax = 2020, ymin = 0, ymax = Inf,alpha = .1,fill = "blue")+
  geom_path(aes(color=Order),show.legend = FALSE) +
  geom_point(size=2,show.legend = FALSE) +scale_x_continuous(breaks=seq(2010, 2021, by = 2))+
  theme_cowplot(font_size=12) + scale_color_manual(values = c("#02A144","grey","#E0A800")) +
  scale_fill_manual(values = c("#02A144","grey","#E0A800")) +
  xlab(NULL) + ylab("Cumulative number") + labs(tag="A") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_blank(),
        plot.tag = element_text(vjust = 1,hjust=-4))+
  scale_shape_manual(values=c(21,22,24))

## plot size of genomes sequenced each year
Fig4B<-ggplot(genome, aes(year,genome.size,fill=Order,shape=Order)) + 
  annotate("rect",xmin = 2016, xmax = 2020, ymin = 0, ymax = Inf,alpha = .1,fill = "blue")+
  geom_point(size=2,alpha=0.7)+
  theme_cowplot(font_size=12) + scale_color_manual(values = c("#02A144","grey","#E0A800")) +
  scale_fill_manual(values = c("#02A144","grey","#E0A800")) +
  xlab(NULL) + ylab("Size (Gb)") +scale_x_continuous(breaks=seq(2010, 2021, by = 2))+
  labs(tag="B") + theme(plot.title = element_text(hjust = 0.5))+
  scale_shape_manual(values=c(21,22,24))+ theme(legend.position="bottom",legend.title=element_blank(),
                                                plot.tag = element_text(vjust = 1,hjust=-4))

plot_grid(Fig4A,Fig4B,nrow=2,align="hv")

## export as pdf, size 3.4in x 5in
##########################

## calculate average size by genome status
genome2 %>% group_by(Genome) %>% summarize(mean=mean(Scaffold.N50),sd=sd(Scaffold.N50),unit=unique(Scaffold.N50.unit)) %>% data.frame()
genome2 %>% subset(Genome!="Contig") %>% summarize(mean=mean(genome.size),sd=sd(genome.size)) %>% data.frame()



#############################################################################################
#############################################################################################



#############################################################################################
######################## Fig 5: SRA DATA AVAILABLE ##########################################

## read in data
data<-read.csv("SraRunTable.txt") # downloaded on June 21 2021
tax<-read.csv("sra_amphibia.csv") # downloaded on June 21 2021

## subset data and rename columns
subdata<-data %>% select(Run,Assay.Type,Instrument,Organism,Platform,ReleaseDate,Bases,Bytes,SRA.Study)
colnames(subdata)[colnames(subdata)=="Organism"]<-"species"

# merge datasets, remove data where the order is missing, and calculate Mb
subdata<-subset(left_join(subdata,tax),order!="")
subdata$Mb<-subdata$Bases/1000000

# reformat time
subdata$ReleaseDate<-substr(subdata$ReleaseDate,1,10)
subdata$year<-substr(subdata$ReleaseDate,1,4)
subdata$month<-substr(subdata$ReleaseDate,6,7)
subdata$day<-substr(subdata$ReleaseDate,9,10)

# add column with info about whether the species is a model
subdata$system <- ifelse(subdata$species %in% c("Xenopus tropicalis","Xenopus laevis","Ambystoma mexicanum"), "model", "non-model")


############## Fig 5A
# calculate cumulative amount of data by order and whether or not it is from a model system
bymodel<-subdata %>% na.omit()  %>% filter(year<2021) %>%group_by(order,system,year) %>% 
  summarize(Mb_year=sum(Mb)) %>% summarize(cumMb=cumsum(Mb_year),year=year,system=system) %>% data.frame()
bymodel$year<-as.numeric(bymodel$year)

Fig5A<-ggplot(na.omit(bymodel),aes(year,cumMb/1000,fill=order,shape=order)) + 
  annotate("rect",xmin = 2016, xmax = 2020, ymin = 0, ymax = Inf,alpha = .1,fill = "blue")+
  geom_line(aes(color=order,linetype=system)) +guides(color=FALSE,shape=FALSE)+
  geom_point(size=2,show.legend = F) +theme_cowplot(font_size = 12) +ylab("Cumulative SRA\ndata (Gb)") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position=c(0.05,0.8),
        legend.title=element_blank(), plot.tag = element_text(vjust = 1,hjust=-8))+ 
  scale_x_continuous(breaks=seq(2008, 2020, by = 4))+
  scale_shape_manual(values=c(21,22,24))+scale_color_manual(values = c("#02A144","grey","#E0A800"))+ 
  scale_fill_manual(values = c("#02A144","grey","#E0A800")) + labs(tag="A") 

############## Fig 5B
### count the cumulative number of studies
numstudies<-subdata %>% na.omit()  %>% filter(year<2021) %>% group_by(order,system,year) %>% 
  summarize(numspecies=length(unique(species)),numstudies=length(unique(SRA.Study))) %>% 
  summarize(cumstu=cumsum(numspecies),year=year,system=system) %>% data.frame()
numstudies$year<-as.numeric(numstudies$year)

Fig5B<-ggplot(numstudies,aes(year,cumstu,fill=order,shape=order)) + 
  annotate("rect",xmin = 2016, xmax = 2020, ymin = 0, ymax = Inf,alpha = .1,fill = "blue")+
  geom_line(aes(color=order,linetype=system),show.legend = F) +
  geom_point(size=2,show.legend = F) + theme_cowplot(font_size = 12) + ylab("Cumulative number\nof studies") + labs(tag="B")+ 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),plot.tag = element_text(vjust = 1,hjust=-8))+
  scale_x_continuous(breaks=seq(2008, 2020, by = 4))+scale_y_continuous(breaks=seq(0, 800, by = 200))+
  scale_shape_manual(values=c(21,22,24))+scale_color_manual(values = c("#02A144","grey","#E0A800"))+ 
  scale_fill_manual(values = c("#02A144","grey","#E0A800")) 


############## Fig 5C
### calculate the cumulative number of species in SRA
sppbyyr<-subdata %>% na.omit()  %>% group_by(year,order) %>%
  summarize(species=unique(species),
            year=unique(year),order=unique(order)) %>% data.frame()

## compare by year to get the number of new species per year
s2008<-droplevels(subset(sppbyyr,year==2008))
s2009<-droplevels(subset(sppbyyr,year==2009))
s2010<-droplevels(subset(sppbyyr,year==2010))
s2011<-droplevels(subset(sppbyyr,year==2011))
s2012<-droplevels(subset(sppbyyr,year==2012))
s2013<-droplevels(subset(sppbyyr,year==2013))
s2014<-droplevels(subset(sppbyyr,year==2014))
s2015<-droplevels(subset(sppbyyr,year==2015))
s2016<-droplevels(subset(sppbyyr,year==2016))
s2017<-droplevels(subset(sppbyyr,year==2017))
s2018<-droplevels(subset(sppbyyr,year==2018))
s2019<-droplevels(subset(sppbyyr,year==2019))
s2020<-droplevels(subset(sppbyyr,year==2020))

s2009n<-s2009[!s2009$species %in% s2008$species,]
s2010n<-s2010[!s2010$species %in% s2008$species,]
s2011n<-s2011[!s2011$species %in% s2008$species,]
s2012n<-s2012[!s2012$species %in% c(as.character(s2011n$species),as.character(s2008$species)),]
s2013n<-s2013[!s2013$species %in% 
                c(as.character(s2012n$species),as.character(s2011n$species),
                  as.character(s2008$species)),]
t2013<-c(as.character(s2012n$species),as.character(s2011n$species),
         as.character(s2008$species),as.character(s2013n$species))
s2014n<-s2014[!s2014$species %in% t2013,]
s2015n<-s2015[!s2015$species %in% c(t2013,as.character(s2014n$species)),]
s2016n<-s2016[!s2016$species %in% c(t2013,as.character(s2014n$species),as.character(s2015n$species)),]
s2017n<-s2017[!s2017$species %in% c(t2013,as.character(s2014n$species),as.character(s2015n$species),
                                    as.character(s2016n$species)),]
t2017<-c(t2013,as.character(s2014n$species),as.character(s2015n$species),
         as.character(s2016n$species),as.character(s2017n$species))
s2018n<-s2018[!s2018$species %in% t2017,]
s2019n<-s2019[!s2019$species %in% c(t2017,as.character(s2018n$species)),]
s2020n<-s2020[!s2020$species %in% c(t2017,as.character(s2018n$species),as.character(s2019n$species)),]

## bind numbers of unique species per year together
numspp<-rbind(s2008,s2009n,s2010n,s2011n,s2012n,s2013n,s2014n,s2015n,s2016n,s2017n,s2018n,s2019n,s2020n) %>%
  group_by(order,year) %>%summarize(numspecies=n()) %>% data.frame()

## add 0s for years with no new species added
# years<-as.data.frame(rep(c(2008:2021),3))
# colnames(years)<-'year'
# years$order<-c(rep("Anura",14),rep("Caudata",14),rep("Gymnophiona",14))
# 
# numspp2<-left_join(years,numspp)
# numspp2[is.na(numspp2)]<-0

## calculate cumulative number of species
cumspp<-numspp %>% group_by(order) %>% mutate(cumspp=cumsum(numspecies)) %>% data.frame()

# add datapoints for caecilians in 2019 and 2020 (no new data, but want to represent on graph)
cae19<-c("Gymnophiona",2019,0,10)
cae20<-c("Gymnophiona",2020,0,10)
cumspp<-rbind(cumspp,cae19,cae20)
cumspp$cumspp<-as.numeric(cumspp$cumspp)
cumspp$year<-as.numeric(cumspp$year)

## plot
Fig5C<-ggplot(subset(cumspp,year<2021),aes(year,cumspp,fill=order,shape=order,group=order)) + geom_path(aes(color=order),show.legend = F) +
  annotate("rect",xmin = 2016, xmax = 2020, ymin = 0, ymax = Inf,alpha = .1,fill = "blue")+
  geom_point(size=2) + theme_cowplot(font_size = 12) + ylab("Cumulative number of\nspecies represented") + 
  theme(legend.position="bottom",axis.title.x = element_blank(),legend.title = element_blank(),
        plot.tag = element_text(vjust = 1,hjust=-8),legend.text=element_text(hjust=3))+
  scale_x_continuous(breaks=seq(2008, 2020, by = 4))+
  scale_shape_manual(values=c(21,22,24))+scale_color_manual(values = c("#02A144","grey","#E0A800"))+ 
  scale_fill_manual(values = c("#02A144","grey","#E0A800")) + labs(tag="C")


plot_grid(Fig5A,Fig5B,Fig5C,ncol=1, align="hv")


## export as pdf, size 3.4in x 6in
##########################

#############################################################################################
#############################################################################################



#############################################################################################
######################## Fig 6: NCBI DATA AVAILABLE #########################################


############## Fig 6ABC
# search using entrez tools
# for yr in {1982..2020}; do for i in {01..12}; do esearch -db nuccore -query "amphibia [ORGN]" -mindate "$yr/$i" -maxdate "$yr/$i"  | epost -db nuccore -format uid| esummary | xtract -pattern DocumentSummary -element Caption,Title,CreateDate Taxon -element TaxId ScientificName  > $yr.$i.txt; done; done
# data were combined across years

## type was assigned with this code, where ncbit_all was a cleaned concatenated file from entrez pull
# ncbit_all$type<-"NA"
# type<-c()
# match<-c()
# # first determine if its mtDNA or nuDNA
# for(i in 1:nrow(ncbit_all)){
#   match<-grepl("cytochrome oxidase|COX|tRNA|ribosomal RNA|rRNA|NADH dehydro|mitochondri|cyt[. ]b|cytochrome b|ATP8|ATP6|control region|d-loop|cytb",ncbit_all$sequence[i])
#   ifelse(match,ncbit_all$type[i]<-"mtDNA",ncbit_all$type[i]<-"nDNA")
#   match<-c()
# }
# # then code as mRNA
# ncbit_all$type[grepl("mRNA|transcript|TSA:",ncbit_all$sequence)]<-"mRNA"

## added system with this code
# ncbit$system <- ifelse(ncbit$species %in% c("Xenopus tropicalis","Xenopus laevis","Ambystoma mexicanum"), "model", "non-model")

# load in labeled data
ncbit<-read.csv("ncbi_all_nuccore_type.csv")

# calculate cumulative number of sequences by year, system, and order
cumtype<-ncbit %>% na.omit() %>% subset(order %in% c("Anura","Caudata","Gymnophiona")) %>% 
  group_by(type,system,order,year) %>% summarize(n=n()) %>% 
  summarize(seq=cumsum(n),year=year,type=type,order=order) %>% data.frame()
cumtype$order<-droplevels(cumtype$order)
str(cumtype)

cumtype$year<-as.numeric(cumtype$year)
cumtype$type<-factor(cumtype$type,levels=c("mtDNA","nDNA","mRNA"))

Fig6ABC<-ggplot(cumtype, aes(year,seq/1000,fill=order,shape=order)) +
  annotate("rect",xmin = 2016, xmax = 2020, ymin = 0, ymax = Inf,alpha = .1,fill = "blue")+
  geom_line(aes(color=order,linetype=system))+ theme_cowplot() + 
  geom_point(size=2,alpha=0.8,show.legend = F)+ guides(color=F,shape=F)+
  scale_color_manual(values = c("#02A144","grey","#E0A800")) +
  scale_fill_manual(values = c("#02A144","grey","#E0A800"))  + 
  ylab("Cumulative number of GenBank sequences (in thousands)")  + xlab(NULL)+
  scale_shape_manual(values=c(21,22,24))+ scale_x_continuous(breaks=seq(1986, 2020, by = 5))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),legend.position=c(0.05,0.9),
        legend.title=element_blank())+ 
  facet_grid(type~.,scales = "free_y") 


############## Fig 6D
## download taxa information
search_year <- function(year, term){
  query <- paste(term, "[SubTree] AND ", year, "[edat] AND species[Rank] NOT 
                 uncultured[prop] 
                 NOT unspecified[prop]")
  entrez_search(db="taxonomy", term=query, retmax=2000)$count   # Get only the count!
}

# First GenBank records are from 1982
begin_Year <- 1982
end_Year <- 2020
year <- begin_Year:end_Year
# Return a vector of counts per year- 
Anura_name_count <- sapply(year, search_year, term="Anura")
Caudata_name_count <- sapply(year, search_year, term="Caudata")
Gymnophiona_name_count <- sapply(year, search_year, term="Gymnophiona")

my_Col_Names <- c("Year", "RawCount", "CumulCount",  "Taxon")
# Make dataframes for Frogs and Salamanders; re-name columns with same titles
myFrogs <- data.frame(year, Anura_name_count, cumsum(Anura_name_count), "Anura")
colnames(myFrogs) <- my_Col_Names
mySalamanders <- data.frame(year, Caudata_name_count, cumsum(Caudata_name_count), "Caudata")
colnames(mySalamanders) <- my_Col_Names
myCaecilians <- data.frame(year, Gymnophiona_name_count, cumsum(Gymnophiona_name_count), "Gymnophiona")
colnames(myCaecilians) <- my_Col_Names

# Bind the three dataframes
Amphib_tibble <- bind_rows(myFrogs, mySalamanders, myCaecilians) %>% gather(RawCount, CumulCount, key = "CountType", value = "myCount")

Fig6D<-ggplot(subset(Amphib_tibble,CountType=="CumulCount"),aes(Year,myCount,fill=Taxon,shape=Taxon,group=Taxon)) + 
  annotate("rect",xmin = 2016, xmax = 2020, ymin = 0, ymax = Inf,alpha = .1,fill = "blue")+
  geom_path(aes(color=Taxon,alpha=0.8),show.legend = F) +
  geom_point(size=2) + theme_cowplot() + ylab("Cumulative number\nof species represented") + 
  scale_shape_manual(values=c(21,22,24))+scale_color_manual(values = c("#02A144","grey","#E0A800"))+ 
  scale_fill_manual(values = c("#02A144","grey","#E0A800")) + 
  scale_y_continuous(breaks=seq(0, 6000, by = 1000))+
  scale_x_continuous(breaks=seq(1986, 2020, by = 5))+ 
  theme(legend.position = "bottom",legend.title=element_blank(),
        legend.justification = "center",axis.title.x=element_blank())


plot_grid(Fig6ABC,Fig6D,nrow=2,labels = NULL,align="hv",axis="blr",rel_heights = c(2,.8))
## this figure was labeled manually after saving




