#load packages
library(phytools)
library(geiger)
library(tcltk)
library(tidyverse)
library(phangorn)
library(ggtree)
library(ggnewscale)
library(viridis)

#set working directory
setwd("/Users/mollywomack/Documents/Documents\ -\ Molly’s\ MacBook\ Pro/Pubs/InPrep/AWeb_Report2020")

#load data
HeatData<-read.csv("HeatData_2021127.csv", header=T)
totedata<-read.csv("Datasets/Family_TotalSpecies.csv", header=T)


#load family tree
FamilyTree<-read.tree("HimeFamilyTree2020_4FamiliesAdded.tre")
FamilyTree<-ladderize(FamilyTree, right = FALSE)
#FamilyTree <- chronopl(FamilyTree, lambda = 1)
#FamilyTree<-force.ultrametric(FamilyTree)

setdiff(totedata$Family,FamilyTree$tip.label)
setdiff(FamilyTree$tip.label,totedata$Family)

#trim frog tree of life to species in your dataset
rownames(HeatData)<-HeatData$Family
name.check(FamilyTree,HeatData)->overlap
overlap$data_not_tree
drop.tip(FamilyTree,overlap$tree_not_data)->FamilyTree;
#plot(ecotree)
#Remove excess data from species not found in tree
HeatData<-HeatData[!rownames(HeatData) %in% overlap$data_not_tree,]
HeatData[] <- lapply(HeatData, function(x) if(is.factor(x)) factor(x) else x)
HeatData<-HeatData[FamilyTree$tip.label, ]

setdiff(totedata$Family,FamilyTree$tip.label)

HeatData$distinct_species[is.na(HeatData$distinct_species)] = 0
HeatData$tip<-paste(HeatData$Family," (", HeatData$distinct_species, "/", HeatData$Species, ")", sep = "")
FamilyTree$tip.label <- HeatData$tip
rownames(HeatData)<-HeatData$tip

colnames(HeatData)
drops <- c("X", "Family", "Order", "Subfamilies","Genera", "Species", "distinct_species","tip")
PropHeatData<-HeatData[ , !(names(HeatData) %in% drops)]
PropHeatData[is.na(PropHeatData)] = 0


colnames(PropHeatData)
colnames(PropHeatData) <- c("new species","call recorded","CT scanned","genome sequenced","ncbi sequence","sra sequence","tested for Bd","Bd positive")


#PropHeatData <- PropHeatData[,c("PropSp_New","PropSp_Calls","PropSp_CT","genomes","PropSp_sra","PropSp_ncbi","PropSp_DiseaseTested", "PropSp_DiseasePositive" )]

na.PropHeatData<-na_if(PropHeatData, 0)

breakpoints <- c(-0.64, -0.62, 0.239, 1.118, 1.997, 2.876, 3.755, 4.634, 5.513, 6.392, 7.271, 8.15)
#colors <- c("white","grey90","grey80","grey70","grey60","grey50","grey40","grey30","grey20","grey10","black")
#phylo.heatmap(FamilyTree, PropHeatData, fsize=1, breaks=breakpoints, colors=colors, standardize=T)

phylo.heatmap(FamilyTree, na.PropHeatData, fsize=1, breaks=NULL, colors=rev(magma(c(0,1))-1), standardize=F, split=c(0.7,0.3))
    
pdf(file = paste("FamilyTree_HeatMap",".pdf",sep="_"), width =10,  height = 15)
phylo.heatmap(FamilyTree, na.PropHeatData, fsize=1, colors=rev(magma(length(1)-1)), standardize=F,split=c(0.7,0.3)) #breaks=breakpoints 
dev.off()


