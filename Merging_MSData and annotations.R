source("E:/proteomics/R-files/Proteomics_functions.R")

library(readxl)
library(dplyr)
library(tidyverse)
dir = ("E:/proteomics/combined_1/txt/proteinGroups.txt")
base_dir = 'E:/proteomics/From_colab/Tables/'

#Merging mass spectroscopy(MSData) and Clusters of orthologous groups(COGData) only.

#Mass spectroscopy data
MSData <- read.delim("E:/proteomics/combined_1/txt/proteinGroups.txt", header=TRUE)

#Clusters of orthologous groups data

#reading eggNOG annotation file
COGFile = "E:/proteomics/From_colab/EGGNoGMapperAnnotations.xlsx"
COGdata = read_excel("E:/proteomics/From_colab/EGGNoGMapperAnnotations.xlsx")

#preprocess variable names to match
COGdata2 = separate(data = COGdata, col = query, into = c("Tr", "Protein_IDs", "ID_9GAMM"), sep = "\\|")



#merge only with COG data ignoring location data 
names(MSData)[names(MSData) == "Protein.IDs"] <- "Protein_IDs"
COGMSData =  merge(COGdata2, MSData, by = "Protein_IDs")


View(COGMSData)

write_tsv(COGMSData,file.path(base_dir, "COGMSData.tsv"))










#reading subcellular location data
LocData = read.delim("E:/proteomics/Subcellular localization of proteins/H. caseinilytica_Subcellular localization of proteins")


#data2 = Fix_colnames(data)

#reading eggNOG annotation file
COGFile = "E:/proteomics/From_colab/EGGNoGMapperAnnotations.xlsx"
COGdata = read_excel("E:/proteomics/From_colab/EGGNoGMapperAnnotations.xlsx")

#mergedTable = Add_COG_data(COGdata, MSData)

View(COGdata)
View(LocData)
View(MSData)

#create common column names
names(MSData)[names(MSData) == "Fasta.headers"] <- "Fasta_headers"
names(LocData)[names(LocData) == "X.SeqName"] <- "Fasta_headers"
#names(COGData)[names(COGData) == "X.SeqName"] <- "Fasta_headers"

#Merge location and MS data
LocMSData = merge(LocData, MSData, by = "Fasta_headers")
merge.data.frame(LocData, MSData, by = intersect(names("Fasta_headers")))

#merge further with COGdata
COGdata2 = separate(data = COGdata, col = query, into = c("Tr", "Protein_IDs", "ID_9GAMM"), sep = "\\|")
View(COGdata2)
names(LocMSData)[names(LocMSData) == "Protein.IDs"] <- "Protein_IDs"
FinalData =  merge(COGdata2, LocMSData, by = "Protein_IDs")




write_tsv(FinalData,file.path(base_dir, "COGMSData.tsv"))


write_tsv(FinalData,file.path(base_dir, "Final.tsv"))
     