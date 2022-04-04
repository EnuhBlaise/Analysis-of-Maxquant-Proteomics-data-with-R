
load_libs <- function() {
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(mice)
  library(tidyverse)
  library(janitor)
  library(kableExtra)
  library(AER)
  library(tinytex)
  library(stringr)
  library(ggplot2)
  
}

Fix_colnames <- function (a) {
  ##arrange the column names in the dataset to not have spaces inbetween.
  data_new1 <- a                  
  colnames(data_new1) <- gsub(" ", "_", colnames(data_new1))
  head(data_new1)                       
  data2 = data_new1
  names(data2)[names(data2) == "#Most-likely-Location"] <- "Most_likely_Location"
  
  return(data2)
  
}

Add_COG_data <- function(a) {
  ###Takes the COG data and merges it with the data from MAss spectrometry to prduce a merged table###
  COGdata = a
  #(COGdata)
  COG = COGdata %>% select(query, COG_category, Description)
  COG2 = separate(data = COG, col = query, into = c("Tr", "Protein_IDs", "ID_9GAMM"), sep = "\\|")
  #View(COG2)
  mergedTable <- merge(data2, COG2, by = "Protein_IDs")
  mergedTable
  
}


Prot_subcellular_locations <- function (a) {
  ##Produces a frequency table of the subcellular location of proteins 
  #Table summaries
  data2 %>% tabyl(Most_likely_Location) %>% {. ->>loc_table } %>% 
    adorn_totals( where = 'row') %>%
    adorn_totals( where = 'col') %>%
    kbl(caption = "Subcellular location of proteins") %>%
    kable_classic(full_width = F, html_font = "Times new roman")
  return(loc_table)
  
  
}

Plot_Sub_location <- function(a) {
  ##Produces a barplot of the subcellular location of proteins
  loc_table[sapply(loc_table, is.character)] <- lapply(loc_table[sapply(loc_table, is.character)], as.factor)
  ggplot(loc_table, aes(x=Most_likely_Location, y=n, fill = Most_likely_Location)) + 
    geom_bar(stat = "identity") + 
    theme_gray()+ 
    scale_x_discrete(guide = guide_axis(angle = 90))+
    xlab("Subcellular location")+
    ylab("count")+
    theme(legend.position = "none")
  
  
}

OuterMembrane_Data <-function(a) {
  #produces 1 table of the data subsetted to outermembrane proteins data only
  outerMembrane_data = subset(a, Most_likely_Location == "OuterMembrane")
  return(outerMembrane_data)
}

InnerMembrane_Data <-function(a) {
  #produces 1 table of the data subsetted to innermembrane proteins data only
  InnerMembrane_data = subset(a, Most_likely_Location == "InnerMembrane")
  return(InnerMembrane_data)
}




OM_protein_table <- function(a) {
  #Tables for outermembrane proteins  only
  #takes a subset table of outer membrane data and provides a freq table
  
  a %>% 
    select(Protein_IDs, COG_category) %>% 
    cbind(Protein = str_extract(a$Fasta_headers, "(?<=9GAMM).*(?= OS)")) %>% 
    cbind((a %>% select(`Sequence_coverage_[%]`, Score,`Peptide_counts_(all)`))) %>%
    kbl(caption = "OuterMembrane proteins") %>%
    kable_classic(full_width = F, html_font = "Times new roman")
}


IM_protein_table <- function(a) {
  #Tables for Innermembrane proteins  only
  #takes a subset table of inner membrane data and provides a freq table
  
  a %>% 
    select(Protein_IDs, COG_category) %>% 
    cbind(Protein = str_extract(a$Fasta_headers, "(?<=9GAMM).*(?= OS)")) %>% 
    cbind((a %>% select(`Sequence_coverage_[%]`, Score,`Peptide_counts_(all)`))) %>%
    kbl(caption = "InnerMembrane proteins") %>%
    kable_classic(full_width = F, html_font = "Times new roman")
  
  
}

OM_appendix <-function(a) {
  #takes a subset table of outer membrane data and provides a corresponding explained COG functions of the proteins
  
  a %>% select(Protein_IDs, Description)%>%
    kbl(caption = "Description of OuterMembrane proteins functions") %>%
    kable_classic(full_width = F, html_font = "Times new roman")
 
  
}

IM_appendix <-function(a) {
  #takes a subset table of inner membrane data and provides a corresponding explained COG functions of the proteins
  a %>% select(Protein_IDs, Description)%>%
    kbl(caption = "Description of InnerMembrane proteins function") %>%
    kable_classic(full_width = F, html_font = "Times new roman")
  
  
}
































