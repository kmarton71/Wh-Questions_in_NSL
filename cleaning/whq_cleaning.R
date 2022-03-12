#LLCD WH-QUESTION STUDY
#DATA CLEANING DOCUMENT
#CREATED BY KAT MARTON 9-20-2021
#LAST EDITED 3-11-2022 by KAT MARTON

#ORGANIZATION OF FILE --------------------------
#This file is the initial cleaning file, creating the nm and qc dataframes.
#qc_cleaning is the next cleaning file. 
#WARNING: The code in this file takes a VERY long time to run.
#1 d, referring to the raw dataset from ELAN
#2 nm, the frame of nonmanuals and their intervals
#3 qc, the frame containing the question content intervals and item information
#4 wh, the frame of wh words and their intervals
#secret 5th and 6th dataframes eg and sp, standing for the english gloss and spanish translation

#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("readxl")

#CREATION OF d DATAFRAME--------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(plyr)

setwd("C:/Users/Owner/Documents/R/LLCD_R")
d <- read.delim("WHQ_DATA_RECODED.txt", header=F)
b <- read_excel("WHQ Participants.xlsx")

#cleaning filename 
d$filename <- str_sub(as.character(d$V7), 1, -5)
d$filename <- str_extract(d$filename, "(\\d\\d.*)|(JP.*)")

#CREATING NM DATAFRAME ---------------------------------
#extracting rows corresponding to nonmanuals
codes <- c("Head tilt", "Chin lift", "Shoulder raise", "Furrowed brow", "Nose wrinkle", "Raised brow")
nm <- d %>% 
  select(-V2,-V6,-V7)%>%
  filter(V1 %in% codes) %>%
  dplyr::rename(nonmanual=V1, start=V3, end=V4, duration=V5)

#INTERVALS FOR NM
nm$start <- ymd_hms(paste("2021-09-21", as.character(nm$start)))
nm$end <- ymd_hms(paste("2021-09-21", as.character(nm$end)))
nm$duration <- as.numeric(hms(as.character(nm$duration)))
nm$interval <- interval(nm$start, nm$end)
nm$nonmanual <- as.character(nm$nonmanual)

#View(nm)
#write.csv(nm, "nm_original.csv")
#CREATING QC DATAFRAME-------------------------------------
#dataframe with item interval information
qc <- d %>%
  filter(V1=="Q Content")%>%
  select(V3,V4,V6,filename)%>%
  dplyr::rename(start=V3,end=V4,item=V6)

#it looks like there are no milliseconds, but there are, they just don't print!
qc$start <- ymd_hms(paste("2021-09-21", as.character(qc$start)))
qc$end <- ymd_hms(paste("2021-09-21", as.character(qc$end)))
qc$interval <- interval(qc$start, qc$end)
qc$item <- as.character(qc$item)

#CREATING WH DATAFRAME ---------------------------------------
wh <- d %>%
  filter(V1=="Wh word")%>%
  select(V3,V4,V6,filename)%>%
  dplyr::rename(start=V3,end=V4,wh_word=V6)

#intervals for wh-words
wh$start <- ymd_hms(paste("2021-09-21", as.character(wh$start)))
wh$end <- ymd_hms(paste("2021-09-21", as.character(wh$end)))
wh$interval <- interval(wh$start, wh$end)
wh$wh_word <- as.character(wh$wh_word)

#ADDITIONAL DATAFRAMES FOR ENGLISH GLOSS AND SPANISH ---------------------------------------------
eg <- d %>%
  filter(V1=="English GLOSS")%>%
  select(V3,V4,V6,filename)%>%
  dplyr::rename(start=V3,end=V4,gloss=V6)

#intervals for english gloss
eg$start <- ymd_hms(paste("2021-09-21", as.character(eg$start)))
eg$end <- ymd_hms(paste("2021-09-21", as.character(eg$end)))
eg$interval <- interval(eg$start, eg$end)
eg$gloss <- as.character(eg$gloss)
#filename didn't populate in row 506
eg$filename[506] <- "51"

sp <- d %>%
  filter(V1=="Spanish")%>%
  select(V3,V4,V6,filename)%>%
  dplyr::rename(start=V3,end=V4,spanish=V6)

#intervals for spanish
sp$start <- ymd_hms(paste("2021-09-21", as.character(sp$start)))
sp$end <- ymd_hms(paste("2021-09-21", as.character(sp$end)))
sp$interval <- interval(sp$start, sp$end)
sp$spanish <- as.character(sp$spanish)


#ASSIGNING Q CONTENT TO NONMANUAL --------------------------------------------------------------------------
#the following function finds one overlap for a given interval from a list of intervals. 
#i.e. it does not check EVERY interval for overlap, but returns after it finds a match. Not suitable for combos
#First parameter is a full row of a dataset, which assumes a variable "interval" and a variable "filename"
#Second parameter is a dataset that includes a "interval", "filename" column.
#returns the index of the overlap.
one_overlap <- function(row, set){
  for (ii in 1:nrow(set)){
      if (int_overlaps(row$interval, set$interval[ii])&(row$filename==set$filename[ii])){
        return(ii)
      }
  }
  return("error, there is no overlap when there should be one")
}

#add information to qc frame: english gloss, spanish, wh-word
#will take forever to load
qc$wh_word <- rep(NA, nrow(qc))
for(ii in 1:nrow(qc)){
  qc$wh_word[ii] = wh$wh_word[one_overlap(qc[ii,], wh)]
}


qc$gloss <- rep(NA, nrow(qc))
for(ii in 1:nrow(qc)){
  qc$gloss[ii] = eg$gloss[one_overlap(qc[ii,], eg)]
}

qc$spanish <- rep(NA, nrow(qc))
for(ii in 1:nrow(qc)){
  qc$spanish[ii] = sp$spanish[one_overlap(qc[ii,], sp)]
}


write.csv(qc, "qc_frame.csv")

#NM SECTION --- LONG LOADING TIME
#add q content information to each nonmanual row
nm$item <- rep(NA, nrow(nm))
nm$gloss <- rep(NA, nrow(nm))
nm$spanish <- rep(NA, nrow(nm))
nm$wh_word <- rep(NA, nrow(nm))

for (ii in 1:nrow(nm)){
  qcIndex = one_overlap(nm[ii,], qc)
  nm$item[ii] = qc$item[qcIndex]
  nm$gloss[ii] = qc$gloss[qcIndex]
  nm$spanish[ii] = qc$spanish[qcIndex]
  nm$wh_word[ii] = qc$wh_word[qcIndex]
}

#adding wh-word overlap on specific nonmanuals
nm$wh_overlap <- rep(NA, nrow(nm))
for (ii in 1:nrow(nm)){
  nm$wh_overlap[ii] = wh$wh_word[one_overlap(nm[ii,], wh)]
}


write.csv(nm, "nm_frame.csv")

#COMBINATIONS BETWEEN NONMANUALS --- IGNORE FOR NOW
#New function: all_overlap, to calculate any and all (instead of just the first, as in one_overlap) interval overlaps.
#This is matching one interval to all intervals in a dataset
#First parameter is a row of a dataset that contains a variable called "interval" and a variable called "filename"
#Second parameter is a dataset. 
#dataset contains first parameter, so there will always be at least one overlap returned, of self. 
#this function is only made to apply to the nm dataframe.
#RETURNS a vector containing all nonmanual types that overlapped.
# all_overlap <- function(row, set){
#   combos = c()
#   for (ii in 1:nrow(set)){
#     if (int_overlaps(row$interval, set$interval[ii])&(row$filename==set$filename[ii])){
#       combos = append(combos,set$nonmanual[ii])
#     }
#   }
#   return(combos)
# }

#add 6 new columns corresponding to each nonmanual type.
#binary variable, 1 for combination, 0 for no
#(Head tilt", "Chin lift", "Shoulder raise", "Furrowed brow", "Nose wrinkle", "Raised brow")
#ht, cl, sr, fb, nw, rb
# 
# 
# nm1$ht <- rep(NA,nrow(nm1))
# nm1$cl <- rep(NA,nrow(nm1))
# nm1$sr <- rep(NA,nrow(nm1))
# nm1$fb <- rep(NA,nrow(nm1))
# nm1$nw <- rep(NA,nrow(nm1))
# nm1$rb <- rep(NA,nrow(nm1))
# 
# for (ii in 1:nrow(nm1)){
#   c = all_overlap(nm1[ii,], nm)
#   nm1$ht[ii] = ifelse("Head tilt"%in%c,1,0)
#   nm1$cl[ii] = ifelse("Chin lift"%in%c,1,0)
#   nm1$sr[ii] = ifelse("Shoulder raise"%in%c,1,0)
#   nm1$fb[ii] = ifelse("Furrowed brow"%in%c,1,0)
#   nm1$nw[ii] = ifelse("Nose wrinkle"%in%c,1,0)
#   nm1$rb[ii] = ifelse("Raised brow"%in%c,1,0)
# }

# #assigning NA values to self combinations in nm
# for (ii in 1:nrow(nm)){
#   if (nm$nonmanual[ii]=="Head tilt"){
#     nm$ht[ii]=NA
#   }else if(nm$nonmanual[ii]=="Chin lift"){
#     nm$cl[ii]=NA
#   }else if(nm$nonmanual[ii]=="Shoulder raise"){
#     nm$sr[ii]=NA
#   }else if(nm$nonmanual[ii]=="Furrowed brow"){
#     nm$fb[ii]=NA
#   }else if(nm$nonmanual[ii]=="Nose wrinkle"){
#     nm$nw[ii]=NA
#   }else if(nm$nonmanual[ii]=="Raised brow"){
#     nm$rb[ii]=NA
#   }
# }
#new dataframe for combinations only: each row is a different combination?




