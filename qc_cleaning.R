#WHQ_CLEANING2
#Initial processing of data has been done, 
#this file is to organize dataframe unique on item and participant
#needs qc_frame and nm_frame as created by whq_cleaning file
#LAST EDITED BY KAT MARTON ON 3/11/2022

#WARNING: THIS CODE FILE IS ALSO VERY LONG
library(tidyverse)
library(lubridate)
library(readxl)
library(plyr)

#set to your own working directory
#setwd("C:/Users/Owner/Documents/R/LLCD_R")

#function to determine overlaps of qc with nonmanuals
all_overlap <- function(row, set){
  overlaps <- c()
  for (ii in 1:nrow(set)){
    if (int_overlaps(row$interval, set$interval[ii])&(row$filename==set$filename[ii])){
      overlaps <- append(ii, overlaps)
    }
  }
  return(overlaps)
}

qcf<- read.csv("qc_frame.csv")
nmf <- read.csv("nm_frame.csv")
nmf$nonmanual <- as.character(nmf$nonmanual)

qcf$filename[qcf$filename=="145 (1)"]<-"145"
nmf$filename[nmf$filename=="145 (1)"]<-"145"

qcf$start <- ymd_hms(as.character(qcf$start))
qcf$end <- ymd_hms(as.character(qcf$end))
qcf$interval <- interval(qcf$start,qcf$end)

nmf$start <- ymd_hms(as.character(nmf$start))
nmf$end <- ymd_hms(as.character(nmf$end))
nmf$interval <- interval(nmf$start, nmf$end)

qcf <- qcf[qcf$filename=="145",]

qcf$ht <- rep(0, nrow(qcf))
qcf$cl <- rep(0, nrow(qcf))
qcf$sr <- rep(0, nrow(qcf))
qcf$fb <- rep(0, nrow(qcf))
qcf$nw <- rep(0, nrow(qcf))
qcf$rb <- rep(0, nrow(qcf))

for (ii in 1:nrow(qcf)){
  nmIndices = all_overlap(qcf[ii,], nmf)
  
  for (jj in nmIndices){
    if (nmf[jj,]$nonmanual=="Head tilt"){
      qcf$ht[ii] = qcf$ht[ii]+1
    }else if (nmf[jj,]$nonmanual=="Chin lift"){
      qcf$cl[ii] = qcf$cl[ii]+1
    }else if (nmf[jj,]$nonmanual=="Shoulder raise"){
      qcf$sr[ii] = qcf$sr[ii]+1
    }else if (nmf[jj,]$nonmanual=="Furrowed brow"){
      qcf$fb[ii] = qcf$fb[ii]+1
    }else if (nmf[jj,]$nonmanual=="Nose wrinkle"){
      qcf$nw[ii] = qcf$nw[ii]+1
    }else if (nmf[jj,]$nonmanual=="Raised brow"){
      qcf$rb[ii] = qcf$rb[ii]+1
    }
  }
}

qcf2 <- qcf %>%
  select(filename,item,gloss,spanish,ht,cl,sr,fb,nw,rb)


#DEMOGRAPHICS FILE,
#manual cleaning 
b <- read_excel("WHQ Participants.xlsx")#NOT INCLUDED IN GIT, has personal info

b <- b %>%
  dplyr::mutate(num=row_number())

b$`Video ID`[49] <- b$notes[49]
b$`Video ID`[51] <- b$notes[51]
b$`Video ID`[52] <- b$notes[52]

b$filename <- as.character(b$`Video ID`)
b$filename <- str_extract(b$filename, "[^.]+")

nmf$filename <- tolower(nmf$filename)
qcf$filename <- tolower(qcf$filename)
b$filename <- tolower(b$filename)

#manual fixing of the Weird 
b$filename[49] <- str_sub(b$filename[49], 14, -1)
b$filename[50] <- "1701 whq karla"
b$filename[51] <- "jp0771h"
b$filename[52] <- str_sub(b$filename[52], 14, -1)
b$filename[12] <- str_sub(b$filename[12], 10, -1)
b$filename[14] <- str_sub(b$filename[14], 11, -1)
b$filename[19] <- str_sub(b$filename[19], 11, -1)
b$filename[26] <- str_sub(b$filename[26], 10, -1)
b$filename[41] <- "2010 wh 11"
b$filename[38] <- "2010_wh 04 small"
b$filename[4] <- "36 wh"
b$filename[10] <- "543"
b$filename[31] <- "241"
b$filename[32] <- "410"
b$filename[2] <- "41"
b$filename[5] <- "51"


nm_merged <- merge(nmf, b, by.x="filename", by.y="filename")
qc_merged <- merge(qcf, b, by.x="filename", by.y="filename")

nm_final <- nm_merged%>%
  dplyr::rename(cohort = `Cohort (hearing, first, second, third)`, ID=`Participant ID`,year_tested=`year of testing`,age=`Age at Test`,year_entry = `Year of Entry`)%>%
  select(nonmanual,ID,duration,wh_word,wh_overlap,item,spanish,gloss,cohort,Name,Sex,age,year_tested,year_entry,interval)
View(nm_final)


qc_final <- qc_merged %>%
  dplyr::rename(cohort = `Cohort (hearing, first, second, third)`, ID=`Participant ID`,year_tested=`year of testing`,age=`Age at Test`,year_entry = `Year of Entry`)%>%
  select(ID, filename,item,ht,cl,sr,fb,nw,rb,cohort,gloss,spanish,Name,Sex,age, year_tested,year_entry)


qc3 <- read.csv("WHQ_ITEM.csv", header=T)%>%
  select(-X)
qc3[179,]$item="name"
#some manual cleaning
qc3$item[qc3$item=="motherâ???Ts birthday"]<-"mother's birthday"

new_qc <- rbind(qc3, qc_final)
write.csv(new_qc, "WHQ_ITEM.csv")
write.csv(nm_final, "WHQ_NONMANUAL.csv")
