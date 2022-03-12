# WHQ RELIABILITY CODE
# CREATED BY KAT MARTON
# LAST EDITED BY KAT MARTON ON 2-16-2022

library(tidyverse)
library(lubridate)
library(readxl)
library(plyr)
library(irr)
library(dplyr)

setwd("C:/Users/Owner/Documents/R/LLCD_R")

# STEP 1: RELIABILITY DATAFRAME ----------------------------
r <- read.delim("rel_recodedv2.txt", header=F)

#cleaning filename 
r$filename <- str_sub(as.character(r$V7), 1, -5)
r$filename <- str_extract(r$filename, "(\\d\\d.*)|(JP.*)")

codes <- c("Head tilt", "Chin lift", "Shoulder raise", "Furrowed brow", "Nose wrinkle", "Raised brow")
nm_r <- r %>% 
  select(-V2,-V6,-V7)%>%
  filter(V1 %in% codes) %>%
  dplyr::rename(nonmanual=V1, start=V3, end=V4, duration=V5)

#INTERVALS FOR NM
nm_r$start <- ymd_hms(paste("2021-09-21", as.character(nm_r$start)))
nm_r$end <- ymd_hms(paste("2021-09-21", as.character(nm_r$end)))
nm_r$duration <- as.numeric(hms(as.character(nm_r$duration)))
nm_r$interval <- interval(nm_r$start, nm_r$end)
nm_r$nonmanual <- as.character(nm_r$nonmanual)

nm_r <- nm_r %>%
  select(nonmanual, filename, interval)

qc_r <- r %>%
  filter(V1=="Q Content")%>%
  select(V3,V4,V6,filename)%>%
  dplyr::rename(start=V3,end=V4,item=V6)

#it looks like there are no milliseconds, but there are, they just don't print!
qc_r$start <- ymd_hms(paste("2021-09-21", as.character(qc_r$start)))
qc_r$end <- ymd_hms(paste("2021-09-21", as.character(qc_r$end)))
qc_r$interval <- interval(qc_r$start, qc_r$end)
qc_r$item <- as.character(qc_r$item)

qc_r <- qc_r %>%
  select(item, filename, interval)

#STEP 2: adding q-content to reliability data -----------
#same code as wh data cleaning
overlap <- function(row, set){
  overlaps <- c()
  for (ii in 1:nrow(set)){
    if (int_overlaps(row$interval, set$interval[ii])&(row$filename==set$filename[ii])){
      overlaps <- append(ii, overlaps)
    }
  }
  return(overlaps)
}

qc_r$ht <- rep(0, nrow(qc_r))
qc_r$cl <- rep(0, nrow(qc_r))
qc_r$sr <- rep(0, nrow(qc_r))
qc_r$fb <- rep(0, nrow(qc_r))
qc_r$nw <- rep(0, nrow(qc_r))
qc_r$rb <- rep(0, nrow(qc_r))


for (ii in 1:nrow(qc_r)){
  nmIndices = overlap(qc_r[ii,], nm_r)
  
  for (jj in nmIndices){
    if (nm_r[jj,]$nonmanual=="Head tilt"){
      qc_r$ht[ii] = qc_r$ht[ii]+1
    }else if (nm_r[jj,]$nonmanual=="Chin lift"){
      qc_r$cl[ii] = qc_r$cl[ii]+1
    }else if (nm_r[jj,]$nonmanual=="Shoulder raise"){
      qc_r$sr[ii] = qc_r$sr[ii]+1
    }else if (nm_r[jj,]$nonmanual=="Furrowed brow"){
      qc_r$fb[ii] = qc_r$fb[ii]+1
    }else if (nm_r[jj,]$nonmanual=="Nose wrinkle"){
      qc_r$nw[ii] = qc_r$nw[ii]+1
    }else if (nm_r[jj,]$nonmanual=="Raised brow"){
      qc_r$rb[ii] = qc_r$rb[ii]+1
    }
  }
}

b <- read_excel("WHQ Participants.xlsx")
b <- b %>%
  dplyr::mutate(num=row_number())

b$`Video ID`[49] <- b$notes[49]
b$`Video ID`[51] <- b$notes[51]
b$`Video ID`[52] <- b$notes[52]

b$filename <- as.character(b$`Video ID`)
b$filename <- str_extract(b$filename, "[^.]+")

nm_r$filename <- tolower(nm_r$filename)
qc_r$filename <- tolower(qc_r$filename)
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


qc_r_merged <- merge(qc_r, b, by.x="filename", by.y="filename")%>%
  dplyr::rename(ID = `Participant ID`)
qc_r_final <- qc_r_merged%>%
  select(ID,item,interval,ht,cl,sr,fb,nw,rb)

write.csv(qc_r_final, "rel_frame.csv")

# STEP 3: ORIGINAL DATA SUBSET CORRESPONDING TO RELIABILITY ---------------------
nm <- read.csv("WHQ_NONMANUAL_v2.csv", header=T)
qc_orig <- read.csv("WHQ_ITEM_v2.csv", header=T)
rel <- read.csv("rel_frame.csv", header=T) %>%
  select(-X)

ids <- unique(rel$ID)
original <- qc_orig %>%
  select(ID, item, interval, ht, cl, sr, fb, nw, rb)%>%
  dplyr::rename(ht2=ht,cl2=cl,sr2=sr,fb2=fb,nw2=nw,rb2=rb)%>%
  filter(ID %in% ids)

cr <- merge(original, rel, by=c("ID", "item", "interval"))

nrow(rel[rel$ID=="191.0",])
nrow(cr[cr$ID=="191.0",])

unique(cr$item)
#RELIABILITY ANALYSIS-------
#arrange data by item, have #of nonmanuals and types
#each nonmanual for coder 1 and coder 2
#how well agree on when nonmanual isn't there
#include correlations to show when percent agreement is closer 
#participant | q-content | # nonmanual 1 | # nonmanual 2 | etc
#pyers mallory has reliability in coding section, papers that pyers sharing
#cohen's kappa computes how big disagreement is

rel <- nm_r_final %>%
  select(nonmanual,item,ID)%>%
  mutate(ht1=ifelse(nonmanual=="Head tilt",1,0),
         cl1=ifelse(nonmanual=="Chin lift",1,0),
         sr1=ifelse(nonmanual=="Shoulder raise",1,0),
         fb1=ifelse(nonmanual=="Furrowed brow",1,0),
         nw1=ifelse(nonmanual=="Nose wrinkle",1,0),
         rb1=ifelse(nonmanual=="Raised brow",1,0),)

rel <- ddply(rel,.(ID,item),summarize, 
              ht1=sum(ht1),
              cl1=sum(cl1),
              sr1=sum(sr1),
              fb1=sum(fb1),
              nw1=sum(nw1),
              rb1=sum(rb1))

nm1 <- nm %>%
  select(nonmanual,item,ID)%>%
  mutate(ht2=ifelse(nonmanual=="Head tilt",1,0),
         cl2=ifelse(nonmanual=="Chin lift",1,0),
         sr2=ifelse(nonmanual=="Shoulder raise",1,0),
         fb2=ifelse(nonmanual=="Furrowed brow",1,0),
         nw2=ifelse(nonmanual=="Nose wrinkle",1,0),
         rb2=ifelse(nonmanual=="Raised brow",1,0),)

nm1 <- ddply(nm1,.(ID,item),summarize, 
             ht2=sum(ht2),
             cl2=sum(cl2),
             sr2=sum(sr2),
             fb2=sum(fb2),
             nw2=sum(nw2),
             rb2=sum(rb2))

reliability <- merge(rel, nm1, by=c("ID", "item"))




# STEP 3: MATCH OVERLAPPING INTERVALS --------
#the overlap must match both nonmanual type and filename
nm$rel <- rep(NA, nrow(nm))
for(ii in 1:nrow(nm)){
  match <- int_overlaps(nm[ii,]$interval, nm_r$interval)&(nm[ii,]$filename==nm_r$filename)&(nm[ii,]$nonmanual==nm_r$nonmanual)
  nm$rel[ii] = ifelse(TRUE %in% match,1,0)
}

#same thing for rel, find the ones only coded in rel
nm_r$rel <- rep(NA, nrow(nm_r))
for(ii in 1:nrow(nm_r)){
  match <- int_overlaps(nm_r[ii,]$interval, nm$interval)&(nm_r[ii,]$filename==nm$filename)&(nm_r[ii,]$nonmanual==nm$nonmanual)
  nm_r$rel[ii] = ifelse(TRUE %in% match,1,0)
}

unmatched <- nm_r[nm_r$rel==0,]

reliability <- rbind(nm, unmatched)
reliability$rel_code <- ifelse(reliability$rel==1,reliability$nonmanual,"unmatched")
reliability <- reliability %>%
  select(nonmanual, rel_code)

kappa2(reliability) #uhhhh 0.524 is cohen's kappa
agree(reliability)  #% agreement is 57.7

#without the unmatched reliability points
nm_rel <- nm 
nm_rel$rel_code <-ifelse(nm_rel$rel==1,nm_rel$nonmanual,"unmatched")
nm_rel <- nm_rel %>%
  select(nonmanual, rel_code)

kappa2(nm_rel)
agree(nm_rel)

#RELIABILITY BY NONMANUAL TYPE: which is the least reliably coded? -------------
rel_ht <- reliability[reliability$nonmanual=="Head tilt",]
agree(rel_ht)

rel_sr <- reliability[reliability$nonmanual=="Shoulder raise",]
agree(rel_sr)

rel_nw <- reliability[reliability$nonmanual=="Nose wrinkle",]
agree(rel_nw)

rel_fb <- reliability[reliability$nonmanual=="Furrowed brow",]
agree(rel_fb)

rel_rb <- reliability[reliability$nonmanual=="Raised brow",]
agree(rel_rb)

rel_cl <- reliability[reliability$nonmanual=="Chin lift",]
agree(rel_cl)



