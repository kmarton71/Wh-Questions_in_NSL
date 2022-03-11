#File created to deidentify the whq data for publication
#last edited by Kat Marton 3/11/2022

library(tidyverse)
setwd("C:/Users/Owner/Documents/R/LLCD_R")

nm <- read.csv("WHQ_NONMANUAL.csv")
qc <- read.csv("WHQ_ITEM.csv")
nm$ID <- gsub("\\..*","",nm$ID)

random_id <- sample(100:999, size=50, replace=FALSE)


#kind of weird way to make sure new random IDs match up in both dfs
rn <- data.frame(nm$ID) %>% 
  dplyr::rename(ID = nm.ID) %>% 
  mutate(frame="nm")
rn2 <- data.frame(qc$ID) %>% 
  dplyr::rename(ID = qc.ID) %>% 
  mutate(frame="qc")

rnn <- rbind(rn,rn2)

rnnid <- rnn %>%
  group_by(ID)%>%
  dplyr::mutate(id = cur_group_id())

nmr <- nm
qcr <- qc
nmr$ID <- rnnid$id[rnnid$frame=="nm"]
qcr$ID <- rnnid$id[rnnid$frame=="qc"]

nmr <- nmr%>%
  select(-Name,-X.1, -X,-interval)
qcr <- qcr%>%
  select(-Name,-X.1, -X, -filename)

write.csv(nmr,"WHQ_NM_ANON.csv")
write.csv(qcr,"WHQ_QC_ANON.csv")

