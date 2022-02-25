#LLCD WH-QUESTIONS ANALYSIS
#CREATED BY KAT MARTON 10-2-2021
#LAST EDITIED [date] by [NAME]

# ORGANIZATIONAL NOTES OF FILE -----------------------
# MAJOR ANALYSES
# 1) PREDICTING NONMANUALS ON COHORT using logistic regression
#     uses the qc dataframe where each row is a question content item
#     1 0 if nonmanual is present during a question
# 2) EFFECT OF COHORT ON NONMANUAL DURATION using linear regression
#     also effect of nonmanual type on duration
#     uses the nm dataframe where each row is a nonmanual
# 3) PREDICTING OVERLAP WITH WH-WORD using logistic regression
#     effect of cohort, nonmanual type and interaction
#     uses nm dataframe where each row is a nonmanual
# ADDITIONAL ANALYSES
# 4) COMBINATIONS OF NONMANUALS
# 5) freq by nonmanual type?
# VISUALIZATIONS

#be careful of plyr/dplyr overlap of some functions, i.e. "rename"
library(lubridate)
library(readxl)
library(plyr)
library(ggplot2)
library(dplyr)
library(lme4)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(reshape2)

#set to your own working directory
setwd("C:/Users/Owner/Documents/R/LLCD_R")
nm <- read.csv("WHQ_NONMANUAL.csv")
qc <- read.csv("WHQ_ITEM.csv")
#View(nm)
#View(qc)

# ANALYSIS 1: PREDICTING NONMANUALS FROM COHORT --------------------------

#adds two new columns: nonmanual indicates 1/0 if there is a nonmanual during a question item
#total_nonmanual shows the total number of nonmanuals that occured during an item
qc2 <- qc %>%
  mutate(nonmanual = ifelse((cl+ht+sr+fb+nw+rb)==0,0,1))%>%
  mutate(total_nonmanual = rowSums(.[5:10]))

#logistic
m1 <- glmer(nonmanual ~ cohort + (1|item) + (1|ID), data=qc2, family=binomial)
tab_model(m1)

m2 <- lmer(total_nonmanual ~ cohort + (1|item) + (1|ID), data=qc2)
tab_model(m2)

m3 <- lmer(ht ~ cohort + (1|item) + (1|ID), data=qc2)
m4 <- lmer(sr ~ cohort + (1|item) + (1|ID), data=qc2)
m5 <- lmer(fb ~ cohort + (1|item) + (1|ID), data=qc2)
m6 <- lmer(nw ~ cohort + (1|item) + (1|ID), data=qc2)
m7 <- lmer(rb ~ cohort + (1|item) + (1|ID), data=qc2)
m8 <- lmer(cl ~ cohort + (1|item) + (1|ID), data=qc2)

# ANALYSIS 2: PREDICTING DURATION ----------------------------------------
#theoretically motivated interaction term between cohort and nonmanual, if we see change in types produced by each cohort
#only running duration on deaf participants
nm$wh_coordination <- ifelse(is.na(nm$wh_overlap),0,1)

deaf_nm <- nm[deaf_nm$cohort!="hearing",]
#View(deaf_nm)

#duration analysis without hearing, using linear predictor year_entry
m9 <- lmer(duration ~ year_entry + nonmanual + year_entry*nonmanual + (1|item) + (1|ID), data=deaf_nm)
tab_model(m9)

#duration analysis with hearing participants, by cohort
m10 <- lmer(duration ~ cohort + nonmanual + year_entry*nonmanual + (1|item) + (1|ID), data=nm)


# ANALYSIS 3: WH-WORD COORDINATION ---------------------------------------
#wh coordination with year entry, only deaf participants
m11 <- glmer(wh_coordination ~ year_entry + nonmanual + year_entry*nonmanual + (1|item) + (1|ID), data=deaf_nm)
#wh coordination with hearing, using cohort as predictor
m12 <- glmer(wh_coordination ~ cohort + nonmanual + cohort*nonmanual + (1|item) + (1|ID), data=nm)

# VISUALIZATIONS
#total number of nonmanuals by cohort and by nonmanual type
#summarize by participant, cohort
#either 6 bars for each cohort or 4 bars for each nonmanual type
#duration: sjplot predicted probabilities of a certain duration by cohort and nonmanual
#plot_model(m, type = "pred")


#literally just plotting the counts of total nonmanual by type and cohort. 
#what are the breakdowns of #participants in each cohort? in final we should be sure to have equal numbers
#or is there something we can do with frequencies?
fr <- ddply(qc, .(cohort),summarize,ht=sum(ht),cl=sum(cl),sr=sum(sr),fb=sum(fb),nw=sum(nw),rb=sum(rb))
fr <- melt(fr)
fr <- fr %>%
  rename(nonmanual=variable)

p1 <- ggplot(data=fr, aes(fill=nonmanual,y=value,x=cohort))+ 
  geom_bar(position="dodge", stat="identity")
p1

nm$duration[1]
