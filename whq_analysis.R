#LLCD WH-QUESTIONS ANALYSIS
#CREATED BY KAT MARTON 10-2-2021
#LAST EDITIED [2-16-2022] by [KAT MARTON]
#For paper entitled "From seed to system: The emergence of non-manual markers for wh-questions in Nicaraguan Sign Language"
#submitted to a Special Issue on "Language Emergence" Submitted to the journal "Languages"

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
library(forcats)
library(RColorBrewer)
library(tidyr)

#set to your own working directory
#setwd("C:/Users/Owner/Documents/R/LLCD_R")

#dataframes with names, original IDs
#nm <- read.csv("WHQ_NONMANUAL.csv") #only nonmanuals
#qc <- read.csv("WHQ_ITEM.csv")

#dataframes with random ids and no names
nm <- read.csv("WHQ_NM_ANON.csv")
qc <- read.csv("WHQ_QC_ANON.csv")

##DEMOGRAPHICS-----------
#number of members of each cohort tested during which year
year_test<-ddply(nm, .(cohort, ID, year_tested), summarise,
                 age=mean(age, na.rm=TRUE))
table<-ddply(year_test, .(cohort, year_tested), summarise,
             N=length(year_tested))

#basic demographics
demographics<-ddply(nm, .(ID, Sex, cohort, year_tested), summarise,
                    age=mean(age, na.rm=TRUE))
#summarizes gender breakdown       
demo_2<-ddply(demographics,.(cohort, Sex), summarise,
              N=length(ID),
              age=mean(age))
#summarizes age information
demo_3<-ddply(demographics, .(cohort), summarise, 
              Mean=(mean(age)),
              Minimum=(min(age)),
              Maximum=(max(age)))

# ANALYSIS 1: PREDICTING NONMANUALS FROM COHORT --------------------------

#adds two new columns: nonmanual indicates 1/0 if there is a nonmanual during a question item
#total_nonmanual shows the total number of nonmanuals that occured during an item

qc2 <- qc %>%
  mutate(nonmanual = ifelse((cl+ht+sr+fb+nw+rb)==0,0,1))%>%
  mutate(total_nonmanual = rowSums(.[4:9]))
qc2$cohort <- as.factor(qc2$cohort)
qc2$cohort <- relevel(qc2$cohort, ref="hearing")

#Mixed effects linear regression predicting total nonmanual use
m2 <- lmer(total_nonmanual ~ cohort + (1|item) + (1|ID), data=qc2)
tab_model(m2)
#models for each of the six nonmanual types
m3 <- lmer(ht ~ cohort + (1|item) + (1|ID), data=qc2)
m4 <- lmer(sr ~ cohort + (1|item) + (1|ID), data=qc2)
m5 <- lmer(fb ~ cohort + (1|item) + (1|ID), data=qc2)
m6 <- lmer(nw ~ cohort + (1|item) + (1|ID), data=qc2)
m7 <- lmer(rb ~ cohort + (1|item) + (1|ID), data=qc2)
m8 <- lmer(cl ~ cohort + (1|item) + (1|ID), data=qc2)
tab_model(m4,m6,m7,m8,m3,m5)

# ANALYSIS 2: PREDICTING DURATION ----------------------------------------
#theoretically motivated interaction term between cohort and nonmanual, if we see change in types produced by each cohort
#only running duration on deaf participants
nm$wh_coordination <- ifelse(is.na(nm$wh_overlap),0,1)
nm$cohort <- relevel(nm$cohort, ref="hearing")
nm$nonmanual <- relevel(nm$nonmanual, ref="Chin lift")

#duration analysis with hearing participants, by cohort. Hearing cohort and chin lift is reference level
m9 <- lmer(duration ~ cohort + nonmanual + cohort*nonmanual + (1|item) + (1|ID), data=nm)
tab_model(m9)

# ANALYSIS 3: WH-WORD CO-ARTICULATION ---------------------------------------

#wh co-articulation with hearing, using cohort as predictor
m10 <- glmer(wh_coordination ~ cohort + nonmanual + cohort*nonmanual + (1|item) + (1|ID), data=nm, family=binomial)
tab_model(m10)


# VISUALIZATIONS-----------------------------------
#set constant color palate
myColors <- brewer.pal(6,"Set2")
names(myColors) <- levels(nm$nonmanual)
colScale <- scale_fill_manual(name = "nonmanual",values = myColors, limits=c("Shoulder raise", "Nose wrinkle", "Brow raise", "Chin lift", "Head tilt", "Brow furrow"))

# PLOT 1 FIGURE 2-----------
#total number of nonmanuals by cohort and by nonmanual type
#note i set the order of the legend to match the first nonmanual plot. 

#Plotting the proportion of questions with nonmanuals
#creating a dataframe that summarizes the mean nonmanual use
NM_avg<-ddply(qc2,.(ID, cohort), summarise,
           Head_tilt=mean(ht, na.rm=TRUE),
           Chin_lift=mean(cl, na.rm=TRUE),
           Shoulder_raise=mean(sr, na.rm=TRUE),
           Brow_furrow=mean(fb, na.rm=TRUE),
           Nose_wrinkle=mean(nw, na.rm=TRUE),
           Brow_raise=mean(rb, na.rm=TRUE)
)

NM_avg <- melt(NM_avg,id.vars="cohort",
               measure.vars = c("Head_tilt","Chin_lift","Shoulder_raise","Brow_furrow","Nose_wrinkle","Brow_raise"))
NM_avg <- NM_avg %>%
  dplyr::rename(nonmanual=variable)

NM_avg$nonmanual<-recode_factor(NM_avg$nonmanual, "Shoulder_raise"="Shoulder raise", "Nose_wrinkle"="Nose wrinkle", "Brow_raise"="Brow raise", "Chin_lift"="Chin lift", "Head_tilt"="Head tilt", "Brow_furrow"="Brow furrow")

#Plots Figure 2: Proportion of Questions with Each Nonmanual across Cohort
p1<-NM_avg%>%
  mutate(nonmanual= fct_reorder(nonmanual, value, .fun='mean')) %>%
  ggplot(aes(x = cohort, y = value)) + 
  geom_boxplot(aes(fill=nonmanual), width = 0.5, size = 0.4,
               position = position_dodge(.6), show.legend=TRUE, outlier.shape=NA)+
  ylim(0,1)+
  labs(title="Frequency of Questions with Each Non-manual across Cohorts", y="Proportion of Questions")+
  theme_classic()
p1+colScale 

# PLOT 2 FIGURE 3-----------
#plot Figure 3: duration by cohort, nonmanual, duration
# Creates a new dataframe to plot duration
dur<-ddply(nm,.(ID, cohort, nonmanual), summarise,
                   N = length(duration),
                   mean = mean(duration, na.rm=TRUE),
                   sd = sd(duration, na.rm=TRUE),
                   se = sd / sqrt(N),
                  maximum=max(duration)
)

#ordered legend according to first plot
p2<- dur%>%
  mutate(nonmanual= fct_relevel(nonmanual, "Shoulder raise", "Nose wrinkle", "Brow raise",
                                "Chin lift", "Head tilt", "Brow furrow")) %>%
  mutate(cohort = fct_relevel(cohort, "hearing", "first", "second", 
                              "third")) %>%
  ggplot(aes(x = cohort, y = mean)) + 
  geom_boxplot(aes(fill=nonmanual), width = 0.5, size = 0.4,
               position = position_dodge(.6), show.legend=TRUE, outlier.shape=NA)+
  labs(title="Average Duration of each Non-manual across Cohorts", y="Seconds")+
  theme_classic()
p2+colScale

# PLOT 3 FIGURE 4-----------
#plot Figure 4 wh-co-articulation
coord<-ddply(nm,.(ID, cohort, nonmanual), summarise,
           N = length(wh_coordination),
           mean = mean(wh_coordination, na.rm=TRUE),
           sum = sum(wh_coordination, na.rm=TRUE),
           sd = sd(wh_coordination, na.rm=TRUE),
           se = sd / sqrt(N)
)

p3<- coord%>%
  mutate(nonmanual= fct_relevel(nonmanual, "Shoulder raise", "Nose wrinkle", "Brow raise",
                                "Chin lift", "Head tilt", "Brow furrow")) %>%
  ggplot(aes(x = cohort, y = sum)) + 
  geom_boxplot(aes(fill=nonmanual), width = 0.5, size = 0.4,
               position = position_dodge(.6), show.legend=TRUE, outlier.shape=NA)+
  labs(title="Number of Questions where the Non-manual is \nCoarticulated with the Wh-Question Word", y="Number of questions \nwith nonmanuals")+
  theme_classic() 
p3+colScale

