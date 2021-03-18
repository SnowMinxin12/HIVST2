### C01_ReadBaselineData
### Reading in baseline data
### author: Minxin Lu
### input:
### output:
library(readr)
library(dbplyr)
library(tidyverse)
library(ggplot2)
DataB <- read_csv("../data/309Index_De-identified_ENG.csv")


# How many self test kits do you want to request this time? 0-5
table(DataB$CX) #no missing data but only 1-5? (at most distributed 4 to others)
ggplot(data=DataB, aes(x=CX)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("How many self test kits do you want to request this time?") +
  xlab("number of self test kits") + ylab("count")

# Do you plan to give the kit to others?
ggplot(data=DataB, aes(x=as.factor(CY))) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("How many self test kits do you want to request this time?") +
  xlab("number of self test kits") + ylab("count")

table(DataB$CY)#NA=102

# do all people applied >1 distributed to others? - NO
# Among all people applied =n did they distributed to others?
cxcy <- DataB %>% select(DC,CX,CY)
cxcy$CX <- as.factor(cxcy$CX)
cxcy$CY <- as.factor(cxcy$CY)

ggplot(data=cxcy, aes(x=CX, fill=CY)) + 
  geom_bar(position = 'dodge') +
  geom_text(stat='count', aes(label=..count..),
            position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Do you plan to give the kit to others?") +
  xlab("number of self test kits wanted to request") + 
  ylab("count")+
  guides(fill=guide_legend(title="Yes=0,No=1"))

# what are the difference between intent to distribute and 


summary(DataB)
# variables that have no variation: D, E, BM.A,BM.B,CF.A,CF.B,CM,CO.A,CO.D
# check categorical variables: S, U 
#nonrelavent variables:  AK,AV,BG,BS,CB,CK(his/her initial name)