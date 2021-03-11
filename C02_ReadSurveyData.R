### C02_ReadSurveyData.R
### Read index survey data 
### author: Minxin Lu
DataS <- read_csv("../data/207survey_De-identified_ENG.csv")
#How many kits did you request?
ggplot(data=DataS, aes(x=C)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("How many kits did you request?") +
  xlab("number of self test kits") + ylab("count")
#How many kits did you successfully distribute?
ggplot(data=DataS, aes(x=as.factor(D))) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("How many kits did you successfully distribute?") +
  xlab("number of self test kits") + ylab("count")


cd <- DataS %>% select(B,C,D)
cd$C <- as.factor(cd$C)
cd$D <- as.factor(cd$D)

ggplot(data=cd, aes(x=C, fill=D)) + 
  geom_bar(position = 'dodge') +
  geom_text(stat='count', aes(label=..count..),
            position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("How many kits did you successfully distribute?") +
  xlab("number of self test kits requested") + 
  ylab("count")+
  guides(fill=guide_legend(title="distributed"))


# want to request v.s. requested
cxcy_cd <- merge(x = cxcy, y = cd, by.x = "DC", by.y = "B")

ggplot(data=cxcy_cd, aes(x=CX, fill=C)) + 
  geom_bar(position = 'dodge') +
  geom_text(stat='count', aes(label=..count..),
            position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("want to request v.s. requested") +
  xlab("number of self test kits want to request") + 
  ylab("count")+
  guides(fill=guide_legend(title="requested"))

ggplot(data=cxcy_cd, aes(x=C, fill=CX)) + 
  geom_bar(position = 'dodge') +
  geom_text(stat='count', aes(label=..count..),
            position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("requested v.s. want to request") +
  xlab("number of self test kits requested") + 
  ylab("count")+
  guides(fill=guide_legend(title="want to request"))

# want to distribute v.s. distributed
ggplot(data=cxcy_cd, aes(x=CY, fill=D)) + 
  geom_bar(position = 'dodge') +
  geom_text(stat='count', aes(label=..count..),
            position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("want to distribute v.s. distributed") +
  xlab("want to distribute, yes=0") + 
  ylab("count")+
  guides(fill=guide_legend(title="distributed"))

# distributed v.s. want to distribute 
ggplot(data=cxcy_cd, aes(x=D, fill=CY)) + 
  geom_bar(position = 'dodge') +
  geom_text(stat='count', aes(label=..count..),
            position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("distributed v.s. want to distribute ") +
  xlab("number of self test kits distributed") + 
  ylab("count")+
  guides(fill=guide_legend(title="want to distribute"))
