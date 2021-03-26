### C01_ReadData
### Reading in baseline data
### author: Minxin Lu
### date: 2021-3-22
### input: 309index_De-identified_ENG_wdistribution.csv
###        207survey_De-identified_ENG.csv
###        269alters_De-identified.csv
### output: DataB = baseline index data
###         DataS = survey index data
###         DataA = alter index data

library(readr)
DataB <- read_csv("../data/309index_De-identified_ENG_wdistribution.csv")
DataS <- read_csv("../data/207survey_De-identified_ENG.csv")
DataA <- read_csv("../data/269alters_De-identified.csv")
