### C13_JointData
### author: Minxin Lu
### date: 2021-4-16
### join 3 datasets together and removed some identification variables
### input: summarizedB.csv, summarizedA.csv, summarizedS.csv from C12
### output: jointdata.csv=one joint data by index confirmation code
summarizedB <- read_csv("../data/summarizedB.csv")
summarizedA <- read_csv("../data/summarizedA.csv")
summarizedS <- read_csv("../data/summarizedS.csv")

summarizedB <- summarizedB[,colnames(summarizedB)!="X1"]
summarizedA <- summarizedA[,colnames(summarizedA)!="X1"]
summarizedS <- summarizedS[,colnames(summarizedS)!="X1"]

colnames(summarizedB) <- paste0("B.",colnames(summarizedB))
colnames(summarizedA) <- paste0("A.",colnames(summarizedA))
colnames(summarizedS) <- paste0("S.",colnames(summarizedS))

summarizedBS <- merge(x=summarizedB,y=summarizedS, by.x="B.confirm_code",by.y="S.confirm_code")
summarizedBSA <- merge(x=summarizedBS,y=summarizedA, by.x="B.confirm_code",by.y="A.confirm_code")

# remove irrelavent variables
varRemoved <- c("B.hang_out_name","B.hang_out_name2","B.hang_out_name3","B.hang_out_name4",
                "B.hang_out_name5","B.hang_out_name6","S.survey_date")
summarizedBS <- summarizedBS[,!colnames(summarizedBS) %in% varRemoved]
summarizedBSA <- summarizedBSA[,!colnames(summarizedBSA) %in% varRemoved]

write.csv(summarizedBS, file="../data/jointdataBS.csv")
write.csv(summarizedBSA, file="../data/jointdataBSA.csv")
