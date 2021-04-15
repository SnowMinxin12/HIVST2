### C13_JointData
### author: Minxin Lu
### date: 2021-4-14
### (1) join 3 datasets together
### input: C12SkipPatterbSumAlters.R
### output: one joint data by index confirmation code
summarizedB <- read_csv("../data/summarizedB.csv")
summarizedA <- read_csv("../data/summarizedA.csv")
summarizedS <- read_csv("../data/summarizedS.csv")

summarizedB <- summarizedB[,colnames(summarizedB)!="X1"]
summarizedA <- summarizedA[,colnames(summarizedA)!="X1"]
summarizedS <- summarizedS[,colnames(summarizedS)!="X1"]

colnames(summarizedB) <- paste0("B.",colnames(summarizedB))
colnames(summarizedA) <- paste0("A.",colnames(summarizedA))
colnames(summarizedS) <- paste0("S.",colnames(summarizedS))

summarizedBA <- merge(x=summarizedB,y=summarizedA, by.x="B.confirm_code",by.y="A.confirm_code")
summarizedBAS <- merge(x=summarizedBA,y=summarizedS, by.x="B.confirm_code",by.y="S.confirm_code")

# remove irrelavent variables
varRemoved <- c("B.confirm_code","B.hang_out_name","B.hang_out_name2","B.hang_out_name3","B.hang_out_name4",
                "B.hang_out_name5","B.hang_out_name6")
summarizedBAS <- summarizedBAS[,!colnames(summarizedBAS) %in% varRemoved]

write.csv(summarizedBAS, file="../data/jointdata.csv")
