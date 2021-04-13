### C13_MatrixforAnalysis
### author: Minxin Lu
### date: 2021-4-12
### 
### (1) join 3 datasets together
### (2) delete all variables with missing > 50% or skip>50%
### input: C12SkipPatterbSumAlters.R
### output: matrix for analysis (1) complete data (2) imputed data
source("C11_Check_Variable_Class.R")
length(unique(DataB$DC))#309
length(unique(DataA$A))# 140 <269
length(unique(DataS$B)) #207
length(intersect(unique(DataA$A),unique(DataB$DC)))# 140
length(intersect(unique(DataA$A),unique(DataS$B)))# 88
length(intersect(unique(DataB$DC),unique(DataS$B))) #185

table(DataB$DD)
hist(DataB$DD)
table(DataA$A)
mean(DataB$DD)
var(DataB$DD)

# keep complete variables
x[, complete.cases(t(x)), drop=FALSE]
