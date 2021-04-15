### C21_MissingdataScreening
### author: Minxin Lu
### date: 2021-4-14
### (1) delete all variables columns with missing > 50% or skip>50%
### (2) delete variable with variation=0
### input: jointdata.csv from C13
### output: jointdata1.csv

jointdata <- read_csv("../data/jointdata.csv")
jointdata <- jointdata[,colnames(jointdata)!="X1"]


# get the variable names that are missing or SKIP >10% overall
var_miss = function(data, geq_percent){
  
  missingness_sorted = as.data.frame(sort(colMeans(is.na(data)),decreasing = TRUE))
  missingness_sorted$var_names = rownames(missingness_sorted)
  colnames(missingness_sorted) = c("miss_percent","var_name")
  var_missing_greater = missingness_sorted[missingness_sorted$miss_percent > geq_percent,]$var_name
  var_missing_greater
}


# all variables columns with SKIP > 10% 
var_SKIP = function(data, geq_percent){
  missingness_sorted = as.data.frame(round(sort(colMeans(jointdata=="SKIP"),decreasing = TRUE),4))
  missingness_sorted$var_names = rownames(missingness_sorted)
  colnames(missingness_sorted) = c("miss_percent","var_name")
  # variables missing >% 
  var_missing_greater = missingness_sorted[missingness_sorted$miss_percent > geq_percent,]$var_name
  var_missing_greater
}

# remove variables with zero variance
jointdata.c21 <- jointdata[ - as.numeric(which(apply(jointdata, 2, var) == 0))]

var_remove = union(var_miss(jointdata.c21,0.1),var_SKIP(jointdata.c21,0.1))
jointdata.c21 <-jointdata.c21[,!colnames(jointdata.c21) %in% var_remove]

write.csv(jointdata.c21,file="../data/jointdata1.csv")
