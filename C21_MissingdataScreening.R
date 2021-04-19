### C21_MissingdataScreening
### author: Minxin Lu
### date: 2021-4-16
### (1) delete all variables columns with missing > 50% or skip>50%
### (2) delete variable with variation=0
### input: jointdata.csv from C13
### output: jointdata1.csv

jointdataB <- read_csv("../data/summarizedB.csv")
jointdataB <- jointdataB[,colnames(jointdataB)!="X1"]
jointdataBS <- read_csv("../data/jointdataBS.csv")
jointdataBS <- jointdataBS[,colnames(jointdataBS)!="X1"]
jointdataBSA <- read_csv("../data/jointdataBSA.csv")
jointdataBSA <- jointdataBSA[,colnames(jointdataBSA)!="X1"]


# get the variable names that are missing or SKIP >10% overall
var_miss = function(data, geq_percent){
  missingness_sorted = as.data.frame(sort(colMeans(is.na(data)),decreasing = TRUE))
  missingness_sorted$var_names = rownames(missingness_sorted)
  colnames(missingness_sorted) = c("miss_percent","var_name")
  var_missing_greater = missingness_sorted[missingness_sorted$miss_percent > geq_percent,]$var_name
  return(var_missing_greater)
}


# all variables columns with SKIP > 10% 
var_SKIP = function(data, geq_percent){
  missingness_sorted = as.data.frame(round(sort(colMeans(data=="SKIP"),decreasing = TRUE),4))
  missingness_sorted$var_names = rownames(missingness_sorted)
  colnames(missingness_sorted) = c("miss_percent","var_name")
  # variables missing >% 
  var_skip_greater = missingness_sorted[missingness_sorted$miss_percent > geq_percent,]$var_name
  return(var_skip_greater)
}

# remove variables with zero variance
var_Zerovar = function(data){
  data <- data[ - as.numeric(which(apply(data, 2, var) == 0))]
  return(data)
}

reduced_data = function(data,miss_geq_percent,skip_geq_percent){
  data1 <- var_Zerovar(data)
  var_remove = union(var_miss(data1,miss_geq_percent),var_SKIP(data1,skip_geq_percent))
  data2 <-data1[,!colnames(data1) %in% var_remove]
  return(data2)
}
write.csv(reduced_data(data=jointdataB,miss_geq_percent=0.1,skip_geq_percent=0.1),
          file="../data/jointdataB_c21.csv")
write.csv(reduced_data(data=jointdataBS,miss_geq_percent=0.1,skip_geq_percent=0.1),
          file="../data/jointdataBS_c21.csv")
write.csv(reduced_data(data=jointdataBSA,miss_geq_percent=0.1,skip_geq_percent=0.1),
          file="../data/jointdataBSA_c21.csv")
