# get outcomevariable for aim 3
B = read_csv("../data/jointdataB_c22_0422.csv")
B = B[,colnames(B)!="X1"]
BSA = read_csv("../data/jointdataBSA_c22_0422.csv")
BSA = BSA[,colnames(BSA)!="X1"]
zeroNewAlter_ID = setdiff(B$confirm_code, BSA$B.confirm_code) 
BSA$newAltersCount = BSA$A.prior_hiv_test_1
BSA$A.prior_hiv_test_1
