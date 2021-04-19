### C31_Logistic.R
### author: 
### date: 2021-4-12
### logistic for aim 1 outcome variable name ="B.test_kits_actual" 
### (for new alters see "A.prior_hiv_test", not sure what's the exact name)
### (1) check model assumption
### (2) 10 folds cross validation with fitted model with model selection: LASSO, best subset selection, AIC
### input: (1) the function F31_Logistic.R, (2) dataset
### output: (1) model (2) important variables (3) coefficients for all variables