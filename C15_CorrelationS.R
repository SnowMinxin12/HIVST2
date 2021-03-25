source("C01_ReadData.R")

#DD and ID
dd_dc_fromB <- DataB %>% select(DD,DC)
dataS.c15 <- merge(x=DataS,y=dd_dc_fromB, by.x = "B", by.y = "DC",all.x = TRUE, all.y = FALSE)
# remove the things that are not of interest in examining correlation
dataS.c15 <- DataS %>% 
  select(-c( "A",# survey date
             "B", # confirmation code
             "D", #How many kits did you successfully distribute?
             # TODO: summarize these by the index?
             "E","F","G","H","I", # sex of recipients
             "J","K","L"."M","N", #Relationship to Index 0=stable sexual partner; 1=casual sexual partner; 2=friends; 3=family; 4=other
             "O","P","Q","R","S", # partner testing
             # partner testing
             
                          
  ))