#####################################
##### Do NOT delete this block #####
setwd(Sys.getenv('PROJECT_HOME'))
####################################

####################################
## write code to read input csv into data frame
df <- read.csv('raw_data.csv')
####################################

## start writing your R code from here
df <- df[!is.na(df$Likelihood_Recommend_H),] 
## end your R code and logic 

####################################
##### write output file ############
write.csv(df, file = 'data.csv')
####################################

