#####################################
##### Do NOT delete this block #####
setwd(Sys.getenv('PROJECT_HOME'))
####################################

####################################
## write code to read input csv into data frame
df <- read.csv('data.csv')
####################################

## start writing your R code from here


library(ggplot2)
library(arules)
library(kernlab)

####################

#PPart B: 1

#Cleaning the data
hc <- df[!is.na(df$Condition_Hotel_H),] 
sc <- hc[!is.na(hc$Staff_Cared_H),] 
#Create dataframe of just the 10th row
test <- sc[ 10, ]
test

LinMod <- lm(formula=Likelihood_Recommend_H ~ Condition_Hotel_H, data=hc)
LinMod2 <- lm(formula=Likelihood_Recommend_H ~ Condition_Hotel_H + Staff_Cared_H, data=sc)

#Test/predict the dataframe
predict(LinMod, test, type="response")

predict(LinMod2, test, type="response")

#The prediction was close but not exact to the actual value 
#LinMod2 was the closest with a number of 9.2 while LinMod had 8.7 which also was not too far off

#Re-do test with the 1065th Element of the dataframe

#Create dataframe of just the 1065th row
test2 <- sc[ 1065, ]
test2

#Test/predict the dataframe with 1065th element
predict(LinMod, test2, type="response")


predict(LinMod2, test2, type="response")

#function that takes the model, a vector of conditions, and vector
#of staff cares and a vector of actual LTR; and returns how many of the model
#predictions were correct - in terms of  predicting a detractorfunction 

#The model is LinMod and LinMod2
#Vector of Conditions

Conditions <- sc$Likelihood_Recommend_H
Conditions
#Vector of Staff Cares

Staff_Cares <- sc$Staff_Cared_H
Staff_Cares  
#Vector of Actual LTR
Act_LTR <- sc$Likelihood_Recommend_H
Act_LTR

#Function that takes into account all these Vectors, have to use 



#Checking whether predicted LTR for the first model is detractor or not
ifelse(LTR1<7, "Detractor", "Not Detractor")

#Checking whether predicted LTR for the second model is detractor or not
ifelse(LTR2<7, "Detractor", "Not Detractor")

Vs <- c(Conditions, Staff_Cares)
Vs

predict(LTR1, Vs, type="response")

df$NPS_Type
str(df$NPS_Type)
df$NPS_Type <- as.character(df$NPS_Type)
df$NPS_Type[df$NPS_Type != "Detractor"] <- "NotDetractor"
df$NPS_Type <- as.factor(df$NPS_Type)


#Testing with random sample of 1,000

sample(df$NPS_Type, 1000, replace = FALSE, prob = NULL)

## end your R code and logic 

####################################
##### write output file ############
# add your R code to write output file
####################################


