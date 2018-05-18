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

#Part A: 1

#Creating a new dataframe by omitting NAs from 'hotel condition' column
hc <- df[!is.na(df$Condition_Hotel_H),] 

#Having both x and y of the scatterplot clean, we can proceed with plotting it by adding additional layers.
myScatter <- ggplot(hc, aes(x=Condition_Hotel_H, y=Likelihood_Recommend_H))
myScatter <- myScatter + geom_point() + geom_jitter(alpha=0.3, position=position_jitter(width=0.2, height=0.2))
myScatter <- myScatter + scale_x_continuous( breaks = 1:10)+ scale_y_continuous( breaks = 1:10)
myScatter <- myScatter + xlab("Hotel Condition") + ylab("Likelihood to Recommend") + theme_classic()

#Creating the png file of the scatterplot.
png(filename="scatter_LTR.png")
myScatter
dev.off()


##################
#Part A: 2

#Creating a new dataframe by omitting NAs from 'staff cared' column
sc <- hc[!is.na(hc$Staff_Cared_H),] 

# Running linear regression model with 1 independent variable
LinMod <- lm(formula=Likelihood_Recommend_H ~ Condition_Hotel_H, data=sc)
summary(LinMod)


#plot(hc$Condition_Hotel_H, hc$Likelihood_Recommend_H, xlab = "Hotel Condition", ylab = "Likelihood to Recommend")
#abline(LinMod)


##################
#Part A: 3



# Running linear regression model with 2 independent variables
LinMod2 <- lm(formula=Likelihood_Recommend_H ~ Condition_Hotel_H + Staff_Cared_H, data=sc)
summary(LinMod2)

#plot(sc$Condition_Hotel_H, sc$Likelihood_Recommend_H, xlab = "Hotel Condition", ylab = "Likelihood to Recommend")
#abline(LinMod2)


##################
#Part A: 4

# R2 for the first model is 0.4501
# R2 for the second model is 0.5743
#Linear models look like:
# y1 = 0.783595 + 0.880256 * Hotel Condition 
# y2 = -1.3016 + 0.618915 * Hotel Condition + 0.49269 * Staff Cared

#In terms of variations that models explain I prefer second model.
#The second model explains more variations of Likelihood to recommend.
#Coefficients of the second model are statistically significant.


##################
#Part A: 5

#Creating a dataframe for Hotel Condition =4
x <- data.frame(Condition_Hotel_H=c(4))
#Predicting LTR
LTR1 <- predict(LinMod, x)
LTR1
#Creating a dataframe for Hotel Condition =4 and Staff Cared=4
x2 <- data.frame(Condition_Hotel_H=c(4), Staff_Cared_H=c(4))
#Predicting LTR
LTR2 <- predict(LinMod2, x2)
LTR2

#Checking whether predicted LTR for the first model is detractor or not
ifelse(LTR1<7, "Detractor", "Not Detractor")

#Checking whether predicted LTR for the second model is detractor or not
ifelse(LTR2<7, "Detractor", "Not Detractor")




####################################################################

#Part B: 1

#Create dataframe of just the 10th row
test <- sc[10,]


#Test/predict the dataframe with 10th element. Result is Not Detractor for both.
LTR1test <- predict(LinMod, test, type="response")
ifelse(LTR1test<7, "Detractor", "Not Detractor")

LTR2test <- predict(LinMod2, test, type="response")
ifelse(LTR2test<7, "Detractor", "Not Detractor")

test$Likelihood_Recommend_H
LTR1test
LTR2test
#The prediction was close but not exact to the actual value of LTR=10 
#LinMod2 was the closest with a number of 9.2 while LinMod had 8.71


##################

#Part B: 2
#Re-do test with the 1065th Element of the dataframe

#Create dataframe of just the 1065th row
test2 <- sc[1065, ]

#Test/predict the dataframe with 1065th element. Result is Detractor for both.
LTR1test <- predict(LinMod, test2, type="response")
ifelse(LTR1test<7, "Detractor", "Not Detractor")

LTR2test <- predict(LinMod2, test2, type="response")
ifelse(LTR2test<7, "Detractor", "Not Detractor")

test2$Condition_Hotel_H
LTR1test
LTR2test

#The prediction was incorrect. The actual value of LTR=7 (Passive) where prediction resulted Detractor
#LinMod was the closest with a number of 6.95 while LinMod2 had 5.49


#############

#Part B: 3

#function that takes the model, a vector of conditions, and vector
#of staff cares and a vector of actual LTR; and returns how many of the model
#predictions were correct - in terms of  predicting a detractorfunction 

#The model is LinMod2
Predictions <- function(P) {
  Act_LTR <- P$Likelihood_Recommend_H
  Prediction <- predict(LinMod2, P, type="response")
  ActDet <- sum(Act_LTR<7)
  PredDet <- sum(Prediction<7 & Act_LTR<7)
  PredDet/ActDet
}
#Calculating percentage of TRUEly predicted Detractors which is 61.85%
Predictions(sc)*100


#############

#Part B: 4

#Testing with random sample of 1,000
#Creating 1000 sample of indexes
randomIndex <- sample(1:dim(sc)[1], 1000, replace=FALSE, prob=NULL)

#Calculating percentage of TRUEly predicted Detractors which is 63.83% for the sample
Predictions(sc[randomIndex,])*100


## end your R code and logic 

####################################
##### write output file ############
# add your R code to write scatter_LTR.png
####################################



