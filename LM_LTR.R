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

# Running linear regression model with 1 independent variable
LinMod <- lm(formula=Likelihood_Recommend_H ~ Condition_Hotel_H, data=hc)
summary(LinMod)


#plot(hc$Condition_Hotel_H, hc$Likelihood_Recommend_H, xlab = "Hotel Condition", ylab = "Likelihood to Recommend")
#abline(LinMod)


##################
#Part A: 3

#Creating a new dataframe by omitting NAs from 'staff cared' column
sc <- hc[!is.na(hc$Staff_Cared_H),] 

# Running linear regression model with 2 independent variables
LinMod2 <- lm(formula=Likelihood_Recommend_H ~ Condition_Hotel_H + Staff_Cared_H, data=sc)
summary(LinMod2)

#plot(sc$Condition_Hotel_H, sc$Likelihood_Recommend_H, xlab = "Hotel Condition", ylab = "Likelihood to Recommend")
#abline(LinMod2)


##################
#Part A: 4

# R2 for the first model is 0.5009
# R2 for the second model is 0.5743
#Linear models look like:
# y1 = 0.85196 + 0.87941 * Hotel Condition 
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

#PPart B: 1

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
# add your R code to write scatter_LTR.png
####################################



