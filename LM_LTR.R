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



## end your R code and logic 

####################################
##### write output file ############
# add your R code to write scatter_LTR.png
####################################



