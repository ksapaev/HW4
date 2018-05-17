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






## end your R code and logic 

####################################
##### write output file ############
# add your R code to write scatter_LTR.png
####################################



