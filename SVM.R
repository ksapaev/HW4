#####################################
##### Do NOT delete this block #####
setwd(Sys.getenv('PROJECT_HOME'))
####################################

####################################
## write code to read input csv into data frame
df <- read.csv('data.csv')
####################################

## start writing your R code from here

#Part C:1

#Creating indexes
randomIndex <- sample(1:dim(df)[1])
summary(randomIndex)

#Setting a cut point
cutPoint2_3 <- floor(2*dim(df)[1]/3)
cutPoint2_3

#Creating a train dataframe
trainData <- df[randomIndex[1:cutPoint2_3],]
str(trainData)

#Creating a test dataframe
testData <- df[randomIndex[(cutPoint2_3+1):dim(spam)[1]],]
str(testData)



## end your R code and logic 

####################################
##### write output file ############
# add your R code to write output file
####################################


