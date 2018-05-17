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

str(df$NPS_Type)
df$NPS_Type <- as.character(df$NPS_Type)
df$NPS_Type[df$NPS_Type != "Detractor"] <- "NotDetractor"
df$NPS_Type <- as.factor(df$NPS_Type)

ifelse(df$NPS_Type=="Detractor", df$NPS_Type=="Detractor", df$NPS_Type=="NotDetractor" )

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


