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

ch <- df[!is.na(df$Condition_Hotel_H),] 

#Checking the structure of NPS_Type
str(ch$NPS_Type)
#Converting as character
ch$NPS_Type <- as.character(ch$NPS_Type)
#Defining Promoters and passives as Not Detractors
ch$NPS_Type[ch$NPS_Type != "Detractor"] <- "NotDetractor"
#Formatting column as factor
ch$NPS_Type <- as.factor(ch$NPS_Type)


#Creating indexes
randomIndex <- sample(1:dim(ch)[1])
summary(randomIndex)

#Setting a cut point
cutPoint2_3 <- floor(2*dim(ch)[1]/3)
cutPoint2_3

#Creating a train dataframe
trainData <- ch[randomIndex[1:cutPoint2_3],]
str(trainData)

#Creating a test dataframe
testData <- ch[randomIndex[(cutPoint2_3+1):dim(ch)[1]],]
str(testData)

svmOutput <- ksvm(NPS_Type ~ ., data=trainData, C=40, prob.model=TRUE)

svmOutput

svmPred <- predict(svmOutput, testData, type="votes")
compTable <- data.frame(testData[,59], svmPred[1,])

table(compTable)

str(svmPred)


## end your R code and logic 

####################################
##### write output file ############
# add your R code to write output file
####################################


