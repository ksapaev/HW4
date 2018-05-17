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

#Part C:1

ch <- df[!is.na(df$Condition_Hotel_H),] 
ch <- ch[!is.na(ch$Brand_PL),]
ch <- ch[!is.na(ch$GP_Tier),]
ch <- ch[!is.na(ch$Type_PL),]



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
length(trainData$NPS_Type)
#Creating a test dataframe
testData <- ch[randomIndex[(cutPoint2_3+1):dim(ch)[1]],]
str(testData)
length(testData$NPS_Type)

#Running SVM on train data
svmOut <- ksvm(NPS_Type ~ Condition_Hotel_H+Brand_PL+GP_Tier+Type_PL, data=trainData, kernel="rbfdot", kpar="automatic", C=5, prob.model=TRUE)

#Predicting SVM for test data
svmPredict <- predict(svmOut, testData, type="votes")

#Creating a dataframe for prediction
compTable <- data.frame(testData[,59], svmPredict[1,])
str(svmPredict)
#Showing predicted results in table
table(compTable)
str(svmPredict)

#473 cases Detractor but classified as not Detractor
#240 cases Detractor and classified as Detractor
#5607 cases not Detractor and classified as not Detractor
#73 cases not Detractor but classified as Detractor
#Accuracy is 1- (473+73)/6393 = 0.9146 or 91.46%


#####################
#Part C:2

#Cleaning dataframe
sc <- ch[!is.na(ch$Staff_Cared_H),]


#Creating indexes
randomIndex2 <- sample(1:dim(sc)[1])
summary(randomIndex2)

#Setting a cut point
cutPoint2_3 <- floor(2*dim(sc)[1]/3)
cutPoint2_3

#Creating a train dataframe
trainData2 <- sc[randomIndex2[1:cutPoint2_3],]
str(trainData2)
length(trainData2$NPS_Type)
#Creating a test dataframe
testData2 <- sc[randomIndex2[(cutPoint2_3+1):dim(sc)[1]],]
str(testData2)
length(testData2$NPS_Type)

#Running SVM on train data
svmOut2 <- ksvm(NPS_Type ~ Condition_Hotel_H+Staff_Cared_H+Brand_PL+GP_Tier+Type_PL, data=trainData2, kernel="rbfdot", kpar="automatic", C=5, prob.model=TRUE)

#Predicting SVM for test data
svmPredict2 <- predict(svmOut2, testData2, type="votes")

#Creating a dataframe for prediction
compTable2 <- data.frame(testData2[,59], svmPredict2[1,])

#Showing predicted results in table
table(compTable2)

#238 cases Detractor but classified as not Detractor
#176 cases Detractor and classified as Detractor
#3569 cases not Detractor and classified as not Detractor
#50 cases not Detractor but classified as Detractor
#Accuracy is 1- (238+50)/4033 = 0.9286 or 92.86%


#####################
#Part C:3

#Checking 1650th survey for the first model
compTable_1650 <- data.frame(testData[1650,59], svmPredict[1,1650])
table(compTable_1650)

#Checking 1650th survey for the second model
compTable2_1650 <- data.frame(testData2[1650,59], svmPredict2[1,1650])
table(compTable2_1650)


#Predicted result is positive for both models. 1650th survey was Not Detractor.
#Models find it as NotDetractor.


###########################################################
#Part D:





###########################################################


## end your R code and logic 

####################################
##### write output file ############
# add your R code to write output file
####################################


