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
library(arulesViz)
library(arules)

############

#Part E:1

#Cleaning the dataframe
df <- df[!is.na(df$LENGTH_OF_STAY_C),] 
df <- df[!is.na(df$Guest_Country_H),] 
df <- df[!is.na(df$Gender_H),] 
df <- df[!is.na(df$Age_Range_H),]
df <- df[!is.na(df$GP_Tier),]
df <- df[!is.na(df$NPS_Type),]
#Generate Interesting Association Rule using below 
#Create Dataframe with appropriate columns
NewDF <- data.frame(LengthOfStay=df$LENGTH_OF_STAY_C, GuestCountry=df$Guest_Country_H, Gender=df$Gender_H, AgeRange=df$Age_Range_H, GPTier=df$GP_Tier)
#NewDF
#patterns <- random.patterns(df$LENGTH_OF_STAY_C = 1000);
str(NewDF)
#Convert column 'Length of stay' into a factor
NewDF$LengthOfStay <- factor(NewDF[ ,1])



#View Rules
rules <- apriori(NewDF, parameter = list(support=0.01,confidence=0.5))
summary(rules)

#Sorting rules by support and confidence    
top.support <- sort(rules, decreasing = TRUE, na.last = NA, by = "support")
top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")

#Show the top 10 rules
inspect(head(top.support, 10))
inspect(head(top.confidence, 10))

png(filename="Top10Support.png")
plot(head(top.support, 10))
dev.off()

png(filename="Top10Confidence.png")
plot(head(top.confidence, 10))
dev.off()

########################
#Part E:2
#Generate rules that predict if someone will be a detractor

#Add LTR Column to detect detractor
#Converting as character
df$NPS_Type <- as.character(df$NPS_Type)
#Defining Promoters and passives as Not Detractors
df$NPS_Type[df$NPS_Type != "Detractor"] <- "NotDetractor"
#Formatting column as factor
df$NPS_Type <- as.factor(df$NPS_Type)

#Adding 'Detractors' column to the NewDF dataframe
NewDF$Detractors <- df$NPS_Type


#Generating rules
Det_rules <- apriori(NewDF, parameter =list(support=0.01,confidence=0.5))
summary(Det_rules)

#Sorting rules by support and confidence 
top.support.det <- sort(Det_rules, decreasing = TRUE, na.last = NA, by = "support")
top.confidence.det <- sort(Det_rules, decreasing = TRUE, na.last = NA, by = "confidence")


#Show the top 10 rules
inspect(head(top.support.det, 10))
inspect(head(top.confidence.det, 10))

png(filename="Top10SupportDet.png")
plot(head(top.support.det, 10))
dev.off()

png(filename="Top10ConfidenceDet.png")
plot(head(top.confidence.det, 10))
dev.off()



## end your R code and logic 

####################################
##### write output file ############
# add your R code to write Top10ConfidenceDet.png
####################################





