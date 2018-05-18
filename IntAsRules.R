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

#Sorting rules by support and confidence    
top.support <- sort(rules, decreasing = TRUE, na.last = NA, by = "support")
top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")

#Show the top 10 rules
inspect(head(top.support, 10))




plot(ruleset)



#Generate rules that predict if someone will be a detractor


#Add LTR Column to detect detractor
NewDF$Likelihood_Recommend_H <- df$Likelihood_Recommend_H

#Detractors are numbers of 6 or less
Detractors <- (NewDF$Likelihood_Recommend_H < 7)
Detractors

NewDF$Detractors <- (Detractors)
NewDF$Detractors

#Detractors <-Detractors)
test2 <- factor(NewDF[ ,6])

NewDF$Likelihood_Recommend_H <- test2

#Generate rules
Det_ruleset <- apriori(NewDF, parameter =list(support=0.01,confidence=0.5))
summary(Det_ruleset)

#Use inspect to look at generated rules

top10De <- sort(Det_ruleset, 10)
top10De

#ifelse(NewDF=="Detractor", NewDF=="Detractor", NewDF=="NotDetractor" )

#Top 10 rules based on confidence of the rule
top.confidence <- sort(Det_ruleset, decreasing = TRUE, na.last = NA, by = "confidence")
top.confidence
inspect(head(top.confidence, 10))
        
#Plot top 10 confidence rules
png(filename="Plot.png")
plot(top.confidence)
dev.off()



## end your R code and logic 

####################################
##### write output file ############
# add your R code to write Plot.png
####################################



