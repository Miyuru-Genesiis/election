
# set working directory
setwd("E:/Studies/R/election-survey/Scripts")

#library(ggplot2)
#library(plyr)

source("plotting.R")
source("process_data.R")


if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio) 

if (!require("ggrepel")) install.packages("ggrepel")


# read file
readFile <- function(fileName){
  df <- read.table(fileName, 
                   header = TRUE,
                   col.names=c("timeStamp", "is_voting", "area_voting", "individual_voting", "vote_reason", "vote_summary", "age", "sex", "religion", 
                               "education", "employment", "ethnicity", "province", "district", "national_security", "national_development", 
                               "unemployemnt", "cost_of_living", "religious_ethnic_issues"),
                   sep = ",")
  
  return(df)
  
}



#### process initial dataset
# load the dataset
df <- readFile("data.csv")


# users voting choice
# process issue summary question
y <- countColumnFreq(df, 4)
y

# create dataframe
dataFrame0 <- createDataFrame(y$individual_voting, y$freq)

dataFrame0 <- dataFrameProcessing(dataFrame0)
dataFrame0
# plotting
setPlotName('../Figures/vote_summery.jpg')
createDonutChart(dataFrame0)
dev.off()

# process issue summary question
y <- countColumnFreq(df, 6)
y

# create dataframe
dataFrame <- createDataFrame(y$vote_summary, y$freq)

dataFrame <- dataFrameProcessing(dataFrame)
dataFrame
# plotting
setPlotName('../Figures/issue_summery.jpg')
createDonutChart(dataFrame)
dev.off()


# process national security question
y <- countColumnFreq(df, 15)
dataFrame2 <- createDataFrame(y$national_security, y$freq)
dataFrame2 <- dataFrameProcessing(dataFrame2)
setPlotName('../Figures/national_security.jpg')
createDonutChart(dataFrame2)
dev.off()


participants <- dataFrame2[,1]


# process national development question
y <- countColumnFreq(df, 16)
dataFrame3 <- createDataFrame(y$national_development, y$freq)
dataFrame3 <- dataFrameProcessing(dataFrame3)
setPlotName('../Figures/national_development_d.jpg')
createDonutChart(dataFrame3)

dev.off()



# process unemployemnt question
y <- countColumnFreq(df, 17)
dataFrame4 <- createDataFrame(y$unemployemnt, y$freq)
dataFrame4 <- dataFrameProcessing(dataFrame4)
setPlotName('../Figures/unemployemnt.jpg')
createDonutChart(dataFrame4)

dev.off()



# process cost_of_living question
y <- countColumnFreq(df, 18)
dataFrame5 <- createDataFrame(y$cost_of_living, y$freq)
dataFrame5 <- dataFrameProcessing(dataFrame5)
setPlotName('../Figures/cost_of_living.jpg')
createDonutChart(dataFrame5)

dev.off()



# process religious_ethnic_issues question
y <- countColumnFreq(df, 19)
dataFrame6 <- createDataFrame(y$religious_ethnic_issues, y$freq)
dataFrame6 <- dataFrameProcessing(dataFrame6)
dataFrame6
setPlotName('../Figures/religious_ethnic_issues.jpg')
createDonutChart(dataFrame6)

dev.off()

# calculate final weighted values for each participant
wFreq <- weightedIssueFrequency(participants, dataFrame, dataFrame2, dataFrame3, dataFrame4, dataFrame5, dataFrame6)

finalData <- data.frame(
  vote_summary=participants,
  freq=wFreq
)


finalData <- dataFrameProcessing(finalData)
finalData
setPlotName("../Figures/finalResults.jpg")
createDonutChart(finalData)

dev.off()


