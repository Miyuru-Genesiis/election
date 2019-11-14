
# set working directory
setwd("E:/Studies/R/election-survey/Scripts")

#library(ggplot2)
#library(plyr)

source("plotting.R")


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

# process issue summary question
y <- countColumnFreq(df, 6)

# create dataframe
dataFrame <- createDataFrame(y$vote_summary, y$freq)

# plotting
setPlotName('../Figures/issue_summery.png')
createDonutChart(dataFrame)
dev.off()



# process national security question
y <- countColumnFreq(df, 15)
dataFrame2 <- createDataFrame(y$national_security, y$freq)

setPlotName('../Figures/national_security.png')
createDonutChart(dataFrame2)
dev.off()



# process national development question
y <- countColumnFreq(df, 16)
dataFrame3 <- createDataFrame(y$national_development, y$freq)
setPlotName('../Figures/national_development_d.png')
createDonutChart(dataFrame3)

dev.off()



# process unemployemnt question
y <- countColumnFreq(df, 17)
dataFrame4 <- createDataFrame(y$unemployemnt, y$freq)

setPlotName('../Figures/unemployemnt.png')
createDonutChart(dataFrame4)

dev.off()



# process cost_of_living question
y <- countColumnFreq(df, 18)
dataFrame5 <- createDataFrame(y$cost_of_living, y$freq)

setPlotName('../Figures/cost_of_living.png')
createDonutChart(dataFrame5)

dev.off()



# process religious_ethnic_issues question
y <- countColumnFreq(df, 19)
dataFrame6 <- createDataFrame(y$religious_ethnic_issues, y$freq)

setPlotName('../Figures/religious_ethnic_issues.png')
createDonutChart(dataFrame6)

dev.off()

