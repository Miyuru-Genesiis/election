
# set working directory
setwd("E:/Studies/R/election-survey/Scripts")

library(datasets)
library(ggplot2)
library(plyr)


if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio) 

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

# get the frequency of categorical data
# get frequncies for each category
# summarises for a given column value
countColumnFreq <- function (df, columneNumber){
  y <- count(df[columneNumber])
  return(y)
}

# creates a dataFrame
createDataFrame <- function(labels, values){
  myFrame <- data.frame(
    vote_summary = labels,
    freq = values
  )
  return(myFrame)
}

setPlotName <- function(plotName){
  png(file = plotName)
}

# plot the figure - pie chart
plotFigure <- function(barChart){

  pie <- barChart + coord_polar("y", start=0)
  pie
}


#### process initial dataset
#load the dataset
df <- readFile("data.csv")

# process issue summary question
y <- countColumnFreq(df, 6)
y
dataFrame <- createDataFrame(y$vote_summary, y$freq)
dataFrame

setPlotName('../Figures/issue_summery.png')

# plot of vote summary question
bp<- ggplot(myFrame, aes(x="", y=freq, fill=vote_summary))+
  geom_bar(width = 1, stat = "identity")


plotFigure(bp)
dev.off()


# process national security question
y <- countColumnFreq(df, 15)
y
dataFrame2 <- createDataFrame(y$national_security, y$freq)

dataFrame2
setPlotName('../Figures/national_security.png')

# plot of national security data
bp2<- ggplot(dataFrame2, aes(x="", y=freq, fill=vote_summary))+
  geom_bar(width = 1, stat = "identity")

bp2
plotFigure(bp2)

dev.off()

# process national development question
y <- countColumnFreq(df, 16)
y
dataFrame3 <- createDataFrame(y$national_development, y$freq)

dataFrame3
setPlotName('../Figures/national_development.png')

# plot of national security data
bp3<- ggplot(dataFrame3, aes(x="", y=freq, fill=vote_summary))+
  geom_bar(width = 1, stat = "identity")

bp3
plotFigure(bp3)

dev.off()

# process unemployemnt question
y <- countColumnFreq(df, 17)
y
dataFrame4 <- createDataFrame(y$unemployemnt, y$freq)

dataFrame4
setPlotName('../Figures/unemployemnt.png')

# plot of unemployemnt data
bp4<- ggplot(dataFrame4, aes(x="", y=freq, fill=vote_summary))+
  geom_bar(width = 1, stat = "identity")


plotFigure(bp4)

dev.off()


# process cost_of_living question
y <- countColumnFreq(df, 18)
y
dataFrame5 <- createDataFrame(y$cost_of_living, y$freq)

dataFrame5
setPlotName('../Figures/cost_of_living.png')

# plot of cost_of_living data
bp5 <- ggplot(dataFrame5, aes(x="", y=freq, fill=vote_summary))+
  geom_bar(width = 1, stat = "identity")


plotFigure(bp5)

dev.off()


# process religious_ethnic_issues question
y <- countColumnFreq(df, 19)
y
dataFrame6 <- createDataFrame(y$religious_ethnic_issues, y$freq)

dataFrame6
setPlotName('../Figures/religious_ethnic_issues.png')

# plot of religious_ethnic_issues data
bp6 <- ggplot(dataFrame5, aes(x="", y=freq, fill=vote_summary))+
  geom_bar(width = 1, stat = "identity")


plotFigure(bp6)

dev.off()

