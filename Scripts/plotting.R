
# Load liberaries
library(ggplot2)
library(plyr)
library(dplyr)
library(ggrepel)


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
plotFigure <- function(barChart, val){
  
  pie <- barChart + coord_polar("y", start=0) + theme_void() 
  pie
}


# Compute the position of labels
plotFigureOld <- function( dataFrame ) {
  
  dataFrame <- dataFrame %>% 
    arrange(desc(vote_summary)) %>%
    mutate(prop = freq / sum(dataFrame$freq) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  # Basic piechart
  ggplot(dataFrame, aes(x="", y=prop, fill=vote_summary)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    
    geom_text_repel(aes(y = ypos, label = vote_summary), color = "white", size=6) +
    scale_fill_brewer(palette="Set1")
  
}

# Compute the position of labels
plotFigure2 <- function( dataFrame ) {
  
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  
  # Compute the position of labels 
  dataFrame <- dataFrame %>% 
    arrange(desc(vote_summary)) %>%
    mutate(prop = freq / sum(dataFrame$freq) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  # Basic piechart
  ggplot(dataFrame, aes(x="", y=prop, fill=vote_summary)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    
    geom_text_repel(aes(y = ypos, label = prop), color = "black", size=3) +
    geom_label_repel( x=4, aes(y=labelPosition, label=vote_summary), size=3) +
    
    scale_fill_brewer(palette="Set1")
  
}


# create donut chart
createDonutChart <- function(data){
  
  # Compute percentages
  #data$fraction <- data$freq / sum(data$freq)
  
  #data$percent <- data$fraction * 100
  
  # Compute the cumulative percentages (top of each rectangle)
  #data$ymax <- cumsum(data$fraction)
  
  # Compute the bottom of each rectangle
  #data$ymin <- c(0, head(data$ymax, n=-1))
  
  # Compute label position
  #data$labelPosition <- (data$ymax + data$ymin) / 2
  
  # Compute a good label
  #data$label <- paste0(data$vote_summary, " : ", data$percent, "%")
  
  # Make the plot
  ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=vote_summary)) +
    geom_rect() +
    geom_label_repel( x=4, aes(y=labelPosition, label=label), size=3) +
    #geom_text(aes(y = labelPosition, label = prop), color = "black", size=3, check_overlap = T) +
    scale_fill_brewer(palette=4) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "none")
}

