
frequencyCalculation <- function( dataFrame ){
  
  # Compute the position of labels 
  dataFrame <- dataFrame %>% 
    arrange(desc(freq)) %>%
    mutate(prop = freq / sum(dataFrame$freq) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  return (dataFrame)
  
}

# calcuate frequency percentage and other plotting related data
dataFrameProcessing <- function(data, data2){
  
  # Compute percentages
  data$fraction <- data$freq / sum(data$freq)
  
  data$percent <- data$fraction * 100
  
  # Compute the cumulative percentages (top of each rectangle)
  data$ymax <- cumsum(data$fraction)
  
  # Compute the bottom of each rectangle
  data$ymin <- c(0, head(data$ymax, n=-1))
  
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  
  # Compute a good label
  data$label <- paste0(data$vote_summary, " : ", data$percent, "%")
  
  return(data)
}


# calcuate weighted values for each issue for 
# this takes the sum of individuals' values of each issue * priority weight gained for the issue 
weightedIssueFrequency <- function( dList, data1, data2, data3, data4, data5, data6){
  
  i <- 2
  n <- length(participants)
  s <- vector("numeric", 6)
  t <- 0
  
  for ( i in 2:n ){

    x <- data2[i, 4]*data1[5,3] + data3[i, 4]*data1[4,3] + data4[i,4]*data1[2,3] + data5[i,4]*data1[1,3] + data6[i,4]*data1[3,3] + data6[i,4]*data1[7,3]
    s[i] <- x
    t <- t+x
    print(x)
    
  }
  print(t)
  s[1] <- 100 - t
  return(s)
  
}


