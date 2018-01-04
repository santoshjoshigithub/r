rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("C:/santosh/dev/jhu/r/week4/data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  ## state checking..
  ##valid_states <- unique(outcome_data$State)
  ##if (!state %in% valid_states){stop("invalid state")}
  
  ## outcome checking..
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% valid_outcome){{stop("invalid outcome")}}
  
  ## For each state, find the hospital of the given rank
  x <- suppressWarnings(
    data.frame(
      Hospital = as.character(outcome_data$Hospital.Name), 
      State = outcome_data$State,
      Heart_Attack_Death_Rate = as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
      Heart_Failure_Death_Rate = as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), 
      Pneumonia_Death_Rate = as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    )
  )
  
  count <- 0
  for (i in unique(x$State))
  {
    #print(s)
    y <- subset(x, x$State == i)
    s <- y[order(y$Hospital, decreasing = F),]
    
    if(outcome == "heart attack")
    {
      z <- data.frame(s[complete.cases(s$Heart_Attack_Death_Rate),], rnk =  rank(s$Heart_Attack_Death_Rate, na.last = NA,  ties.method = "first"))
      r <- data.frame(Hospital = z$Hospital, State = z$State, Rate = z$Heart_Attack_Death_Rate, Rank = z$rnk)
      if(count==0)
      {
        temp <- subset (r, r$Rank == num)
        final <- data.frame(hospital = temp$Hospital, state = temp$State)
      }
      else
      {
        temp <- subset (r, r$Rank == num)
        final <- rbind(final,data.frame(hospital = temp$Hospital, state = temp$State))
      }
    }
    count <- count + 1
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return (final)
}
