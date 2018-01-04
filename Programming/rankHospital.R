rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("C:/santosh/dev/jhu/r/week4/data/outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  ## state checking..
  valid_states <- unique(outcome_data$State)
  if (!state %in% valid_states){stop("invalid state")}
  ## outcome checking..
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% valid_outcome){{stop("invalid outcome")}}
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  x <- suppressWarnings(
    data.frame(
      Hospital = as.character(outcome_data$Hospital.Name), 
      State = outcome_data$State,
      Heart_Attack_Death_Rate = as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
      Heart_Failure_Death_Rate = as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), 
      Pneumonia_Death_Rate = as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    )
  )
  ## subsetting based on the state parameter passed in the formal arguments..
  y <- subset(x, x$State == state)
  ## order y based on states..
  s <- y[order(y$Hospital, decreasing = F),]
  if(outcome == "heart attack")
  {
    z <- data.frame(s[complete.cases(s$Heart_Attack_Death_Rate),], rnk =  rank(s$Heart_Attack_Death_Rate, na.last = NA,  ties.method = "first"))
    r <- data.frame(Hospital = z$Hospital, Rate = z$Heart_Attack_Death_Rate, Rank = z$rnk)
  }
  if(outcome == "heart failure")
  {
    z <- data.frame(s[complete.cases(s$Heart_Failure_Death_Rate),], rnk =  rank(s$Heart_Failure_Death_Rate, na.last = NA,  ties.method = "first"))
    r <- data.frame(Hospital = z$Hospital, Rate = z$Heart_Failure_Death_Rate, Rank = z$rnk)
  }
  if(outcome == "pneumonia")
  {
    z <- data.frame(s[complete.cases(s$Pneumonia_Death_Rate),], rnk =  rank(s$Pneumonia_Death_Rate, na.last = NA,  ties.method = "first"))
    r <- data.frame(Hospital = z$Hospital, Rate = z$Pneumonia_Death_Rate, Rank = z$rnk)
  }
  if(num != "best" & num != "worst" & num > max(r$Rank))
  {
    return(NA)
  }
  if(num == "best")
  {
    return(as.character(subset(r$Hospital, r$Rank == min(r$Rank))))
  }
  if(num == "worst")
  {
    return(as.character(subset(r$Hospital, r$Rank == max(r$Rank))))
  }
  if(num != "best" & num != "worst")
  {
    return(as.character(subset(r$Hospital, r$Rank == num)))
  }
}
