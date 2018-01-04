best <- function (state, outcome)
{
  ## Read outcome data
  outcome_data <- read.csv("C:/santosh/dev/jhu/r/week4/data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  ## state checking..
  valid_states <- unique(outcome_data$State)
  if (!state %in% valid_states){stop("invalid state")}
  
  ## outcome checking..
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% valid_outcome){{stop("invalid outcome")}}
  
  ## Return hospital name in that state with lowest 30-day death rate..
 
  ## the given data set looks unreadable, hence subsetting and renaming column as below.. 
  
  ## suppresswarnings function is used to remove warning after coercion of data..
  x <- suppressWarnings(
                          data.frame(
                                      Hospital = as.character(outcome_data$Hospital.Name), 
                                      State = outcome_data$State,
                                      Heart_Attack_Death_Rate = as.numeric(outcome_data$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
                                      Heart_Failure_Death_Rate = as.numeric(outcome_data$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), 
                                      Pneumonia_Death_Rate = as.numeric(outcome_data$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                                    )
                        )
  
  ## subsetting based on the state parameter passed in the formal arguments..
  y <- subset(x, x$State == state)
  
  ## best hospital are the ones having minimum death rate. NAs are removed.
  if(outcome == "heart attack")
  {
    z <- subset(y, y$Heart_Attack_Death_Rate == min(y$Heart_Attack_Death_Rate, na.rm = TRUE) )
  }
  
  if(outcome == "heart failure")
  {
    z <- subset(y, y$Heart_Failure_Death_Rate == min(y$Heart_Failure_Death_Rate, na.rm = TRUE) )
  }
  
  if(outcome == "pneumonia")
  {
    z <- subset(y, y$Pneumonia_Death_Rate == min(y$Pneumonia_Death_Rate, na.rm = TRUE) )
  }
  return(as.character(z$Hospital))
}