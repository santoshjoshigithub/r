#Write a function that takes a directory of data files and a threshold for complete cases and calculates 
#the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases 
#(on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors 
#that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric 
#vector of length 0.

correlation <- function (directory, threshold)
{
  
  check<-0
  corVector = NULL
  
  ##following loop process all the files based on the vector id.
  for (i in 1:332)
  {
    #set the directory where files are located.
    file_directory<-file.path(getwd(), directory)
    
    if(i<10)
    {
      i<-paste("00", i, sep = "")
      full_file_path <- paste(file_directory,"/",i, ".csv", sep = "")
    }
    
    if(i>9 && i<100)
    {
      i<-paste("0", i, sep = "")
      full_file_path <- paste(file_directory,"/",i, ".csv", sep = "")
    }
    
    if (check == 0)
    {
      temp_data <- read.csv(file = full_file_path, header=TRUE, sep=",")
      df_complete_cases <- temp_data[complete.cases(temp_data),]
    }
    
    else 
    {
      temp_data <- read.csv(file = full_file_path, header=TRUE, sep=",")
      df_complete_cases <- temp_data[complete.cases(temp_data),]
    }
    
    if(nrow(df_complete_cases) > threshold)
    {
      corVector = c(corVector, cor(df_complete_cases[,2], df_complete_cases[,3]))
    }
  }
  check <- check+1  
  
  return(corVector)
}