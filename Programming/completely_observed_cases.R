## Part-2
## Write a function that reads a directory full of files and reports the number of completely observed cases 
## in each data file. The function should return a data frame where the first column is the name of the file 
## and the second column is the number of complete cases.

countCompCases <- function (directory, id)
{
  check<-0
  for(i in id)
  {
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
      temp_complete_cases <- temp_data[complete.cases(temp_data),]
      df <- c(id = i, count = nrow(temp_complete_cases))
    }
    
    else 
    {
      temp_data <- read.csv(file = full_file_path, header=TRUE, sep=",")
      temp_complete_cases <- temp_data[complete.cases(temp_data),]
      df <- rbind(df, c(id = i, count = nrow(temp_complete_cases)))
    }
    
    check <- check+1  
  }
  
  return(df)
}