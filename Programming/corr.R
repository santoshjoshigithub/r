corr <- function (directory, threshold = 0){
  
  #Initialize the counter..
  count <- 0
  
  corVector = NULL
  
  #store the original working directory path...
  original_working_directory <- getwd()    
  
  #Set the new working direcory where specdata folder is stored...
  setwd("C:/santosh/dev/jhu/r/week2")  
  
  #Get the directory of files..  
  file_directory<-file.path(getwd(), directory)
  
  
  #load content of each file into a single data frame for further processing...
  for(i in  1:332)
  {
    if(i<10)
    {
      full_file_path <- paste(file_directory,"/",paste("00", i, sep = ""), ".csv", sep = "")
    }
    
    if(i>9 && i<100)
    {
      full_file_path <- paste(file_directory,"/",paste("0", i, sep = ""), ".csv", sep = "")
    }
    
    if(i>=100)
    {
      full_file_path <- paste(file_directory,"/",i, ".csv", sep = "")
    }
    
    if (count == 0)
    {
      temp_data <- read.csv(file = full_file_path, header=TRUE, sep=",")
      df_complete_cases <- temp_data[complete.cases(temp_data),]
    }
    
    else 
    {
      temp_data <- read.csv(file = full_file_path, header=TRUE, sep=",")
      df_complete_cases <- rbind(df_complete_cases,temp_data[complete.cases(temp_data),])
    }
    
    if(nrow(df_complete_cases) > threshold)
    {
      corVector = rbind(corVector, c(cor(df_complete_cases[,2], df_complete_cases[,3])))
    }
  }
  
  count <- count+1  
  return(as.vector(corVector))
}
