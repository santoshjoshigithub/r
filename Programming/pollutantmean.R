#The zip file containing the data can be downloaded here: specdata.zip. 
#The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring 
#data for fine particulate matter (PM) air pollution at 332 locations in the United States. 
#Each file contains data from a single monitor and the ID number for each monitor is contained 
#in the file name. For example, data for monitor 200 is contained in the file 200.csv. 
#Each file contains three variables. Date: the date of the observation in (year-month-day) 
#format, sulfate: the level of sulfate PM in the air on that date (measured in micrograms per 
#cubic meter), and nitrate: the level of nitrate PM in the air on that date (measured in 
#micrograms per cubic meter)


#part1
#Part-1
#Write a function named pollutantmean that calculates the mean of a pollutant (sulfate or nitrate) 
#across a specified list of monitors. The function pollutantmean takes three arguments: directory, 
#pollutant, and id. Given a vector monitor ID numbers, pollutantmean reads that monitors particulate 
#matter data from the directory specified in the directory argument and returns the mean of the pollutant 
#across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <- function (directory, pol, id)
{
  check<-0
  
  ##following loop process all the files based on the vector id.
  for (i in id)
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
    }
    else 
    {
      temp_data<-rbind(temp_data,read.csv(file = full_file_path, header=TRUE, sep=","))
    }
    
    check <- check+1  
    
  }
  
  if(pol == "sulfate")
  {
    result <- mean(temp_data$sulfate, na.rm = TRUE)
  }
  
  else  if(pol == "nitrate")
  {
    result <- mean(temp_data$nitrate, na.rm = TRUE)
  }
  return (result)
}
