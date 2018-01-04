plot1 <- function()
{
  library(dplyr)
  
  ##read the UCI data set for Electric power consumption..
  ds_power <- filter(
                 read.csv("household_power_consumption.txt", sep = ';', header = TRUE), 
                 as.Date(Date,"%d/%m/%Y") == "2007-02-01" | as.Date(Date,"%d/%m/%Y") == "2007-02-02"
                 )
  
  #create histogram plot using base plotting system
  hist(
         as.numeric(ds_power$Global_active_power)
        ,xlab = "Global Active Power (kilowatts)" 
        ,main="Global Active Power"
        ,col="red"
      )
    
  #copy png file to working directory..
  dev.copy(png, file="plot1.png", width=480, height=480)
  
  #close the connection
  dev.off()
}
