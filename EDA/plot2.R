plot2 <- function()
{
  library(dplyr)
  
  ##read the UCI data set for Electric power consumption..
  ds_power <- filter(
    read.csv("household_power_consumption.txt", sep = ';', header = TRUE), 
    as.Date(Date,"%d/%m/%Y") == "2007-02-01" | as.Date(Date,"%d/%m/%Y") == "2007-02-02"
    
  )
  
  ds_power$Date <- as.Date(ds_power$Date,"%d/%m/%Y")
  ds_power <- transform(ds_power, timestamp=as.POSIXct(paste(Date, Time), "%d/%m/%Y %H:%M:%S"))
  plot(
    ds_power$timestamp
    ,ds_power$Global_active_power
    ,xlab=""
    ,ylab="Global Active Power (kilowatts)"
    ,type="l"
  )
  
  #copy png file to working directory..
  dev.copy(png, file="plot2.png", width=480, height=480)
  
  #close the connection
  dev.off()
}
