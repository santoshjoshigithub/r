plot3 <- function()
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
        ,as.numeric(as.character(ds_power$Sub_metering_1))
        ,xlab=""
        ,ylab="Energy sub metering"
        ,type="l"
  )
  
  lines(ds_power$timestamp,ds_power$Sub_metering_2,col="red")
  lines(ds_power$timestamp,ds_power$Sub_metering_3,col="blue")
  
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), lwd=c(1,1))
  
  #copy png file to working directory..
  dev.copy(png, file="plot3.png", width=480, height=480)
  
  #close the connection
  dev.off()
}

