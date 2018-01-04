plot4 <- function()
{
  library(dplyr)
  
  ##read the UCI data set for Electric power consumption..
  ds_power <- filter(
    read.csv("household_power_consumption.txt", sep = ';', header = TRUE), 
    as.Date(Date,"%d/%m/%Y") == "2007-02-01" | as.Date(Date,"%d/%m/%Y") == "2007-02-02"
    
  )
  
  ds_power$Date <- as.Date(ds_power$Date,"%d/%m/%Y")
  ds_power <- transform(ds_power, timestamp=as.POSIXct(paste(Date, Time), "%d/%m/%Y %H:%M:%S"))
  
  #set 2 rows and 2 columns display..
  par(mfrow=c(2,2))
  
  #1
  plot(ds_power$timestamp,ds_power$Global_active_power, type="l", xlab="", ylab="Global Active Power")
  
  #2
  plot(ds_power$timestamp,ds_power$Voltage, type="l", xlab="datetime", ylab="Voltage")
  
  #3
  plot(ds_power$timestamp,ds_power$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(ds_power$timestamp,ds_power$Sub_metering_2,col="red")
  lines(ds_power$timestamp,ds_power$Sub_metering_3,col="blue")
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), bty="n", cex=.5)
  
  #4
  plot(ds_power$timestamp,ds_power$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
  
  #copy png file to working directory..
  dev.copy(png, file="plot4.png", width=480, height=480)
  
  #close the connection
  dev.off()
}

