# Name: plot4
# Purpose: plot four plots, global active power, voltage, global reactive power and sub engery metering
# against selected dates(converted to week days)
# and save it to a png file as plot4.png
# the given dates are "01/02/2007", "02/02/2007" 


plot4 <- function(){
  dates<- c("01/02/2007", "02/02/2007" ) 
  plot_4(dates)
}

plot_4<- function (dates) {
  
  path = getwd()
  png(filename = paste(path,"/plot4.png", sep=""), width=480, height= 480, units="px", pointsize = 12,     bg="white")
  
  sourcedf <- createdataframe(dates, 10000)
  
  summar <- summarise(group_by(sourcedf, Date), marks=n())
  days <- weekdays(as.Date(summar[[1]], "%d/%m/%Y"), TRUE)
  days <- c(days, weekdays(as.Date(summar[[1]][dim(summar)[1]], "%d/%m/%Y") + 1, TRUE))
  times <- c(1)
  for(i in (1:dim(summar)[1]))
  {
    times <- c(times, times[i] + summar[[2]][i])
  }
  
  par(mfrow=c(2,2), mar=c(4,4,1,0))
  
  plot(x=1:dim(sourcedf)[1], y= as.numeric(sourcedf$Global_active_power), type="l", ylab="Global Active Power (kilowatts)", xaxt="n", xlab="")
  axis(1, at = times, labels = days)
  
  
  plot(x=1:dim(sourcedf)[1], y=as.numeric(sourcedf$Voltage), type="l", lwd=1.0, ylab="Voltage", xaxt="n", xlab="date time")
  axis(1, at = times, labels = days)
  
  plot(x=1:dim(sourcedf)[1], y=as.numeric(sourcedf$Sub_metering_1), type="l", lwd=1.0, ylab="Energy sub metering", xaxt="n", xlab="")
  lines(1:dim(sourcedf)[1], as.numeric(sourcedf$Sub_metering_2), type ="l", lwd=1.0, col="red")
  lines(1:dim(sourcedf)[1], as.numeric(sourcedf$Sub_metering_3), type ="l", lwd=1.0, col="blue")
  axis(1, at = times, labels = days)
  legend("topright", pch=1, col=c(1, 2, 3), legend=c(names(sourcedf)[[7]], names(sourcedf)[[8]],names(sourcedf)[[9]]))
  
  
  plot(x=1:dim(sourcedf)[1], y=as.numeric(sourcedf$Global_reactive_power), type="l", lwd=1.0, xaxt="n", xlab="date time", ylab="Global_reactive_power")
  
  axis(1, at = times, labels = days)
  dev.off()
}


# Name: createdataframe
# Purpose: Create a dataframe that contains releveant data records for given dates.
# Because of large size of the data file it is preferred to read only concerned records 
# from the source data file into a dataframe in memory.  This is to avoid the pitfall 
# that computer RAM is not big enough to hold the entire data records in memory.
# Use read.table method to read first block of rows (pre defined number such as 10000 rows) 
# that include the first row as header for the data frame to be created.  Then subset any rows 
# from this newly created data frame whose values in its first column Date match the given dates 
# and assign it to itself.  This data frame will be the final output data frame that contains 
# only date matched rows.
# Repeat the process to continue reading a block of rows to create a working data frame within a while loop, 
# save the working data frame's row nubmer, and then subset any rows from this working data frame 
# whose date match the given dates and assign it to itself. 
# Then call rbind to append current subsetted working data frame to the first data frame.
# If saved row number of current working data frame is not equal to the pre defined block number 
# it means it has reached the end of file. So the process stops. 
# The fianl output is a data frame contains all rows with matching dates.
# 
# In case the last block of rows in the source text file matches the predefined block number an error 
# would be raised because there are no more rows to read from the source text file.  
# The work around this is to change the predefined block of rows to read.  

# Author: Jian Song
# Date: 05/28/2017
createdataframe <- function(dates, readblocksize = 1000){
  path <- getwd()
  sdfileconn <- file(paste(path, "/household_power_consumption.txt", sep=""), "r")
  
  dtFormat <- "%d/%m/%Y"
  dates <- as.Date(dates, format = dtFormat)
  df <- read.table(sdfileconn, header = TRUE, nrows = readblocksize, sep=";", stringsAsFactors = FALSE)
  head(df, 10)
  df <- subset(df, as.Date(df$Date, format = dtFormat) %in% dates)
  
  continue <- readblocksize
  while (continue == readblocksize)
  {
    dfline <- read.table(sdfileconn, header = TRUE, nrows = readblocksize, sep=";", stringsAsFactors = FALSE)
    continue <- dim(dfline)[1]
    names(dfline) <- names(df)
    dfline <- subset(dfline, as.Date(dfline$Date, format = dtFormat) %in% dates)
    df <- rbind(df, dfline)
  }
  close(sdfileconn)
  df
}