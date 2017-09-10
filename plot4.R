plot4 <- function()
{
  ncol <- 9
  nrows <- 2075259
  totalBytes <- (ncol * 40) + (ncol*nrows*8)
  
  sizeinMB <- round(totalBytes/2^{20},2)
  availableRAM <- memory.limit()-memory.size()
  
  if(sizeinMB > availableRAM)
  {
    stop("No enough memory available to load the dataset")
  }
  
  householdData <- read.table("household_power_consumption.txt", skip=grep("1/2/2007", readLines("household_power_consumption.txt")), nrows = 2880,sep=";",na.strings = "?")
  names(householdData) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
  householdData <- householdData[complete.cases(householdData),]
  householdData$Date <- as.Date(householdData$Date, "%d/%m/%Y")
  householdData <- mutate(householdData, DateTime = paste(householdData$Date, householdData$Time))
  
  householdData$DateTime <- as.POSIXct(householdData$DateTime)
  
  #fourth plot
  par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
  with(householdData, 
    plot(Global_active_power~DateTime, type="l", 
         ylab="Global Active Power (kilowatts)", xlab=""))
  with(householdData,
    plot(Voltage~DateTime, type="l", 
         ylab="Voltage (volt)", xlab=""))
  
  with(householdData, plot(Sub_metering_1~DateTime, type="l", ylab="Energy sub metering", xlab=""))
  with(householdData, lines(Sub_metering_2~DateTime,col='Red'))
  with(householdData, lines(Sub_metering_3~DateTime,col='Blue'))
  legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  with(householdData,
  plot(Global_reactive_power~DateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab=""))
  
  
  dev.copy(png,"plot4.png", width=480, height=480)
  dev.off()
  
}