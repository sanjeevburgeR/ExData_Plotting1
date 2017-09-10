plot2 <- function()
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
  
  #second plot
  plot(householdData$Global_active_power~householdData$DateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  dev.copy(png,"plot2.png", width=480, height=480)
  dev.off()
   
}