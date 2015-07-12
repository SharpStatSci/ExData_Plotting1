# plot4.R
# Exploratory Data Analysis Project 1
# July 11 2015, Jesse Sharp
# Practice using git, github and R plots

# Instructions

#The dataset has 2,075,259 rows and 9 columns. calculate a rough estimate of how much memory
#the dataset will require before reading into R.
#We will only be using data from the dates 2007-02-01 and 2007-02-02.
#One alternative is to read just those dates rather than reading in the entire dataset
#You may find it useful to convert the Date and Time variables to Date/Time classes in R
#using the strptime() and as.Date() functions.
#Note that in this dataset missing values are coded as ?.

#For each plot you should
#Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.

#Name each of the plot files as plot1.png, plot2.png, etc.
#Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e.
#code in plot1.R constructs the plot1.png plot. Your code file should include code for reading the data
#so that the plot can be fully reproduced. You must also include the code that creates the PNG file.

# Conservatively
# size_est <- (2075259*9*8)/1000/1000
# size_est [1] 145.9166 MB

setwd("C:/StatWare/Rprog/ExplorAnalysis/project1")
library("data.table")

# Ineffeciently read the data then subset, I could not make a conditional function work
na_list <- c("?","'?'","NA","")
power <- fread("C:/StatWare/Rprog/ExplorAnalysis/HPCdata/power_use.txt", na.strings=na_list)

str(power)

# Read 2075259 rows and 9 (of 9) columns from 0.124 GB file in 00:00:03

# Transform the date from character back to date and then filter
date_char <- power[,power$Date]
power$Date2 <- as.Date(date_char, format="%d/%m/%Y")
head(power$Date2)

power$Date <- as.Date(power[,power$Date],format="%d/%m/%Y" )

power.df <- power[(power$Date=="2007-02-01") | (power$Date=="2007-02-02"),]

# Convert measures back to numeric
#df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
power.df$Global_active_power <- as.numeric(power.df$Global_active_power)
power.df$Global_reactive_power <- as.numeric(power.df$Global_reactive_power)
power.df$Voltage <- as.numeric(power.df$Voltage)
power.df$Sub_metering_1 <- as.numeric(power.df$Sub_metering_1)
power.df$Sub_metering_2 <- as.numeric(power.df$Sub_metering_2)
power.df$Sub_metering_3 <- as.numeric(power.df$Sub_metering_3)

# Create a date time field
power.df <- transform(power.df, Date.Time=as.POSIXct(paste(power.df$Date, power.df$Time)), "%d/%m/%Y %H:%M:%S")

# Recreate plot 4: A plot by minute of global active power by submeter

plot4 <- function() {
    par(mfrow=c(2,2))

    # subplot 1
    plot(power.df$Global_active_power~power.df$Date.Time, type="l", xlab=" ", ylab="Global Active Power")
    # subplot 2
    plot(power.df$Voltage~power.df$Date.Time, type="l", xlab="datetime", ylab="Voltage")
    # subplot 3
    plot(power.df$Sub_metering_1~power.df$Date.Time, type="l", xlab=" ", ylab="Energy sub metering")
    lines(power.df$Sub_metering_2~power.df$Date.Time,col="red")
    lines(power.df$Sub_metering_3~power.df$Date.Time,col="blue")
    legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  ")
           ,lty=c(1,1), bty="n", cex=.5)
    #lty removes the box, cex shrinks the text, spacing added after labels so it renders correctly
    # subplot 4
    plot(power.df$Global_reactive_power~power.df$Date.Time, type="l", xlab="datetime", ylab="Global_reactive_power")
    dev.copy(png, file="plot4.png", width=480, height=480)
    dev.off()
    cat("plot4.png saved", getwd())
}
plot4()

