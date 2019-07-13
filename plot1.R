library(tidyverse)
library(lubridate)
library(scales)

setwd(choose.dir())

#Imports the data set
epc <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")

#Converts the Date column to YMD from DMY
epc$Date <- as.Date(parse_date_time(epc$Date, "dmy"))

#Filters the dates to our specified time between 2007-02-01 and 2007-02-02
epc_filtered <- epc %>%
    filter(between(epc$Date, as.Date("2007-02-01"), 
                   as.Date("2007-02-02")))

#Convert the Global_active_power into a numeric value 

epc_filtered$Global_active_power <- as.numeric(as.character(epc_filtered$Global_active_power))


#Exporting data so that I can import smaller file each time

write.table(epc_filtered, "epc_filtered.txt")

#Plot 1

ggplot(epc_filtered, aes(x = Global_active_power, colour = "black")) +
    geom_histogram(aes(fill = "red"), colour = "black", bins = 13,
                   boundary = 0) +
    scale_x_continuous(breaks = seq(0,6, by = 2), limits = c(0, 6)) +
    scale_y_continuous(breaks = seq(0, 1200, by = 200)) + 
    ylab("Frequency") + xlab("Global Active Power (kilowatts)") +
    ggtitle("Global Active Power") +
    theme_minimal() + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.y = element_text(angle = 90))