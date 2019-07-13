library(tidyverse)
library(lubridate)
library(scales)

setwd(choose.dir())

#Plot 2

#Importing the filtered data
epc_filtered <- read.table("epc_filtered.txt", sep = "")

##Combine date and time columns to dateTime and remove Date and Time columns
epc_filtered <- epc_filtered %>%
    mutate(dateTime = paste(epc_filtered$Date, epc_filtered$Time)) %>%
    select(dateTime, everything())

epc_filtered <- epc_filtered %>%
    select(-Date, -Time)

epc_filtered$dateTime <- as.POSIXct(epc_filtered$dateTime)


#The Plot
ggplot(epc_filtered, aes(x = epc_filtered$dateTime,
                           y = epc_filtered$Global_active_power)) +
    geom_line() +
    ylab("Global Active Power (kilowatts)") + xlab("") + 
    scale_x_datetime(date_breaks = "1 day", date_labels = c("Sat", "Thu", "Fri")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA, colour = "black"),
          axis.text.y = element_text(angle = 90))

