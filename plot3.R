library(tidyverse)
library(lubridate)
library(scales)

setwd(choose.dir())

#Plot 3

#Importing the filtered data
epc_filtered <- read.csv("epc_filtered.txt", header = TRUE,
                           sep = "")

##Combine date and time columns to dateTime and remove Date and Time columns
epc_filtered <- epc_filtered %>%
    mutate(dateTime = paste(epc_filtered$Date, epc_filtered$Time)) %>%
    select(dateTime, everything())

epc_filtered <- epc_filtered %>%
    select(-Date, -Time)

epc_filtered$dateTime <- as.POSIXct(epc_filtered$dateTime)

#Gather to put the sub_metering in one column and by type
epc_filtered_metering <- epc_filtered %>%
    gather(Sub_metering_1:Sub_metering_3, key = "metering_num", value = "value")

##The Plot
ggplot(epc_filtered_metering, aes(x = epc_filtered_metering$dateTime,
                         y = epc_filtered_metering$value)) +
    geom_line(aes(colour = epc_filtered_metering$metering_num)) +
    ylab("Energy sub metering") + xlab("") + 
    scale_x_datetime(date_breaks = "1 day", date_labels = c("Sat", "Thu", "Fri")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA, colour = "black"),
          legend.position = c(0.8, 0.85),
          axis.text.y = element_text(angle = 90)) +
    scale_colour_discrete(name = "")

