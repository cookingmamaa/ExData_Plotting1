library(ggpubr)
library(tidyverse)
library(lubridate)
library(scales)
theme_set(theme_pubr())

setwd(choose.dir())

#Plot 4

#Importing the filtered data
epc_filtered <- read.table("epc_filtered.txt", sep = "")

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

#Plot 2
p2 <- ggplot(epc_filtered, aes(x = epc_filtered$dateTime,
                         y = epc_filtered$Global_active_power)) +
    geom_line() +
    ylab("Global Active Power") + xlab("") + 
    scale_x_datetime(date_breaks = "1 day", date_labels = c("Sat", "Thu", "Fri")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA, colour = "black"),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(angle = 90))

#Plot 3
p3 <- ggplot(epc_filtered_metering, aes(x = epc_filtered_metering$dateTime,
                                  y = epc_filtered_metering$value)) +
    geom_line(aes(colour = epc_filtered_metering$metering_num)) +
    ylab("Energy sub metering") + xlab("") + 
    scale_x_datetime(date_breaks = "1 day", date_labels = c("Sat", "Thu", "Fri")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA, colour = "black"),
          legend.position = c(0.7, 0.8),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(angle = 90)) +
    scale_colour_discrete(name = "") 

#Plot A
plota <- ggplot(epc_filtered, aes(x = epc_filtered$dateTime,
                         y = epc_filtered$Voltage)) +
    geom_line() +
    ylab("Voltage") + xlab("datetime") + 
    scale_x_datetime(date_breaks = "1 day", date_labels = c("Sat", "Thu", "Fri")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA, colour = "black"),
          axis.text.y = element_text(angle = 90)) +
    scale_y_continuous(breaks = seq(234, 246, by = 4))

#Plot B
plotb <- ggplot(epc_filtered, aes(x = epc_filtered$dateTime,
                                  y = epc_filtered$Global_reactive_power)) +
    geom_line() +
    ylab("Global_reactive_power") + xlab("datetime") + 
    scale_x_datetime(date_breaks = "1 day", date_labels = c("Sat", "Thu", "Fri")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA, colour = "black"), 
          axis.text.y = element_text(angle = 90))

#Combining all the plots
ggarrange(p2, plota, p3, plotb, ncol = 2, nrow = 2)
