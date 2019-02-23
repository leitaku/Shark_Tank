# Plot data points
library(tidyverse)
library(lubridate)

# Location of sample data
demo_data_folder <- "JWSDetectionData"

# 1. Read in the data files. 
files <- list.files(path = demo_data_folder, pattern="csv", full.names = TRUE) # list all the csv files in this folder

detections <- NULL # set this to null for now, so that we can add to it later
for( f in files ) { # for each file... 
  temp_rows <- read_csv(f)  # load the data from one file
  detections <- bind_rows(detections, temp_rows)
}

# clean up environment by removing names no longer needed
remove(temp_rows, f, files) 

colnames(detections)[1]<-"Date_Time" # let's rename it so it is easier to call to
#detections$Date_Time[1:5]

# Convert the Receiver and Transmitter to factors (i.e. categories)
detections <- mutate(detections, Receiver = as.factor(Receiver), Transmitter = as.factor(Transmitter))
head(detections)

##################################################################
library(ggplot2)
library(maps)
library(ggthemes)

detections2 <- detections[-which(detections$Latitude < 29),]

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() +
  coord_cartesian(ylim = c(33.05, 34.8), xlim = c(-121, -117.05)) 
#ylim = c(33, 35), xlim = c(-122, -113)
map <- world +
  geom_point(aes(x = Longitude, y = Latitude),
             data = detections2, 
             colour = 'firebrick', alpha = 0.05) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

map

########get unique locations##################################################
detections3 <- unique(detections2[,c("Station.Name", "Longitude", "Latitude")])
map <- map +
  geom_point(aes(x = Longitude, y = Latitude),
             data = detections3, 
             colour = 'black', alpha = 1) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

map
##############################################################################
# min & max values
# min(detections2$Latitude) 
# max(detections2$Latitude)
# min(detections2$Longitude)
# max(detections2$Longitude)
##################################################################







