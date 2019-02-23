################################
### Creating an abacus plot ####
################################


###
# 0. Load R packages required in the script
###
library(tidyverse)   # collection of R packages designed for data science, see: https://www.tidyverse.org/
library(lubridate)

# The fields package has a great color scheme: tim.colors(256)
# Install a package into R only once
# install.packages("fields")   # make this a comment after running it

library(fields)

# Location of sample data
demo_data_folder <- "RScriptsDemo/sample-data/abacus"

# 1. Read in the data files. 
files <- list.files(path = demo_data_folder, pattern="csv", full.names = TRUE) # list all the csv files in this folder

detections <- NULL # set this to null for now, so that we can add to it later
for( f in files ) { # for each file... 
  temp_rows <- read_csv(f)  # load the data from one file
  detections <- bind_rows(detections, temp_rows)
}

# So now we have one dataframe with all our detection data. 

# clean up environment by removing names no longer needed
remove(temp_rows, f, files) 

######
# 2. Some general housekeeping
######

# We need to make sure that the date/time are read correctly by R. 

head(detections) # this will show us the first few rows of the data we are working with.
  # 1: DateTime; 2: Receiver ID; 3: Transmitter ID; 4: Transmitter Name (often NA); 5: Transmitter 
  # Serial Number (often NA); 6: Transmitter Sensor Value (often NA); 7: Sensor Units of measurement
  # (often NA); 8: Receiver Station Name; 9: Latitude of receiver (DD); 10: Longitude of receiver (DD)

# Notice that the first column is for date and time, but it has a really weird column name
colnames(detections)[1]<-"Date_Time" # let's rename it so it is easier to call to
detections$Date_Time[1:5]   # Date_Time of first 5 rows

class(detections$Date_Time) # We need to see what format date/time is in 
  # You should receive a note in the bottom telling you that it's a character string. 
  # But it should be a datetime. We need to convert it

# the lubridate package has a family of functions to convert to date, datetime, etc.

detections <- mutate(detections, Date_Time = mdy_hm(Date_Time))

# Confirming conversion:
class(detections$Date_Time)
detections$Date_Time[1:5]


# Convert the Receiver and Transmitter to factors (i.e. categories)
detections <- mutate(detections, Receiver = as.factor(Receiver), Transmitter = as.factor(Transmitter))
head(detections)

# Note the datatype of Receiver and Transmitter is now factor

###
# Step 3: Wouldn't it be cool to be able to plot this by receiver Latitude???
###

# We're going to use tim.colors for this plot, but we want to make sure that we can not only see
# all of the data, but also that we can differentiate latitudes when we plot it. 

# First, let's look at the min and max of the data
min(detections$Latitude) # 33.5714
max(detections$Latitude) # 34.057

# Even though this doesn't seem like a big gap, for GPS data, it kind of is. Let's check to see 
# whether the data are pretty equally dispersed. 
quantile(detections$Latitude)
# Oof, not really, the first three quantiles of the data are close to 33.5, but the 4th quantile
# is 0.5 degrees higher 

# I think we should specify our min and max for our color scheme so that it 
# incorporates much of the data, but doesn't force us to get rid of our resolution
min_lat <- 33.55
max_lat <- 33.6
# With 0.05 degree of resolution, this should work quite well.


##########################
# Using ggplot()   See: https://ggplot2.tidyverse.org/
##########################

# Now we want to plot the time of the detection on the x-axis, the Transmitter on the y-axis. 
# To color it by Latitude

# aes() stands for aesthetic

abacus_plot <- ggplot(data = detections, mapping = aes(x = Date_Time, y = Transmitter)) +
  geom_point(mapping = aes(color = Latitude))

# By default, a two-color gradient is used on continuous data
abacus_plot


# Looks good! We can make it better!

# As expected, Latitude range is too large, most data is concentrated in the range [minlat, maxlat]
# let's plot that data instead using the limit argument and points outside this range will use black
# In addition, we'll use a rainbow of colors instead of the limiting two-color gradient

#   use a color for Latitude values in the range [min_lat, max_lat]
#   use #000000 for Latitude values outside this range
#   we use 256 colors comes from tim.colors in reverse. 256 color values provides a nice gradient,
#      we chose to put them in reverse because you typically have warmer waters at smaller latitudes
#      (so that means red will be at the smaller latitudes, and blue will be at the larger latitudes).

# You can choose any amount of colors you want in the future, and any color scheme you want. 

# All this is done in ggplot by adding scales to our plot


# Using scales to use a rainbow of colors instead and limit the data as well.
# Plotting all data with limits of min/max latitude using rainbow of colors
abacus_plot_v2 <- abacus_plot + scale_color_gradientn(limits=c(min_lat, max_lat), 
                                                      breaks = seq(min_lat, max_lat, 0.02), na.value = "#000000",
                                                      guide="colorbar", aesthetics = "color", colors = rev(tim.colors(256)))
abacus_plot_v2

# Great, but the legend is too short, we can control the legend in this case let's set the heigh
abacus_plot_v3 <- abacus_plot_v2  +  theme(legend.key.height = unit(0.1, "npc"))
abacus_plot_v3


# We can do one last improvement. There are over 45,000 detections
dim(detections)

# Our plot is hiding these because points are drawn on top of each other. 
# How do we resolve this? Answer: By adding some noise


#### Other attempts ####

# Plotting data with limits of min/max latitude using two color gradient

plot_2color_gradient <- abacus_plot + scale_color_continuous(limits=c(min_lat, max_lat), 
                                                   breaks = seq(min_lat, max_lat, 0.02), 
                                                   guide="colorbar", aesthetics = "color", 
                                                   low="darkorange", high="darkorchid") + 
  theme(legend.key.height = unit(0.1, "npc"))

plot_2color_gradient



# The R package RColorBrewer has some nice color palettes
#   See: https://www.rdocumentation.org/packages/RColorBrewer/versions/1.1-2/topics/RColorBrewer

######################





# "Phew! And now we have a nice abacus plot!"   -- Echelle Burns.
