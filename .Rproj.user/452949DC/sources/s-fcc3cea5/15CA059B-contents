####################
### Mapping in R ###
####################

# Shapefiles are a type of file that stores geometric location and attribute information. 
# Shapefiles can be points, lines, or polygons. 
# Often times, you would use a GIS software, like ArcMap or QGIS to view and create shapefiles. 
# You can also do this in R. 

# Examples of shapefiles include: bathymetry (seafloor depth) lines, state boundary polygons,
# and or sample point locations. 

# Note that shapefiles have seven separate files associated with one "layer". These files must be
# in the same place and have the same name for the shapefiles to work properly.

# All sample data should be located in: RScripts/sample-data/mapping/

# Here are the packages you will need to work with shapefiles. 
# install.packages("rgdal")
# install.packages("fields")

library(rgdal) # for reading in shapefiles
library(fields) # for a gradient of colors


### 
# Step 0: create data frame with names of all files we'll need
###
demo_data_folder <- "RScriptsDemo/sample-data/mapping"
files <- data.frame("bathymetry" = paste(demo_data_folder, "all-depths.shp", sep="/"), 
                    "states" = paste(demo_data_folder, "us-states.shp", sep="/"), 
                    "receiverlocs" = paste(demo_data_folder, "receiverlocs.csv", sep="/"), 
                    "vps" = paste(demo_data_folder, "vps-model.csv", sep="/"),
                    "image_output" = paste(getwd(), "shapefile-example.png", sep="/"),
                    stringsAsFactors = FALSE)



# Step 1: Load in the shapefiles. 
# Let's start with bathymetry data.

bathymetry<-readOGR(files$bathymetry) # this is the shapefile for bathymetry (seafloor depth)
  # there should be a line every 10 meters.
proj4string(bathymetry) # we can use this to see what geographical projection this layer is in
  # This projection is the same as decimal degrees
plot(bathymetry, col="blue") # let's plot it to see what it looks like

# Now on to the US state boundaries. 
states<-readOGR(files$states) # this is the shapefile for us states
proj4string(states) # let's check out the geographic projection
  # Notice that this says NAD83. This is not the same as WGS84 (decimal degrees), so we need to 
  # convert it. If we don't convert it, we won't be able to plot both shapefiles on top of one
  # another.
states<-spTransform(states, proj4string(bathymetry)) # this will transform the shapefile so
  # that it's in the same projection as the bathymetry layer
proj4string(states) # now they should match up.

plot(states, col="gray", add=T) # let's plot this and see how it looks. add=T allows us to add
  # another layer to the plot

# Step 2: Load in the other data.
# We can also add csv files with GPS locations to these data in order to map them together. 
receiver.locs<-read.csv(files$receiverlocs) # these are receiver locations for Echelle's project
points(receiver.locs$X, receiver.locs$Y) # let's add these points to the map to make sure they
  # show up. X has longitudonal coordinates in decimal degrees, and Y has latitudonal coordinates
  # in decimal degrees.

# We can also add some fish movement data. This is a simulated dataset with a fish that was 
# tagged in the receiver array.
vpsdata<-read.csv(files$vps) # VPS = VEMCO Positioning System (it transforms receiver presence/
  #absence data into real GPS positions)
vpsdata$times<-as.POSIXct(vpsdata$times, tz="UTC", format="%Y-%m-%d %H:%M:%S") # we need to make sure
  # that the times are read as times, because that's how we're going to color these points later. 
  # Note that the way these points are shown are in YYYY-mm-dd HH:MM:SS
vpsdata$numerictimes<-as.numeric(vpsdata$times) # now that our times are read as times, we will need
  # to convert them to numbers so that we can use them with our color gradient later (it only thinks
  # in numbers, not times). Using as.numeric() changes the times from posix to seconds since epoch, 
  # with the epoch beginning in 1970-01-01. We can convert the time back later if we want to, but it's
  # not necessary right now. 

# Step 3: Set our boundaries for our plot.
xlimits<-c(-118.076750, -117.953354) # i'm only interested in this range of longitudes
ylimits<-c(33.558575, 33.637742) # and this range of latitudes

# Step 4: Plot it.
png(files$image_output, height=480*8.5, width=480*11, res=72*8) # this line will create a png file
  # in our working directory, with the following dimensions and resolutions
plot(receiver.locs$X, receiver.locs$Y, xlim=xlimits, ylim=ylimits, xlab="Longitude (DD)", 
     ylab="Latitude (DD)", type="n") # we're going to start by plotting a blank graph using the receiver
  # coordinates as a dummy dataset. We want the xlimits and ylimits to be those that we specified above, 
  # and we want the x and y labels to be intuitive (Latitude and Longitude in decimal degrees). We use
  # type = "n" to specify that we don't actually want the data plotted. That's because we're going to add
  # some basemaps first, and layering properly is very important to how the plot turns out in the end.
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="slategray1") # this creates a 
  # rectangle within the plot area, starting from the smallest x value and the smallest y value and going
  # to the largest x value and thte largest y value. we decide the color should be "slategray1"
plot(bathymetry, col="slategray3", lwd=2, add=T) # now, we're going to add the bathymetry lines. We choose
  # the color to be "slategray3" so that it stands out against the background, but is not overpowering. 
  # lwd=2 means that the line width will be 2 (default is 1). We are adding this to the plot, not making
  # a new one, so add=T
plot(states, col="gray", add=T) # we want to add the US states to this map as well, and color them gray
points(receiver.locs$X, receiver.locs$Y, pch=20) # now, we're re-adding our gps positions, as solid black
  # points.
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="NA") # When we added the shapefiles
  # to the map, the outline of the map was no longer black (the shapefiles bled over), so we're going to
  # add another, clear (col="NA"), rectangle over the plot so that our outline is nice and crisp.
points(vpsdata$LON, vpsdata$LAT, pch=20, 
       col=color.scale(vpsdata$numerictimes,zlim=c(min(vpsdata$numerictimes), max(vpsdata$numerictimes)),
                        col=rev(tim.colors(256))))
  # Finally, we want to add our simulation data, with the x axis being the LON coordinates, y axis being
  # the LAT coordinates. We want the points to be filled in, and we are going to color them by the numeric
  # time values. The beginning of the track will have red dots, and the end of the track will have blue
  # dots. 
dev.off() # this ends our graphing window and saves the finished plot

