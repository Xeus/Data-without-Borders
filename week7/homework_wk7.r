# Data Without Borders
# Jake Porway
#
# Victor Ben Turner, vt520@nyu.edu
# Homework: Week #7

# PREPARATION/MISE EN PLACE
# ### FIX PATH!!!
snf <- read.csv("/Users/benturner/Dropbox/Code/R/Data without Borders/week7/snf_3.csv", header=TRUE, as.is=TRUE)
attach(snf)
geo <- read.csv("/Users/benturner/Dropbox/Code/R/Data without Borders/week7/geo.csv", header=TRUE, as.is=TRUE)
attach(geo)
# ANSWERS BELOW

# PART 1

# To merge the two datasets, we’re going to have to create a common column for 
# our x/y values instead of two separate columns.  We can do that with the paste()
# function, which pastes comma-separated bits of data together.  Assuming your 
# data is loaded from snf_3.csv as “snf”, we can join the two columns with paste()
# like so: 
#  snf$xy <- paste(snf$x, “,”, snf$y, sep=””)
snf$xy <- paste(snf$x, ",", snf$y, sep="")

# Q: Create an “xy” column in the geo.csv data frame using the same method.
geo$xy <- paste(geo$xcoord, ",", geo$ycoord, sep="")

# Q: Merge the two datasets using the merge() function we learned about in Lecture 7.
merged <- merge(snf, geo, by="xy")

# Q: Cool, we’ve got our map, let’s add the points to it.  Use the points()  function
# to add the lat/lon points of every stop onto the map.  Use the rgb() function from
# Lecture 7 to set the color of the points so that they have some transparency.
library(maps)
map('county', 'new york', xlim=c(-74.25026, -73.70196), ylim=c(40.50553, 40.91289), mar=c(0,0,0,0), bg=1, col=8)
map.axes()
points(merged$lon, merged$lat, col=rgb(0,255,255,10,maxColorVal=255), pch=19, cex=0.8, bg=0)
# Q: What do you see?
# A: Concentrations of stops in the downtown Brooklyn area and in the northern part of
#    Manhattan turning into the Bronx. There are also some "stops" in the water between
#    Brooklyn and Staten Island, so that seems like either errors in reporting, or a ton
#    of stops on ferries! Some areas seem to have no data at all, which is worth noting.

# Q: Method 1 – The Simplest, Built-in Colors:  Let’s just use R’s default colors to 
# plot the races of each stop.  To do that, we need to map our race values to 
# integers (e.g “B” = 1, “W” = 2).  Note that our races are back to being characters 
# (e.g. “B”, “W”, “Q”, etc.).  Check this trick:  to convert them, we can use as.factor() 
# to treat the race characters as factors (review Lecture 8 on factors if that doesn’t 
# quite make sense) and then use as.numeric() to compress those factor values 
# down into integers.  Plot those points using the resulting vector of integers as the 
# color vector.
merged$race_num <- as.numeric(as.factor(merged$race))

map('county', 'new york', xlim=c(-74.25026, -73.70196), ylim=c(40.50553, 40.91289), mar=c(0,0,0,0), bg=1, col=8)
map.axes()
# "B" "Q" "P" "W" "Z" "U" "A" "I"
races <- unique(merged$race)
# 2 5 4 7 8 6 1 3
races_by_num <- as.numeric(as.factor(unique(merged$race_num)))
points(merged$lon, merged$lat, col=rgb(t(col2rgb(merged$race_num)), alpha=20, max=255), pch=19, cex=0.8, bg=0)
# NOTE: SEE http://research.stowers-institute.org/efg/R/Color/Chart/

# Method 3 – The Craziest, Color Brewer’s Palettes:
library(RColorBrewer)
colors <- brewer.pal(8, "Set3")
race.colors <- data.frame(race=unique(snf$race), colors)
# TODO: Now what? How to map race color number to hex code?

# Q: No matter which plot you made, what do you see?
# A: Yellow points mainly in Staten Island and SW Brooklyn. Big concentrations of
#    primarily red dots in Brooklyn and East Brooklyn and, perhaps, the Rockaways.
#    Without seeing transparencies, it looks like it's mixed in most other places.
#    Maybe some cyan concentrations near Red Hook and Williamsburg/Greenpoint.

# Q: Load up the animation library that we learned about in Lecture 7.
library(animation)

# Q: Create a day column in our snf data.
# Use POSIXct and lubridate package as we did in Lecture 6 to convert 
# the time strings to time objects and pull out the day using the mday() 
# function from lubridate.
library(lubridate)
merged$day <- mday(as.POSIXct(merged$time))

# Q: Create a for loop that ranges from 1:30 (one for each day in November) and plot 
# the points that occur on each day in each iteration of the loop.  Wrap this all in 
# the saveHTML() function so that it makes an animation.
saveHTML({ for (i in 1:30) {
  map('county', 'new york', xlim=c(-74.25026, -73.70196), ylim=c(40.50553, 40.91289), mar=c(0,0,0,0), bg=1, col=8)
  points(merged$lon[merged$day == i], merged$lat[merged$day == i], col=merged$race_num, pch=18, cex=0.8)
  ani.pause()
}
})

# Q: What do you see?
# A: I don't see that much from this view. Perhaps I can guess where a weekend is, and
#    imagine I see more incidents occur on weekend days, but that is too big an inference.