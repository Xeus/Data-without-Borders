# Data Without Borders
# Jake Porway
#
# Victor Ben Turner, vt520@nyu.edu
# Homework: Week #6

# PREPARATION/MISE EN PLACE
# ### FIX PATH!!!
tweets <- read.csv("/Users/benturner/Dropbox/Code/R/Data without Borders/week6/tweets2009.csv", header=FALSE, as.is=TRUE)
attach(tweets)

# Assign headers.
names(tweets)  <- c("time", "seconds", "screen_name", "text")

# Convert time column into time object.
times <- as.POSIXlt(tweets$time)
seconds <- as.numeric(times)

# ANSWERS BELOW

# PART 1

# Q: Make a plot of the timeseries as a line (plot() with type=’l’) just so we can see our 
# data (recall you can create a timeseries by pulling out the “counts” entry of the 
# histogram object).  What do you see?
h <- hist(seconds, breaks=500)
plot(h$counts, type='l')
# A: I see a pattern (albeit not an entirely regular one) of an undulating wave. The plot
# has fairly regular peaks and valleys but sometimes there are two peaks to every valley.
# This all suggests something cyclical with high activity denoting a traditionally active
# period and low activity denoting a traditionally quiet period.

# Q: Let’s figure out if there are any cycles / seasonal trends in this data.  Use the 
# acf() function to identify cycles in the tweet frequency, just as we did with the 
# NYPD data.  Recall that acf() only looks at a small time frame so you’ll want to 
# pass it a lag.max argument that’s about 200 or more.  Where is it most likely that 
# we have a cycle and how can you tell?
highest.correlation <- acf(h$counts, lag.max=175)
# A: 175 seems to be where the cycle completes a wave and starts a new one (half a peak,
# two valleys, half a peak).  It is also at the highest point of the peak, with highest
# correlation.

# Q: OK, let’s remove the cycles and analyze this data.  Create an official timeseries 
# object with frequency equal to the cycle length.  Use decompose() to decompose 
# the timeseries into its components and plot the results.  What do you see in terms 
# of an overall trend?
tweet.counts <- ts(h$counts, frequency=175)
parts <- decompose(tweet.counts)
plot(parts)
# A: The observed chart showed a bit of the seasonal (daily) pattern, but it had been
# muted.  The trend chart shows an arcing curve that starts high, slopes lower, then starts
# to turn up again.  The seasonal chart seems to be consistent with a daily pattern, starting
# low in mid-day (maybe my lag was off?) but traffic picking up into the evening and the
# rest of the night (accounting for western time zones?).

# PART 2

iran.tweets <- tweets[grep("iran", ignore.case=TRUE, tweets$text), ]
iran.times <- as.POSIXlt(iran.tweets$time)
iran.seconds <- as.numeric(iran.times)

# Q: Plot the time series for iran.tweets using a histogram with breaks=100.  Add red 
# vertical lines to the plot at the 3 largest peaks using abline().
iran.h <- hist(iran.seconds, breaks=100)
plot(iran.h$counts, type='l')
abline(v=c(43, 58, 65), col="red")

# Q: There’s not a lot of seasonality in this plot, so let’s go straight to analyzing the 
# trend.  Use SMA() with the default settings to smooth the signal and plot it.
library(TTR)
iran.smoothed <- SMA(iran.h$counts, n=3)
plot(iran.smoothed, type='l', main="Frequency of Iran Tweets", xlab="Time", ylab="Iran Tweet Counts")

# Q: Let’s build a basic event detection algorithm, but let’s not use the total number of 
# tweets, since that misses the “velocity” of the signal.  Use the diff() function with a 
# lag of 5 to look at the differences in tweet volume over time on the smoothed 
# signal (use ?diff if you need a refresher).  Create a figure with two graphs – one 
# with the smoothed signal above and one with the diff() of the signal below it.  
# What do you see?
iran.velocity <- diff(iran.smoothed)
par(mfrow=c(2,1))
plot(iran.smoothed, type='l')
plot(iran.velocity, type='l', main="Velocity")
# A: At the beginning, there is low velocity or change in input, but it rapidly picks up
# at around the 37-40 mark, and then there is a large change in the velocity for the rest
# of the time while volume remains much higher (it started at near zero).  So something
# big happened to cause a lot more variation and frequency.

# Q: I’d like to know why all these tweets started increasing.  Can we figure out what 
# time the tweets started increasing using your results from diff, i.e. where is the 
# biggest jump in tweets? (hint:  there are lots of ways to do this, many of which 
# require you to remove the NAs created by SMA)  Pull 20 or so tweets from 
# around that time and write down why you think they’re increasing based on what 
# people are saying.
iran.tweets$time[37] # 2009-06-12 19:40:58
iran.tweets$text[37:57]
# A: 37-40 index was when it began to pick up. This correlates with June 12, 2009, at 7:41PM.
# It appears that this time period was when Ahmadinejad "won" the Iranian presidential elections,
# despite strong opposition.
# "RT @atompkins: Iranian state media says Admadinejad wins re-election http://bit.ly/eon7L"