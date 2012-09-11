# Data Without Borders
# Jake Porway
#
# Victor Ben Turner, vt520@nyu.edu
# Homework: Week #1

# PART 1

# PREPARATION/MISE EN PLACE
# loads stop/frisk data from external CSV
# ### FIX PATH!!!
data <- read.csv("http://www.jakeporway.com/teaching/data/snf_11_2011_1.csv", header=TRUE, as.is=TRUE)
attach(data)

# ANSWERS BELOW
# Q: How many women were stopped?
# A: 3,927 entries found classified as "F" for "sex"
females <- data[data$sex == "F",]
numFemales <- nrow(females)
print(numFemales)

# Q: What percentage of the stops is this?
# A: 6.76% of total stops
totalStops <- nrow(data)
percentageFemales <- numFemales/totalStops
print(percentageFemales)

# Q: How many different kinds of suspected crimes are there?
# A: 1,356 kinds of suspected crimes
uniqueSuspectedCrimes <- length(unique(data$crime.suspected))
print(uniqueSuspectedCrimes)

# Q: What do you think about that?
# A: For citizens I think it's a good thing because they will be charged
# with the precise crime that they are accused of having committed, instead
# of being lumped into large categories. For police and the courts, it must
# make things far more difficult because there's no way anyone can remember
# the intricacies of each crime's precedents. For data, there are probably
# some duplicate codings, misspellings (e.g. "ASSASULT" or "FEKLONY"), and
# a need to place in larger buckets in order to display on charts (such as
# misdemeanors and felonies).

# Q: Is that what you expected?
# A: I would hope so, to have a lot of charges categorized in a democratic
# society.

# Q: Which precinct had the most stops?
# A: 47th Precinct, North Bronx (sorted so index is #1)
# http://www.nyc.gov/html/nypd/html/precincts/precinct_047.shtml
precinctWithMostStops <- which.max(rev(sort(table(data$precinct))))
print(precinctWithMostStops)

# Q: How many were there?
# A: 2,597 stops
mostStopsAtTopPrecinct <- max(table(data$precinct))
print(mostStopsAtTopPrecinct)

# Q: Which precinct had the least stops?
# A: 13th Precinct, PCVST (my neighborhood!)
# http://www.nyc.gov/html/nypd/html/precincts/precinct_013.shtml
precinctWithFewestStops <- which.min(sort(table(data$precinct)))
print(precinctWithFewestStops)

# Q: How many people between 18 and 30 were stopped?
# A: 29,865 stops (inclusive of 18 and 30)
totalBetween18and30 <- sum(table(age[age >= 18 & age <= 30]))
print(totalBetween18and30)

# Notice that there are columns for “frisked”, “searched”, and
# “arrested”.  Every row represents a stop, but not everyone was
# frisked, searched, or arrested.  Use your burgeoning data skills to:

# Q: Find the number of people who were given the full treatment:  frisked,
# searched, and then arrested.
# A: 1,829 stops
fullTreatment <- length(which(frisked == 1 & searched == 1 & arrested == 1))
print(fullTreatment)

# Make a histogram of their ages.
under120 <- data[age < 120 & frisked == 1 & searched == 1 & arrested == 1, "age"]
bins <- seq(0,120,by=1)
hist(under120, bins, xlab="Age", ylab="Frequency of Stops", main="Stops Conducted with Frisk, Search, & Arrest of Those Under Age 120")

# PART 2
# I chose the Google government removal request transparency report dataset
# because it had a small number of columns; I was looking at UN data but
# they often mixed and matched their column data so columns could include
# multiple sets of comparative data, making R work on it more clumsy without
# separating out the data into different files, or specifying columns.
# http://www.google.com/transparencyreport/

# The data is published by Google to show how its traffic and content is
# regulated and influenced by corporations and governments.

# The data is broken down into a log-like format, with different entries
# for different Reasons for takedown requests, country requesting the
# takedown, time period, and Google product affected.

# PREPARATION/MISE EN PLACE
dataGoog <- read.csv("http://www.google.com/transparencyreport/removals/government/google-government-removal-requests-by-product-and-reason.csv", header=TRUE, as.is=TRUE)
attach(dataGoog)

# The Google products which had the most government removal requests were
# YouTube (184), Web Search (107), and Blogger (104).
table(dataGoog$Product)

# The top reasons for government removal requests were Defamation (147),
# Privacy and Security (119), and Other (114).
table(dataGoog$Reason)

# Bar chart of removal request court orders by country.
# Shows US, India, and Brazil with the most total requests by number of court orders, by country.
library(lattice)
barchart(table(dataGoog$Country, dataGoog$Court.Orders), xlab="# of Court Orders", main="Google Transparency Report: # of Court Orders by Country")