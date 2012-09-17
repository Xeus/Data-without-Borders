# Data Without Borders
# Jake Porway
#
# Victor Ben Turner, vt520@nyu.edu
# Homework #2

# PART 1

# PREPARATION/MISE EN PLACE
# loads stop/frisk data from external CSV
snf <- read.csv("http://jakeporway.com/teaching/data/snf_2.csv", as.is=TRUE)
attach(snf)

# Write code to return the percentage of people who were frisked for each 
# race.  In other words, count up the number of people who were frisked
# for a given race divided by the number of people of that race stopped.
# NOTE 1:  You can look up what each race code means at 
# http://www.jakeporway.com/teaching/resources/SNF_codes.pdf
# NOTE 2:  The enterprising amongst you might use a loop to do this, but
# since we haven’t technically learned how to do those yet, I’d just type
# each case in by hand.
numRaces = length(unique(race))
totalFrisks = sum(frisked==1)
frisksByRaceSorted = rev(sort(table(race[frisked==1])))

# Q: Which race leads to the highest percentage of frisks?
# A: Black (1), 54% of total frisks
raceMostFrisks = frisksByRaceSorted[1]
highestPercentageFrisksByRace = frisksByRaceSorted[1]/totalFrisks
print(highestPercentageFrisksByRace)

# Q: Which one the lowest?  
# A: American Indian/Native Alaskan (6), 0.3% of total frisks
raceLeastFrisks = frisksByRaceSorted[numRaces]
lowestPercentageFrisksByRace = frisksByRaceSorted[numRaces]/totalFrisks
print(lowestPercentageFrisksByRace)

# Q: Plot the number of times each crime occurs in descending order (we’ve 
#    learned a couple of ways to do this, though using sort(), table() and
#    that new type=parameter to plot() is your best bet).
plot(rev(sort(table(crime.suspected))), type='l', main="Frequency of Offenses, by Crime Suspected", xlab="Suspected Crimes", ylab="# of Times")

# Q: What does this distribution of crimes look like?  In other words, are
#    there an equal number of every kind of crime or are there a few that 
#    dominate?
# A: The plot looks like a power-law graph, where most of the crimes are not committed
#    frequently, but a small concentration of crime types occur most frequently.
#    http://en.wikipedia.org/wiki/Power_law

# Q: Well I’m kind of answering that question for you here – let’s take the top
#    30 suspected crimes and look at those.  If we were to just look at stops
#    where the crime.suspected was one of the top 30 crimes, what percentage of
#    the stops would that cover?
# A: 91.3%
frequencyTop30Crimes = sum(rev(sort(table(crime.suspected)))[1:30])
totalCrimes = length(crime.suspected)
percentageTop30Crimes = frequencyTop30Crimes/totalCrimes
print(percentageTop30Crimes)

# Q: Do you think that’s enough?
# A: The actual result is probably higher since there are similarly named
#    crimes in the top 30, but visually, this still shows a power-law relationship.
#    It might even show that there are approximately a total of 30 official/actual crime
#    categories, if the percentage is approaching 100%, but human error/misclassification
#    is causing the missing percentage/error.

# One last thing we should do before we see what crimes each race is being stopped for.  
# Did you notice something about the top 30 crimes suspected?  Yeah lots of them have 
# similar names!  If you didn’t see this in HW1, notice that the top two crimes are “FEL” 
# and “FELONY”.  Hmm, I wonder if those are the same?  Let’s clean up our 
# crime.suspected variable a little bit by replacing all crimes with just their first 3 letters.  
# That should help consolidate some of our crimes.  But, wait, how do we do that?
#
# Learning in the Homework
# There’s a great function called “substr()”.  As you might imagine, it takes “substrings” of 
# strings.  Like almost all R functions it’s vectorized, so you can just pass it a vector of strings, the position of the first letter you want, the position of the last letter you want, 
# and you’re good to go.  So, for example, to pull the word “awesome” out of this string, 
# you’d do:
#  X <- “R is awesome beyond belief”
#  substr(X, 6, 12)

# Q: Write code to create a variable called “crime.abbv” that consists of just the 
#    first three letters of crime.suspected and show the code to add it to our main data frame.  
#    Now what percentage of the stops do the top 30 crime.abbvs account for?
# A: 98.4%
freqTop30CrimesBy1st3Letters = sum(rev(sort(table(substr(crime.suspected, 1, 3))))[1:30])
percentageTop30CrimesBy1st3Letters = freqTop30CrimesBy1st3Letters / totalCrimes
print(percentageTop30CrimesBy1st3Letters)

# OK, we’re finally ready to see what the top crimes are for each race.
# Q: Write code to show the top 3 crimes each race is suspected of (rev(), 
#    sort(), and table() are your friends here again, but you’ll have to subset the data by race 
#    first).  Huh.  If you do this right, almost all the top 3’s should be the same, but a few are 
#    different.  What are these differences?
# HELP?: How would I sort table(substr(crime.suspected, 1, 3),race) without it losing its indices?
# A: The typical top 3 crimes are felony, misdemeanor, and criminal possession of a weapon.
#    However, for white people, the #3 crime was burglary.  For Asians, #2 was robbery.
#    For Native Americans/Alaskans, #2 was robbery and #3 was grand larceny.
top3CrimesNeg1 = rev(sort(table(substr(crime.suspected, 1, 3)[race==-1]))) # unk
top3Crimes1 = rev(sort(table(substr(crime.suspected, 1, 3)[race==1]))) # black
top3Crimes2 = rev(sort(table(substr(crime.suspected, 1, 3)[race==2]))) # black Hispanic
top3Crimes3 = rev(sort(table(substr(crime.suspected, 1, 3)[race==3]))) # white Hispanic
top3Crimes4 = rev(sort(table(substr(crime.suspected, 1, 3)[race==4]))) # white
top3Crimes5 = rev(sort(table(substr(crime.suspected, 1, 3)[race==5]))) # Asian/Pacific Islander
top3Crimes6 = rev(sort(table(substr(crime.suspected, 1, 3)[race==6]))) # Amer Indian/Nat Alaskan
print(top3CrimesNeg1[1:3])
print(top3Crimes1[1:3])
print(top3Crimes2[1:3])
print(top3Crimes3[1:3])
print(top3Crimes4[1:3])
print(top3Crimes5[1:3])
print(top3Crimes6[1:3])

# PART TWO – A Picture’s Worth 1000 Words

# This one’s a quickie:  We saw in class that we could look at the number of stops 
# happening each day, using our day variable and table().  It was pretty simple to get those 
# counts, plot them, and even color our plot by day of week.  Arguably a more important 
# analysis is what time of day is most common for Stop and Frisks.  Let’s find out!
# NOTE:  I almost forgot – you’ll probably use table() to get the stops counted by 
# hour.  Table() has its own plotting conventions, so you should use 
# as.vector(table(…)) to convert the results to a vector of counts (see lecture 2).  
# That way we can play with the plotting parameters ourselves.

# Q: Let’s create an “hour” variable that tells us what hour of the day each stop
#    happened during and add it to our dataset.  How do we do this?  Well we’ve got a 
#    great column of “time” variables that always has the hour in the same place.  Use 
#    the substr() function we learned about above to strip out the hour, then use 
#    as.numeric() from lecture 2 to convert it to a number.
hour = as.numeric(substr(time, 12, 13))
stopsByHour = table(hour)
stopsByHourSorted = rev(sort(table(hour)))
hoursLength = length(unique(hour))
stopsByHourVector = as.vector(table(as.numeric(substr(time, 12, 13))))

# Q: Create a line plot (i.e. a plot with type=”l”) of the stops by hour.  Which hour of the 
# day has the most stops?
# A: Hard to tell on line chart (easier with bars), but it's 20:00 (8PM), with 4,607.
plot(stopsByHour, type='l', main="Stops By Hour", xlab="Hour", ylab="# of Stops")
hourMostStops = stopsByHourSorted[1]
print(hourMostStops)

# Q: Which hour has the fewest?
# A: 06:00 (6AM) with 323
hourLeastStops = stopsByHourSorted[hoursLength]
print(hourLeastStops)

# Q: Create the same plot but with points instead of lines.  Use a different plotting 
# symbol than the default and color the max point and min points different colors.
pointColor = 5
colorVector = rep(pointColor, 24)
colorVector[which(stopsByHour == max(stopsByHour))] = 6 # most stops
colorVector[which(stopsByHour == min(stopsByHour))] = 7
plot(stopsByHour, type='p', main="Stops By Hour", xlab="Hour", ylab="# of Stops", col=colorVector)

# PART THREE – No More 64oz Containers

# [ THIS ONE’S A BONUS – YOU DON’T HAVE TO DO IT BUT I WROTE IT AND I 
#   LIKED IT SO HERE IT IS.  DIVE IN IF YOU LIKE, OTHERWISE WE’LL GO OVER IT 
#   IN CLASS]

# People are quick to jump on racial and gender discrimination by the NYPD, but one form 
# of discrimination that’s rarely brought up is body discrimination.  Let’s see if heavier 
# people are more or less likely to get arrested (NOTE:  We’re not at the stage where we’re actually proving this or generalizing from this dataset, we just want to see if more 
# or fewer heavier people were arrested than lighter people in our dataset).
# The Stop and Frisk dataset actually contains a “build” variable that specifies the 
# suspect’s build, but let’s get more scientific than that.  Because we have a “height” and 
# “weight” variable, let’s calculate the BMI for each suspect.

# Q: We’ve seen that many of the variables in our dataset have missing values that 
# are either “0” or “999”, and height and weight are no exception.  First create a 
# subset of the data only consisting of “good” weights and heights.  For our 
# purposes, let’s create a subset where the weights are all between 90 and 400, 
# and the heights are greater than 40.
goodSizes = snf[weight > 90 & weight < 400 & height > 40,]

# Q: Add a BMI variable to our dataset, where BMI is computed as 
#    (weight)*703/(height*height).
wVector = goodSizes$weight
hVector = goodSizes$height
bmiVector = (goodSizes$weight)*703/(goodSizes$height*goodSizes$height)
goodSizesWithBMI = cbind(goodSizes, bmi=bmiVector)

# Q: The US Government defines people with BMIs 30 or higher to be obese.  What 
#    percentage of people with BMI’s greater than or equal to 30 who were stopped 
#    were ultimately arrested?
# A: 7.8%
totalStops = nrow(snf)
bmiOver30 = goodSizesWithBMI[goodSizesWithBMI$bmi >= 30,]
percentBMIOver30Arrested = nrow(bmiOver30[bmiOver30$arrested==1,]) / nrow(bmiOver30)
print(percentBMIOver30Arrested)

# Q: What percentage of people with BMI’s less than 30 who were stopped were
#    ultimately arrested?
# A: 5.2%
bmiUnder30 = goodSizesWithBMI[goodSizesWithBMI$bmi < 30,]
percentBMIUnder30Arrested = nrow(bmiUnder30[bmiUnder30$arrested==1,]) / nrow(bmiUnder30)
print(percentBMIUnder30Arrested)

# Q: What do you think of this result?
# A: 7.8% of people with BMIs over 30 were arrested while 5.2% under 30 were arrested.
#    Neither percentage is very large or different from each other, but one would have
#    to do statistical analysis to see how closely the two variables were correlated
#    in order to see the strength of the relationship.