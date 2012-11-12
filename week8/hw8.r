# Data Without Borders
# Jake Porway
#
# Victor Ben Turner, vt520@nyu.edu
# Homework: Week #8

# PREPARATION/MISE EN PLACE
# ### FIX PATH!!!
snf <- read.csv("/Users/benturner/Dropbox/Code/R/Data without Borders/week8/snf_4.csv", header=TRUE, as.is=TRUE)

# Q: Make a “height” column for your data that is the total number of inches tall each 
# person is.
snf$height <- snf$feet*12+snf$inches
attach(snf)

# Q: Plot and describe the variables “height”, “weight”, “period_obs”, and 
# “period_stop”.  Use terms that we learned this lesson – talk about the centers of 
# the distributions, their shapes, and whether they’re skewed or not.  If they’re very
# skewed, try plotting a smaller subset of the data and describing that (e.g. all 
# values less than 50).
mean(height) # 68.58
mean(weight) # 169.28
plot(table(snf$height))
plot(table(snf$weight[snf$weight < 500]))
# A: Height and weight have a normal bell curve distribution, with means of 68.58" and
# 169.28lbs respectively.  Weight seems to have uneven consecutive bars though, which
# might suggest reports rounding weight up or down.  Weight also seems to have a lot
# more outliers, so the plot was limited to weight below 500lbs.  The height data is cleaner.
mean(period_obs) # 2.37
mean(period_stop) # 5.59
median(period_obs) # 1
median(period_stop) # 5
plot(table(snf$period_obs[snf$period_obs < 50]))
plot(table(snf$period_stop[snf$period_stop < 50]))
# The plots for observed/stopped time are easier to view if data over 50 is discarded.
# An overwhelming number of observed's are at a mode of 1 (with a mean of 2.37) and stopped's at
# a mode of 5 (mean at 5.59), suggesting officers chose to round to 1 minute and 5 minutes for
# recording their data.  I would have used mode() if it returned the mode, used the median
# instead even though it returns something slightly different.

# Q: Create a subset of the data where period_obs and period_stop are less than 40.
under40 <- snf[snf$period_obs < 40 & snf$period_stop < 40,]

# Q: Create a jittered() scatterplot of the data.  What do you see?
plot(jitter(under40$period_obs), jitter(under40$period_stop))
for (i in unique(under40$period_obs)) {
  points(i, mean(under40$period_stop[under40$period_obs == i]), col=2, pch=18)
}

# Q: Build a linear model predicting the period_stop variable from period_obs.  What is 
# the slope of your model?
linear.model <- lm(under40$period_stop ~ under40$period_obs)
abline(linear.model)
summary(linear.model)
slope <- linear.model$coefficients[[2]]
# A: Slope: 0.09, r^2 only 0.0056

# Q: Based on your intuition, would you say this is a good model?
# A: Not particularly, little correlation with increase in periods of time; as
# period of observation time increases, period of time stopped doesn't necessarily
# increase and indeed seems to stay flat around the average of 5 minutes.
# Is there reporting error here?

# Q: Using your model, predict how long you expect someone to be stopped if they’re 
# observed for 5 minutes.
intercept <- linear.model$coefficients[[1]]
slope * 5 + intercept
# A: 5.56 minutes

# Q: Using your model, predict how long you expect someone to be stopped if they’re 
# observed for 60 minutes.  Even though we built the model only on data for 
# periods < 40, we do have some data for when people were observed for 60 
# minutes.  Compute the mean for those period_stops where period_obs = 60.
slope * 60 + intercept
# A: 10.64 minutes

# Q: Create a scatterplot of the height and weight variables.  Jitter() or use 
# transparency() so we can see where the bulk of the data lies.
plot(jitter(snf$weight), jitter(snf$height))

# Q: Trim your data to exclude extreme height or weight values.  Write down what 
# threshold you used.
adjusted.hw <- snf[height < 90 & weight < 500,]
# A: Height less than 90", not too many cases above that, but the limit can't be
# lowered much more.  Weight less than 500.

# Q: Run a linear model predicting weight from height.  What is the slope of that 
# model?
linear.model.hw <- lm(adjusted.hw$weight ~ adjusted.hw$height)
summary(linear.model.hw)
slope.hw <- linear.model.hw$coefficients[[2]]
intercept.hw <- linear.model.hw$coefficients[[1]]
# A: Slope: 4.29

# Q: How much do you expect someone who’s 6’ 0” to weigh?
slope.hw * (6 * 12 + 0) + intercept.hw
# A: 183.43lbs