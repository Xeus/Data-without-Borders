# Data Without Borders
# Jake Porway
#
# Victor Ben Turner, vt520@nyu.edu
# Homework: Week #3

# PART 1

# PREPARATION/MISE EN PLACE
# loads stop/frisk data from external CSV
# ### FIX PATH!!!
data <- read.csv("libya_tweets.csv", header=TRUE, as.is=TRUE)
attach(data)

# ANSWERS BELOW
# Q: How many unique users have more than 100000 followers?
# A: 13
more.than.100k.followers = unique(screen_name[followers > 100000])
size.mt100kf = length(more.than.100k.followers)
print(size.mt100kf)

# Q: What are their screen names?
# A: "detikcom", "DonLemonCNN", "HuffingtonPost", "Dputamadre", "WorldRss",
#    "AlMasryAlYoum", "theobscurant", "fadjroeL", "TPO_Hisself", "CAPAMAG",
#    "TwittyAlgeria", "foxandfriends", "PranayGupte"
print(more.than.100k.followers)

# Q: What are the top 3 locations people are from (not counting blanks)?
# A: 1 - USA (34); 2 - Tripoli, Libya (28); 3 - London (20)
top.3.locations = rev(sort(table(location[location != ""])))[1:3]
print(top.3.locations)

# Q: What is the text of the tweet that was retweeted the most times and who tweeted it?
# A: "Here's another Romney Libya fact check. Have fun! http://t.co/YfdrtnF4" - Don Lemon at CNN
most.rt.tweetid = rev(sort(table(retweet)))[1] # 247533773971918848 (8 RTs)
most.rt.text = text[tweet_id == 247533773971918848]
most.rt.author = screen_name[tweet_id == 247533773971918848]
print(most.rt.text)
print(most.rt.author)

# Q: Plot the distribution of the number of people the users are following (don’t 
#    worry about the fact that some people will be counted multiple times – just pretend
#    each row is a different user).  NOTE:  We don’t want to use table() here because we don’t 
#    want to know how many people had exactly 4014 followers, for example, we just want to 
#    see the overall distribution, so use hist() to plot the distribution of “following”.
#    What do you see?
# A: Almost no one is following more than 10,000 people, so while the x-axis of the
#    histogram goes out to 150k, the only really visible bar is from 0-10k.
hist(following)

# Q: Let’s reduce our set to just people with fewer than 5000 followers and look at the
#    histogram again.  What do you see now?
# A: Still, most users follow fewer than 500 people, but there is some distribution of
#    people following 500-1k, 1k-1.5k, 2k-2.5k, etc.  The distribution is more visible.
num.following.less.than.5000 = nrow(data[following < 5000, ]) # 2379
hist(following[following < 5000])

# Q: Have you tried using different breaks?
# A: Yes. For example, If bins are set to approximately every 200, then most users by
#    far follow fewer than 100 people, and about 500 people follow 200-400 people.
bins = nrow(data[following < 5000, ]) / 50
hist(following[following < 5000], breaks = bins)

# Q: Does anything surprise you?
# A: No, normal power-law curve, though there is an uptick in frequency of users
#    who follow about 2k people. Why? Do bots max out at following 2k? Was there
#    an old Twitter max limit for how many people one could follow? Is 2k a
#    magic number?

# PART 2

# Q: Write code to find the 5 most popular words used in the descriptions of our 
#    users.
# A: 1 - "the" (867), 2 - "and" (689), 3 - "of" (581), 4 - "" (511), 5 - "a" (479)
list.desc.words = tolower(unlist(strsplit(description, " ")))
top.5.words = rev(sort(table(list.desc.words)))[1:5]
print(top.5.words)

# Q: Using your skills with %in% and a vector of stopwords, remove the 
#    stopwords from the descriptions and recompute the top 5 words our
#    Twitter users use to describe themselves.
# A: 1 - "news" (233), 2 - "love" (110), 3 - "world" (96), 4 - "follow" (73),
#    5 - "conservative" (69)
stop.words <- read.csv("english.stop.txt", header=TRUE, as.is=TRUE)
filtered.words = list.desc.words[ ! (list.desc.words %in% unlist(stop.words)) ] # both have to be same class!
filtered.words = filtered.words[ ! (filtered.words %in% c("", "&", "-", "|")) ]
top.5.filtered = rev(sort(table(filtered.words)))[1:5]
print(top.5.filtered)

# Q: What do you think of the results? Do you have a sense of what 
#    types of users are most common in our dataset?
# A: The users are probably American (or English-speaking) news junkies
#    (tweets were in English) who follow international news and may be
#    conservative in political leaning.

# PART 3

# Q: Tell me what search terms you used and then teach me something about 
#    your dataset, preferably something you find interesting.
# A: I did a search for "cartel". (results in the git repo) I connected to the
#    Twitter streaming API endpoint pretty late at night so there wasn't much new stuff
#    coming in. I could have probably used a static API request.I ended up getting 207
#    tweets. I was specifically looking for info on Z-40, a leader of Los Zetas, defecting to
#    CDG and also upcoming violence in Tamaulipas between Los Zetas and Knights
#    Templar + Cartel del Golfo. Because it is so dangerous for journalists in
#    Mexico, social media is the chief way in which information is spread, as
#    I learned in my previous security analysis job, so I wanted to see what
#    I could find.

#    I also at one point made a Twitter page that pulled in tweets
#    by neighborhood in Washington, DC. I found that instead of looking for
#    the most high-frequency words, I actually preferred to sort by the least-
#    frequency words because they were often the most unique and packed full
#    of content. But I had to remove a lot of common words, any @handles, and
#    all hashtags. It took a lot of cleaning up but I managed to get a fairly
#    good representation of what each neighborhood was "known" for.

#    So this time I sorted by lowest-frequency and found the regular suspect words:
#    "pelicula", "sicario", and "videos" (the preferred method of online commo
#    by narcos).

#    There was nothing too revealing in my pull, though I figure
#    judging by the hashtags, I could probably use a pull like this to filter
#    a topic and find the hashtags people are actually using to talk.
#    for 

# BONUS

# Q: Now that you’ve been using R for a bit, what’s one thing you wish you knew how to do?
# A: I wish I knew how to crunch my own numbers on the election poll data, the Census, or
#    some datasets on data.gov (boring as they are). Or maybe look at Mexico border violence
#    stats since that's one of my old work hobbies (from DHS).