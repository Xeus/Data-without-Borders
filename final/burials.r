# Ben Turner
# Data Without Borders, Jake Porway
# Final Project
# NYU-ITP, Fall 2012
#
# Documented at http://blog.benturner.com/2012/12/02/analyzing-us-veteran-gravesite-data/
#
# My project was to take the veterans & beneficiaries
# gravesite data available at data.gov and extract some
# trends.  I ended up pulling in state population data
# from primarily Wikipedia for historical comparisons
# of every 50 years (1800, 1850, 1900, 1950, 2000).

# Gravesite data available free from the USG's data.gov:
# https://explore.data.gov/browse?q=2012&sortBy=relevance&tags=gravesites&page=1
# https://explore.data.gov/catalog/raw?q=gravesites%202012&sortBy=relevance
# had to open files in excel, save, to proper csv format
# dc file on open.gov had to have %20 removed

bs.stats <- data.frame()
state.names <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
bs.stats <- data.frame(cbind(1:51, 0, 0), row.names=state.names)
colnames(bs.stats) <- c('num', 'population', 'deaths.per.capita')

file.path <- '/YOUR_PATH_HERE/'
file.ext <- '.csv'
state.pops <- read.csv(paste(file.path, "census50yrblocks", file.ext, sep=""), h=T, as.is=T)

extract.year <- function(d) {
  for (i in 1:length(d)) {
    if (is.na(d[i]) || is.null(d[i])) {
      d[i] <- ""
    }
    else if (nchar(d[i]) == 4) {
      d[i] <- d[i]
    }
    else {
      tmp <- unlist(strsplit(d[i], "/"))[3]
      if (is.na(tmp)) {
        tmp <- ""
      }
      else if (nchar(tmp) == 2) {
        if (as.numeric(tmp) < 20) {
          tmp <- paste("20", tmp, sep="")
        }
        else {
          tmp <- paste("19", tmp, sep="")
        }
      }
      else if (nchar(tmp) == 1) {
        tmp <- paste("200", tmp, sep="")
      }
      else {
      }
      d[i] <- tmp
    }
  }
  return(d)
}
state.stat.row.names <- c('deaths.2000', 'deaths.1950','deaths.1900','deaths.1850','deaths.1800', 'deaths.per.capita.2000', 'deaths.per.capita.1950', 'deaths.per.capita.1900', 'deaths.per.capita.1850', 'deaths.per.capita.1800', 'pop.army', 'pop.airforce', 'pop.navy', 'pop.marines', 'pop.army.2000', 'pop.army.1950', 'pop.army.1900', 'pop.army.1850', 'pop.army.1800', 'pop.airforce.2000', 'pop.airforce.1950','pop.airforce.1900','pop.airforce.1850','pop.airforce.1800', 'pop.navy.2000', 'pop.navy.1950', 'pop.navy.1900', 'pop.navy.1850', 'pop.navy.1800', 'pop.marines.2000', 'pop.marines.1950', 'pop.marines.1900', 'pop.marines.1850', 'pop.marines.1800','pop.pfc', 'pop.pfc.by.year')
stats <- data.frame(1:36, row.names=state.stat.row.names)

# had to do a unique() to get range of branches
state.stats <- function(bs, p) {
  deaths.2000 <- nrow(bs[bs$death.year==2000,])
  deaths.1950 <- nrow(bs[bs$death.year==1950,])
  deaths.1900 <- nrow(bs[bs$death.year==1900,])
  deaths.1850 <- nrow(bs[bs$death.year==1850,])
  deaths.1800 <- nrow(bs[bs$death.year==1800,])
  deaths.per.capita.2000 <- deaths.2000 / p[1] # 2000
  deaths.per.capita.1950 <- deaths.1950 / p[2] # 1950
  deaths.per.capita.1900 <- deaths.1900 / p[3] # 1900
  deaths.per.capita.1850 <- deaths.1850 / p[4] # 1850
  deaths.per.capita.1800 <- deaths.1800 / p[5] # 1800
  pop.army <- length(grep("ARMY", bs$branch))
  pop.airforce <- length(grep("AIR", bs$branch))
  pop.navy <- length(grep("NAVY", bs$branch))
  pop.marines <- length(grep("MARINE CORPS", bs$branch))
  pop.army.2000 <- length(grep("ARMY", bs$branch[bs$death.year==2000]))
  pop.army.1950 <- length(grep("ARMY", bs$branch[bs$death.year==1950]))
  pop.army.1900 <- length(grep("ARMY", bs$branch[bs$death.year==1900]))
  pop.army.1850 <- length(grep("ARMY", bs$branch[bs$death.year==1850]))
  pop.army.1800 <- length(grep("ARMY", bs$branch[bs$death.year==1800]))
  pop.airforce.2000 <- length(grep("AIR", bs$branch[bs$death.year==2000]))
  pop.airforce.1950 <- length(grep("AIR", bs$branch[bs$death.year==1950]))
  pop.airforce.1900 <- length(grep("AIR", bs$branch[bs$death.year==1900]))
  pop.airforce.1850 <- length(grep("AIR", bs$branch[bs$death.year==1850]))
  pop.airforce.1800 <- length(grep("AIR", bs$branch[bs$death.year==1800]))
  pop.navy.2000 <- length(grep("NAVY", bs$branch[bs$death.year==2000]))
  pop.navy.1950 <- length(grep("NAVY", bs$branch[bs$death.year==1950]))
  pop.navy.1900 <- length(grep("NAVY", bs$branch[bs$death.year==1900]))
  pop.navy.1850 <- length(grep("NAVY", bs$branch[bs$death.year==1850]))
  pop.navy.1800 <- length(grep("NAVY", bs$branch[bs$death.year==1800]))
  pop.marines.2000 <- length(grep("MARINE CORPS", bs$branch[bs$death.year==2000]))
  pop.marines.1950 <- length(grep("MARINE CORPS", bs$branch[bs$death.year==1950]))
  pop.marines.1900 <- length(grep("MARINE CORPS", bs$branch[bs$death.year==1900]))
  pop.marines.1850 <- length(grep("MARINE CORPS", bs$branch[bs$death.year==1850]))
  pop.marines.1800 <- length(grep("MARINE CORPS", bs$branch[bs$death.year==1800]))
  pop.pfc <- length(grep("PFC", bs$rank))
  pop.pfc.by.year <- length(grep("PFC", bs$rank[bs$death.year==2000]))
  return(c(deaths.2000, deaths.1950, deaths.1900, deaths.1850, deaths.1800, deaths.per.capita.2000, deaths.per.capita.1950, deaths.per.capita.1900, deaths.per.capita.1850, deaths.per.capita.1800, pop.army, pop.airforce, pop.navy, pop.marines, pop.army.2000, pop.army.1950, pop.army.1900, pop.army.1850, pop.army.1800, pop.airforce.2000, pop.airforce.1950, pop.airforce.1900, pop.airforce.1850, pop.airforce.1800, pop.navy.2000, pop.navy.1950, pop.navy.1900, pop.navy.1850, pop.navy.1800, pop.marines.2000, pop.marines.1950, pop.marines.1900, pop.marines.1850, pop.marines.1800, pop.pfc, pop.pfc.by.year))
}

loadState <- function(state.file.name) {
  new.data <- read.csv(paste(file.path, state.file.name, file.ext, sep=""), h=T, as.is=T)
  tmp.data <- data.frame(new.data[new.data$relationship=="Veteran (Self)",])
  state.data <- data.frame(tmp.data$d_first_name, tmp.data$d_mid_name, tmp.data$d_last_name, tmp.data$d_birth_date, tmp.data$d_death_date, tmp.data$city, tmp.data$state, tmp.data$zip, tmp.data$relationship, tmp.data$branch, tmp.data$rank, tmp.data$war, stringsAsFactors=FALSE)
  colnames(state.data) <- c("first_name", "mid_name", "last_name", "d_birth_date", "d_death_date", "city", "state", "zip", "relationship", "branch", "rank", "war")
  
  # http://tolstoy.newcastle.edu.au/R/help/06/08/32614.html
  # formats years correctly
  state.data$death.year <- extract.year(state.data$d_death_date)
  
  return(state.data) # return only veterans, no beneficiaries
}

# Load all the states' data into their respective variables.
bs.al <- loadState("ngl_alabama") # AL Oct 12
stats$Alabama <- c(state.stats(bs.al, state.pops$Alabama))
bs.ak <- loadState("ngl_alaska") # AK Oct 12
stats$Alaska <- c(state.stats(bs.ak, state.pops$Alaska))
bs.ar <- loadState("ngl_arkansas") # AR Oct 12
stats$Arkansas <- c(state.stats(bs.ar, state.pops$Arkansas))
bs.az <- loadState("ngl_arizona") # AZ Oct 12
stats$Arizona <- c(state.stats(bs.az, state.pops$Arizona))
bs.ca <- loadState("ngl_california") # CA Oct 12
stats$California <- c(state.stats(bs.ca, state.pops$California))
bs.co <- loadState("ngl_colorado") # CO Oct 12
stats$Colorado <- c(state.stats(bs.co, state.pops$Colorado))
bs.ct <- loadState("ngl_connecticut") # CT Oct 12
stats$Connecticut <- c(state.stats(bs.ct, state.pops$Connecticut))
bs.de <- loadState("ngl_delaware") # DE Oct 12
stats$Delaware <- c(state.stats(bs.de, state.pops$Delaware))
bs.fl <- loadState("ngl_florida") # FL Oct 12
stats$Florida <- c(state.stats(bs.fl, state.pops$Florida))
# bs.foreign <- loadState("ngl_foreign_addresses") # foreign addresses Oct 12
bs.ga <- loadState("ngl_georgia") # GA Oct 12
stats$Georgia <- c(state.stats(bs.ga, state.pops$Georgia))
bs.hi <- loadState("ngl_hawaii") # HI Oct 12
stats$Hawaii <- c(state.stats(bs.hi, state.pops$Hawaii))
bs.id <- loadState("ngl_idaho") # ID Oct 12
stats$Idaho <- c(state.stats(bs.id, state.pops$Idaho))
bs.il <- loadState("ngl_illinois") # IL Oct 12
stats$Illinois <- c(state.stats(bs.il, state.pops$Illinois))
bs.in <- loadState("ngl_indiana") # IN Oct 12
stats$Indiana <- c(state.stats(bs.in, state.pops$Indiana))
bs.ia <- loadState("ngl_iowa") # IA Oct 12
stats$Iowa <- c(state.stats(bs.ia, state.pops$Iowa))
bs.ks <- loadState("ngl_kansas") # KS Oct 12
stats$Kansas <- c(state.stats(bs.ks, state.pops$Kansas))
bs.ky <- loadState("ngl_kentucky") # KY Oct 12
stats$Kentucky <- c(state.stats(bs.ky, state.pops$Kentucky))
bs.la <- loadState("ngl_louisiana") # LA Oct 12
stats$Louisiana <- c(state.stats(bs.la, state.pops$Louisiana))
bs.me <- loadState("ngl_maine") # ME Oct 12
stats$Maine <- c(state.stats(bs.me, state.pops$Maine))
bs.md <- loadState("ngl_maryland") # MD Oct 12
stats$Maryland <- c(state.stats(bs.md, state.pops$Maryland))
bs.ma <- loadState("ngl_massachusetts") # MA Oct 12
stats$Massachusetts <- c(state.stats(bs.ma, state.pops$Massachusetts))
bs.mi <- loadState("ngl_michigan") # MI Oct 12
stats$Michigan <- c(state.stats(bs.mi, state.pops$Michigan))
bs.mn <- loadState("ngl_minnesota") # MN Oct 12
stats$Minnesota <- c(state.stats(bs.mn, state.pops$Minnesota))
bs.ms <- loadState("ngl_mississippi") # MS Oct 12
stats$Mississippi <- c(state.stats(bs.ms, state.pops$Mississippi))
bs.mo <- loadState("ngl_missouri") # MO Oct 12
stats$Missouri <- c(state.stats(bs.mo, state.pops$Missouri))
bs.mt <- loadState("ngl_montana") # MT Oct 12
stats$Montana <- c(state.stats(bs.mt, state.pops$Montana))
bs.ne <- loadState("ngl_nebraska") # NE Oct 12
stats$Nebraska <- c(state.stats(bs.ne, state.pops$Nebraska))
bs.nv <- loadState("ngl_nevada") # NV Oct 12
stats$Nevada <- c(state.stats(bs.nv, state.pops$Nevada))
bs.nh <- loadState("ngl_new_hampshire") # NH Oct 12
stats$New.Hampshire <- c(state.stats(bs.nh, state.pops$New.Hampshire))
bs.nj <- loadState("ngl_new_jersey") # NJ Oct 12
stats$New.Jersey <- c(state.stats(bs.nj, state.pops$New.Jersey))
bs.nm <- loadState("ngl_new_mexico") # NM Oct 12
stats$New.Mexico <- c(state.stats(bs.nm, state.pops$New.Mexico))
bs.ny <- loadState("ngl_new_york") # NY Oct 12
stats$New.York <- c(state.stats(bs.ny, state.pops$New.York))
bs.nc <- loadState("ngl_north_carolina") # NC Oct 12
stats$North.Carolina <- c(state.stats(bs.nc, state.pops$North.Carolina))
bs.nd <- loadState("ngl_north_dakota") # ND Oct 12
stats$North.Dakota <- c(state.stats(bs.nd, state.pops$North.Dakota))
bs.oh <- loadState("ngl_ohio") # OH Oct 12
stats$Ohio <- c(state.stats(bs.oh, state.pops$Ohio))
bs.ok <- loadState("ngl_oklahoma") # OK Oct 12
stats$Oklahoma <- c(state.stats(bs.ok, state.pops$Oklahoma))
bs.or <- loadState("ngl_oregon") # OR Oct 12
stats$Oregon <- c(state.stats(bs.or, state.pops$Oregon))
bs.pa <- loadState("ngl_pennsylvania") # PA Oct 12
stats$Pennsylvania <- c(state.stats(bs.pa, state.pops$Pennsylvania))
bs.ri <- loadState("ngl_rhode_island") # RI Oct 12
stats$Rhode.Island <- c(state.stats(bs.ri, state.pops$Rhode.Island))
bs.sc <- loadState("ngl_south_carolina") # SC Oct 12
stats$South.Carolina <- c(state.stats(bs.sc, state.pops$South.Carolina))
bs.sd <- loadState("ngl_south_dakota") # SD Oct 12
stats$South.Dakota <- c(state.stats(bs.sd, state.pops$South.Dakota))
bs.tn <- loadState("ngl_tennessee") # TN Oct 12
stats$Tennessee <- c(state.stats(bs.tn, state.pops$Tennessee))
bs.tx <- loadState("ngl_texas") # TX Oct 12
stats$Texas <- c(state.stats(bs.tx, state.pops$Texas))
# bs.territories <- loadState("ngl_usa_territories") # USA
#stats$Alabama <- c(state.stats(bs.al, state.pops$Alabama))territories Oct 12
bs.ut <- loadState("ngl_utah") # UT Oct 12
stats$Utah <- c(state.stats(bs.ut, state.pops$Utah))
bs.vt <- loadState("ngl_vermont") # VT Oct 12
stats$Vermont <- c(state.stats(bs.vt, state.pops$Vermont))
bs.va <- loadState("ngl_virginia") # VA Oct 12
stats$Virginia <- c(state.stats(bs.va, state.pops$Virginia))
bs.wa <- loadState("ngl_washington") # WA Oct 12
stats$Washington <- c(state.stats(bs.wa, state.pops$Washington))
bs.dc <- loadState("ngl_washingtondc") # DC Oct 12
stats$District.of.Columbia <- c(state.stats(bs.dc, state.pops$District.of.Columbia))
bs.wv <- loadState("ngl_west_virginia") # WV Oct 12
stats$West.Virginia <- c(state.stats(bs.wv, state.pops$West.Virginia))
bs.wi <- loadState("ngl_wisconsin") # WI Oct 12
stats$Wisconsin <- c(state.stats(bs.wi, state.pops$Wisconsin))
bs.wy <- loadState("ngl_wyoming") # WY Oct 12
stats$Wyoming <- c(state.stats(bs.wy, state.pops$Wyoming))

# Join all the states' data together into 1 large set.
# sans bs.foreign & bs.territories
bs <- rbind(bs.al, bs.ak, bs.az, bs.ar, bs.ca, bs.co, bs.ct, bs.de, bs.fl, bs.ga, bs.hi, bs.id, bs.il, bs.in, bs.ia, bs.ks, bs.ky, bs.la, bs.md, bs.me, bs.ma, bs.mi, bs.mn, bs.ms, bs.mo, bs.mt, bs.nv, bs.ne, bs.nh, bs.nj, bs.nm, bs.ny, bs.nc, bs.nd, bs.oh, bs.ok, bs.or, bs.pa, bs.ri, bs.sc, bs.sd, bs.tn, bs.tx, bs.ut, bs.vt, bs.va, bs.wa, bs.dc, bs.wv, bs.wi, bs.wy)

# Deaths that occurred during various wars (but not as a result of, necessarily):
# Civil War: 1861-1865
nrow(bs[bs$death.year >= 1861 & bs$death.year <= 1865,])
# WW1: 1917-1920
nrow(bs[bs$death.year >= 1917 & bs$death.year <= 1920,])
# WW2: 1940-1945
nrow(bs[bs$death.year >= 1940 & bs$death.year <= 1945,])
# Korean: 1950-1954
nrow(bs[bs$death.year >= 1950 & bs$death.year <= 1954,])
# Vietnam
nrow(bs[bs$death.year >= 1964 & bs$death.year <= 1975,])
# Persian Gulf War
nrow(bs[bs$death.year >= 1990 & bs$death.year <= 1991,])
# OIF
nrow(bs[bs$death.year >= 2003 & bs$death.year <= 2011,])
# OEF
nrow(bs[bs$death.year >= 2001 & bs$death.year <= 2013,])

# TESTING GARBAGE FOLLOWS

# http://tolstoy.newcastle.edu.au/R/help/06/08/32614.html
# formats years correctly
#bs.al$death.year <- extract.year(bs.al$d_death_date)
#bs.dc$death.year <- extract.year(bs.dc$d_death_date)

#allcounties<-data.frame(county=map('county', plot=FALSE)$names) 
#allcounties$group<-c(rep(1:6,513),rep(1,4))[order(c(rep(1:6,513),rep(1,4)))] 
### My colors: 
#classcolors <- rainbow(6) 
#
### 1. If I want to have no borders between counties: 
#map('county',fill=TRUE,col=classcolors[allcounties$group],resolution=0,lty=0,bg 
#    = "transparent") 
#map('state', lwd=1, add=TRUE) 
#
##### 2. If I want to see borders between counties (of a desired 
#color, e.g., gray): 
  ### For line color: 
#  oldpar <- par(fg='gray') # change the default fg (foreground color) to white 
### My US map: 
#map('county',fill=TRUE,col=classcolors[allcounties$group],lty=1,bg = 
#  "transparent") 
#par(oldpar) 

#library(maps)
#map("state")
#data(us.cities)

# tests
# sort(table(bs.dc$rank[bs.dc$war == "PERSIAN GULF"]))

# plot(table(bs$death.year))
# write.table(M,file="Myfile.csv",sep=",",row.names=F)

# stats['deaths.in.year',]$Alabama
# stats.dc <- data.frame(state.stats(bs.dc, state.pops$District.of.Columbia), row.names=state.stat.row.names)

# test <- read.csv("/Users/benturner/Downloads/census50yrblocks.csv", h=T, as.is=T)
# test$Arkansas[test$Year==2000]
# test <- nrow(bs.dc[bs.dc$death_year==1950]) / state.pops$District.of.Columbia[state.pops$Year==1950]