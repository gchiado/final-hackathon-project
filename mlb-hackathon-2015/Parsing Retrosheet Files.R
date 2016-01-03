library(retrosheet)
setwd("~/Documents/Retrosheets")
 
source("parse.retrosheet2.pbp.R")
fields <- read.csv("~/Documents/Retrosheets/download.folder/unzipped/fields.csv")
### 2015 event files
parse.retrosheet2.pbp(2015)
all2015 <- read.csv("~/Documents/Retrosheets/download.folder/unzipped/all2015.csv", header=FALSE)
names(all2015) <- fields[, "Header"]
### 2014 event files
parse.retrosheet2.pbp(2014)
all2014 <- read.csv("~/Documents/Retrosheets/download.folder/unzipped/all2014.csv", header=FALSE)
names(all2014) <- fields[, "Header"]
### 2013 event files
parse.retrosheet2.pbp(2013)
all2013 <- read.csv("~/Documents/Retrosheets/download.folder/unzipped/all2013.csv", header=FALSE)
names(all2013) <- fields[, "Header"]

setwd("~/Documents/mlb-hackathon-2015")

write.csv(all2013, file = "all2013.csv")
write.csv(all2014, file = "all2014.csv")
write.csv(all2015, file = "all2015.csv")