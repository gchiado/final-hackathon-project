##############################################################################################
##############################  HACKATHON PROJECT ############################################
##############################################################################################

##### Pitch-by-pitch data provided for Hackathon Project - variable names #####

# [1] "seasonYear"              "gameString"              "gameDate"                "gameType"                "visitor"                
# [6] "home"                    "visitingTeamFinalRuns"   "homeTeamFinalRuns"       "inning"                  "side"                   
# [11] "batterId"                "batter"                  "batterHand"              "pitcherId"               "pitcher"                
# [16] "pitcherHand"             "catcherId"               "catcher"                 "timesFaced"              "batterPosition"         
# [21] "balls"                   "strikes"                 "outs"                    "manOnFirst"              "manOnSecond"            
# [26] "manOnThird"              "endManOnFirst"           "endManOnSecond"          "endManOnThird"           "visitingTeamCurrentRuns"
# [31] "homeTeamCurrentRuns"     "pitchResult"             "pitchType"               "releaseVelocity"         "spinRate"               
# [36] "spinDir"                 "px"                      "pz"                      "szt"                     "szb"                    
# [41] "x0"                      "y0"                      "z0"                      "vx0"                     "vy0"                    
# [46] "vz0"                     "ax"                      "ay"                      "az"                      "paResult"               
# [51] "runsHome"                "battedBallType"          "battedBallAngle"         "battedBallDistance"      "atbatDesc" 



setwd("~/Documents/final-hackathon-project/mlb-hackathon-2015")
pitchdata2015 <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/2015.csv")
pitchdata2014 <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/2014.csv")
pitchdata2013 <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/2013.csv")

#######################################################################################################
######################## DATA CLEANING - Adding new variables to data #################################
########################     Run separately for all three datasets    #################################
#######################################################################################################

##### Season Sequence #####
pitchdata2015$season_seq = 1
for(i in 2:nrow(pitchdata2015)){
  pitchdata2015[i, "season_seq"] = pitchdata2015[i-1, "season_seq"]+1
}

#### Reorder by gameDate and pitcher_id ####
pitchdata2015 <- pitchdata2015[order(pitchdata2015$gameDate, pitchdata2015$pitcherId), ]

##### BF caluclation ######
pitchdata2015$BF = 1
for(i in 2:nrow(pitchdata2015)){
  if(pitchdata2015[i, "pitcherId"] != pitchdata2015[i-1, "pitcherId"]){
    pitchdata2015[i, "BF"] = 1
  }else
    if(pitchdata2015[i, "pitcherId"] == pitchdata2015[i-1,"pitcherId"] & pitchdata2015[i, "batterId"] != pitchdata2015[i-1,"batterId"] |
       pitchdata2015[i, "pitcherId"] == pitchdata2015[i-1,"pitcherId"] & pitchdata2015[i, "inning"] != pitchdata2015[i-1,"inning"]){
      pitchdata2015[i, "BF"] = pitchdata2015[i-1,"BF"]+1
    }else
    {pitchdata2015[i, "BF"] = pitchdata2015[i-1, "BF"]}
}

#### Calculate number of times through the order #########
pitchdata2015$TTO = with(pitchdata2015,ceiling(BF/9))

#### Number of pitches thrown in game #### 
pitchdata2015$game_pitches = 1
for(i in 2:nrow(pitchdata2015)){
  if(pitchdata2015[i, "pitcherId"] == pitchdata2015[i-1, "pitcherId"]){
    pitchdata2015[i, "game_pitches"] = pitchdata2015[i-1, "game_pitches"]+1
  }else
  {pitchdata2015[i, "game_pitches"] = 1}
}

#####  Last pitch of the at bat #########
pitchdata2015$last_pitch_ab = FALSE
for(i in 1:nrow(pitchdata2015)){
  if(pitchdata2015[i, "paResult"] == "" | pitchdata2015[i, "inning"] != pitchdata2015[i+1, "inning"]){
    pitchdata2015[i, "last_pitch_ab"] = FALSE
  }else
    pitchdata2015[i, "last_pitch_ab"] = TRUE
}

#### First pitch of the at bat ######
pitchdata2015$first_pitch_ab = TRUE
for(i in 2:nrow(pitchdata2015)){
  if(pitchdata2015[i-1, "paResult"] == "" | pitchdata2015[i, "strikes"] != 0 | pitchdata2015[i, "balls"] != 0){
    pitchdata2015[i, "first_pitch_ab"] = FALSE
  }else
    pitchdata2015[i, "first_pitch_ab"] = TRUE
}

#### Add the pitch number of the AB #########
pitchdata2015$pitch_of_ab = as.integer(1)
for(i in 2:nrow(pitchdata2015)){
  if(pitchdata2015[i-1, "last_pitch_ab"] | 
     pitchdata2015[i, "batterId"] != pitchdata2015[i-1, "batterId"]){
    pitchdata2015[i, "pitch_of_ab"] = 1
  }else
    pitchdata2015[i, "pitch_of_ab"] = pitchdata2015[i-1, "pitch_of_ab"] + 1
}

##### Count #######
pitchdata2015$count = paste(pitchdata2015$balls, pitchdata2015$strikes, sep = "-")
pitchdata2015$count = as.factor(pitchdata2015$count)

###### Count after the pitch ######
pitchdata2015$count_after = NA
for(i in 1:nrow(pitchdata2015)){
  if(pitchdata2015[i, "last_pitch_ab"] == FALSE){
    pitchdata2015[i, "count_after"] = as.character(pitchdata2015[i+1, "count"])
  }else
    pitchdata2015[i, "count_after"] = NA
}
pitchdata2015$count_after = as.factor(pitchdata2015$count_after)

### Revert three-out pitches back to previous number of outs ### 
pitchdata2015 <- pitchdata2015[order(pitchdata2015$season_seq), ]
for(i in 1:nrow(pitchdata2015)){
  if(pitchdata2015[i, "outs"] == 3){
    pitchdata2015[i, "outs"] = pitchdata2015[i-1, "outs"]
  }
}

#### Create base-out state variable ####
RUNNER1 <- ifelse(as.character(pitchdata2015[ ,"manOnFirst"]) == "false", 0, 1)
RUNNER2 <- ifelse(as.character(pitchdata2015[ ,"manOnSecond"]) == "false", 0, 1)
RUNNER3 <- ifelse(as.character(pitchdata2015[ ,"manOnThird"]) == "false", 0, 1)
# Create get.state() function to combine the runner indicators and the # of outs
get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}
pitchdata2015$base_out_state <- get.state(RUNNER1, RUNNER2, RUNNER3, pitchdata2015$outs)
pitchdata2015$base_out_state = as.factor(pitchdata2015$base_out_state)

### Add the event outs of that pitch
pitchdata2015$event_outs = ifelse(pitchdata2015$paResult == "IP_OUT" | pitchdata2015$paResult == "K" | pitchdata2015$paResult == "FC" |
                                    pitchdata2015$paResult == "BI" | pitchdata2015$paResult == "NO_PLAY", 1, 0)
pitchdata2015$event_outs = ifelse(pitchdata2015$paResult == "DP", 2, as.numeric(pitchdata2015$event_outs))
pitchdata2015$event_outs = ifelse(pitchdata2015$paResult == "TP", 3, as.numeric(pitchdata2015$event_outs))

#### Make runScoredOnPitch numeric version of runsHome  ####
pitchdata2015$runsScoredOnPitch = ifelse(is.na(pitchdata2015$runsHome), 0, as.numeric(pitchdata2015$runsHome))

##### Add flags for all possible paResults ####
pitchdata2015$single = ifelse(pitchdata2015$paResult == "S", 1, 0)
pitchdata2015$double = ifelse(pitchdata2015$paResult == "D", 1, 0)
pitchdata2015$triple = ifelse(pitchdata2015$paResult == "T", 1, 0)
pitchdata2015$homerun  = ifelse(pitchdata2015$paResult == "HR", 1, 0)
pitchdata2015$walk = ifelse(pitchdata2015$paResult == "BB" | pitchdata2015$paResult == "IBB", 1, 0)
pitchdata2015$strikeout = ifelse(pitchdata2015$paResult == "K", 1, 0)
pitchdata2015$hitbypitch = ifelse(pitchdata2015$paResult == "HBP", 1, 0)
pitchdata2015$hit = ifelse(pitchdata2015$paResult == "S" | pitchdata2015$paResult == "D" | pitchdata2015$paResult == "T" |
                             pitchdata2015$paResult == "HR", 1, 0)
pitchdata2015$sacfly = ifelse(pitchdata2015$paResult == "SF", 1, 0)
pitchdata2015$sacbunt = ifelse(pitchdata2015$paResult == "SH", 1, 0)
pitchdata2015$plateapp = ifelse(pitchdata2015$paResult != "" & pitchdata2015$paResult != "NO_PLAY", 1, 0)
pitchdata2015$atbat = ifelse(pitchdata2015$paResult != "" & pitchdata2015$paResult != "BB" & pitchdata2015$paResult != "SF" &
                               pitchdata2015$paResult != "IBB" & pitchdata2015$paResult != "HBP" & pitchdata2015$paResult != "SH" &
                               pitchdata2015$paResult != "CI" & pitchdata2015$paResult != "FI" &
                               pitchdata2015$paResult != "NO_PLAY", 1, 0)
pitchdata2015$out = ifelse(pitchdata2015$paResult == "IP_OUT" | pitchdata2015$paResult == "K" | pitchdata2015$paResult == "FC" |
                             pitchdata2015$paResult == "BI" |  pitchdata2015$paResult == "NO_PLAY", 1, 0)
pitchdata2015$doubleplay = ifelse(pitchdata2015$paResult == "DP", 1, 0)
pitchdata2015$tripleplay = ifelse(pitchdata2015$paResult == "TP", 1, 0)
pitchdata2015$intentbb = ifelse(pitchdata2015$paResult == "IBB", 1, 0)
pitchdata2015$unintentbb = ifelse(pitchdata2015$paResult == "BB", 1, 0)

##### Run Expectancy Matix for paResults and counts #####
library(plyr)
library(dplyr)
data2015 <- subset(pitchdata2015)
##
data2015$RUNS <- with(data2015, visitingTeamCurrentRuns + homeTeamCurrentRuns)
data2015$HALF.INNING <- with(data2015, paste(gameString, inning, side))
data2015$RUNS.SCORED <- with(data2015, runsScoredOnPitch)
RUNS.SCORED.INNING <- aggregate(data2015$RUNS.SCORED, 
                                list(HALF.INNING=data2015$HALF.INNING), sum)
RUNS.SCORED.START <- aggregate(data2015$RUNS, 
                               list(HALF.INNING=data2015$HALF.INNING), "[", 1)
MAX <- data.frame(HALF.INNING = RUNS.SCORED.START$HALF.INNING)
MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data2015 <- merge(data2015, MAX)
N <- ncol(data2015)
names(data2015)[N] <- "MAX.RUNS"
data2015$RUNS.ROI <- with(data2015, MAX.RUNS - RUNS)
### Creating the Matrix
RUNNER1 <- ifelse(as.character(data2015[ ,"manOnFirst"]) == "false", 0, 1)
RUNNER2 <- ifelse(as.character(data2015[ ,"manOnSecond"]) == "false", 0, 1)
RUNNER3 <- ifelse(as.character(data2015[ ,"manOnThird"]) == "false", 0, 1)
get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}
data2015$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2015$outs)
NRUNNER1 <- with(data2015, as.numeric(endManOnFirst == "true"))
NRUNNER2 <- with(data2015, as.numeric(endManOnSecond == "true"))
NRUNNER3 <- with(data2015, as.numeric(endManOnThird == "true"))
NOUTS <- with(data2015, outs + out + (2*doubleplay) + (3*tripleplay))
data2015$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
data2015 <- subset(data2015, (STATE != NEW.STATE) | (RUNS.SCORED > 0))
data.outs <- ddply(data2015, .(HALF.INNING), summarize, Outs.Inning=sum(event_outs))
data2015 <- merge(data2015, data.outs)
data2015C <- subset(data2015, Outs.Inning == 3)
RUNS <- with(data2015C, aggregate(RUNS.ROI, list(STATE), mean))
RUNS$Outs <- substr(RUNS$Group, 5, 5)
RUNS <- RUNS[order(RUNS$Outs), ]
RUNS.out <- matrix(round(RUNS$x, 3), 8, 3)
dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")
RUNS.out
RUNS.POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), 32, 1)
dimnames(RUNS.POTENTIAL)[[1]]<-c(RUNS$Group, "000 3", "001 3", "010 3", "011 3", "100 3", "101 3", "110 3", "111 3")
data2015$NEW.STATE = ifelse(data2015$NEW.STATE == "000 4", "000 3", data2015$NEW.STATE)
data2015$RUNS.STATE <- RUNS.POTENTIAL[data2015$STATE, ]
data2015$RUNS.NEW.STATE <- RUNS.POTENTIAL[data2015$NEW.STATE, ]
data2015$RUNS.VALUE <- data2015$RUNS.NEW.STATE - data2015$RUNS.STATE + data2015$RUNS.SCORED
### Calculate the run value of all paResults ###
d.homerun <- subset(data2015, paResult == "HR")
mean.HR <- mean(d.homerun$RUNS.VALUE)
d.single <- subset(data2015, paResult =="S")
mean.single <- mean(d.single$RUNS.VALUE)
d.double <- subset(data2015, paResult =="D")
mean.double <- mean(d.double$RUNS.VALUE)
d.triple <- subset(data2015, paResult =="T")
mean.triple <- mean(d.triple$RUNS.VALUE)
d.walk <- subset(data2015, paResult == "BB")
mean.walk <- mean(d.walk$RUNS.VALUE)
d.ibb <- subset(data2015, paResult == "IBB")
mean.ibb <- mean(d.ibb$RUNS.VALUE)
d.hbp <- subset(data2015, paResult == "HBP")
mean.hbp <- mean(d.hbp$RUNS.VALUE)
d.ip_out <- subset(data2015, paResult == "IP_OUT")
mean.ip_out <- mean(d.ip_out$RUNS.VALUE)
d.strikeout <- subset(data2015, paResult == "K")
mean.strikeout <- mean(d.strikeout$RUNS.VALUE)
d.fc <- subset(data2015, paResult == "FC")
mean.fc <- mean(d.fc$RUNS.VALUE)
d.dp <- subset(data2015, paResult == "DP")
mean.dp <- mean(d.dp$RUNS.VALUE)
d.tp <- subset(data2015, paResult == "TP")
mean.tp <- mean(d.tp$RUNS.VALUE)
d.sh <- subset(data2015, paResult == "SH")
mean.sh <- mean(d.sh$RUNS.VALUE)
d.sf <- subset(data2015, paResult == "SF")
mean.sf <- mean(d.sf$RUNS.VALUE)
d.roe <- subset(data2015, paResult == "ROE")
mean.roe <- mean(d.roe$RUNS.VALUE)
d.sh_roe <- subset(data2015, paResult == "SH_ROE")
mean.sh_roe <- mean(d.sh_roe$RUNS.VALUE)
d.sf_roe <- subset(data2015, paResult == "SF_ROE")
mean.sf_roe <- mean(d.sf_roe$RUNS.VALUE)
d.bi <- subset(data2015, paResult == "BI")
mean.bi <- mean(d.bi$RUNS.VALUE)
d.ci <- subset(data2015, paResult == "CI")
mean.ci <- mean(d.ci$RUNS.VALUE)
d.fi <- subset(data2015, paResult == "FI")
mean.fi <- mean(d.fi$RUNS.VALUE)
d.no_play <- subset(data2015, paResult == "NO_PLAY")
mean.no_play <- mean(d.no_play$RUNS.VALUE)
#### Put into dataframe
pa_result = c("HR", "S", "D", "T", "BB", "IBB", "HBP", "IP_OUT", "K", "FC",
              "DP", "TP", "SH", "SF", "ROE", "SH_ROE", "SF_ROE", "BI", "CI", "FI", "NO_PLAY")
pa_result_value = c(mean.HR, mean.single, mean.double, mean.triple, mean.walk, mean.ibb, mean.hbp, mean.ip_out, 
                    mean.strikeout, mean.fc, mean.dp, mean.tp, mean.sh, mean.sf, mean.roe, mean.sh_roe,
                    mean.sf_roe, mean.bi, mean.ci, mean.fi, mean.no_play)
resultvalues15 = data.frame(pa_result, pa_result_value)
resultvalues15$year = 2015
resultvalues15$pa_result_value = round(resultvalues15$pa_result_value, 3)
write.csv(resultvalues15, file = "resultvalues15.csv")
### Add PA result values to pitchdata
pitchdata2015$pa_result_value = resultvalues15$pa_result_value[match(pitchdata2015$paResult, resultvalues15$pa_result)]
### Calculate the run values through all counts using Retrosheet data
all2015 <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/all2015.csv")
data2015_2.0 <- subset(all2015)
data2015_2.0$RUNS <- with(data2015_2.0, AWAY_SCORE_CT + HOME_SCORE_CT)
data2015_2.0$HALF.INNING <- with(data2015_2.0, paste(GAME_ID, INN_CT, BAT_HOME_ID))
data2015_2.0$RUNS.SCORED <- with(data2015_2.0, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
                                   (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
RUNS.SCORED.INNING <- aggregate(data2015_2.0$RUNS.SCORED, 
                                list(HALF.INNING=data2015_2.0$HALF.INNING), sum)
RUNS.SCORED.START <- aggregate(data2015_2.0$RUNS, 
                               list(HALF.INNING=data2015_2.0$HALF.INNING), "[", 1)
MAX <- data.frame(HALF.INNING = RUNS.SCORED.START$HALF.INNING)
MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data2015_2.0 <- merge(data2015_2.0, MAX)
N <- ncol(data2015_2.0)
names(data2015_2.0)[N] <- "MAX.RUNS"
data2015_2.0$RUNS.ROI <- with(data2015_2.0, MAX.RUNS - RUNS)
### Creating the Matrix
RUNNER1 <- ifelse(as.character(data2015_2.0[ ,"BASE1_RUN_ID"]) == "", 0, 1)
RUNNER2 <- ifelse(as.character(data2015_2.0[ ,"BASE2_RUN_ID"]) == "", 0, 1)
RUNNER3 <- ifelse(as.character(data2015_2.0[ ,"BASE3_RUN_ID"]) == "", 0, 1)
get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}
data2015_2.0$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2015_2.0$OUTS_CT)
NRUNNER1 <- with(data2015_2.0, as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1))
NRUNNER2 <- with(data2015_2.0, as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | BAT_DEST_ID == 2))
NRUNNER3 <- with(data2015_2.0, as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 | RUN3_DEST_ID == 3 | 
                                            BAT_DEST_ID == 3))
NOUTS <- with(data2015_2.0, OUTS_CT + EVENT_OUTS_CT)
data2015_2.0$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
data2015_2.0 <- subset(data2015_2.0, (STATE != NEW.STATE) | (RUNS.SCORED > 0))
data.outs <- ddply(data2015_2.0, .(HALF.INNING), summarize, Outs.Inning=sum(EVENT_OUTS_CT))
data2015_2.0 <- merge(data2015_2.0, data.outs)
data2015_2.0C <- subset(data2015_2.0, Outs.Inning == 3)
RUNS <- with(data2015_2.0C, aggregate(RUNS.ROI, list(STATE), mean))
RUNS$Outs <- substr(RUNS$Group, 5, 5)
RUNS <- RUNS[order(RUNS$Outs), ]
RUNS.out <- matrix(round(RUNS$x, 3), 8, 3)
dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")
RUNS.out
RUNS.POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), 32, 1)
dimnames(RUNS.POTENTIAL)[[1]]<-c(RUNS$Group, "000 3", "001 3", "010 3", "011 3", "100 3", "101 3", "110 3", "111 3")
data2015_2.0$RUNS.STATE <- RUNS.POTENTIAL[data2015_2.0$STATE, ]
data2015_2.0$RUNS.NEW.STATE <- RUNS.POTENTIAL[data2015_2.0$NEW.STATE, ]
data2015_2.0$RUNS.VALUE <- data2015_2.0$RUNS.NEW.STATE - data2015_2.0$RUNS.STATE + data2015_2.0$RUNS.SCORED
## Average run value through each unique count
retro2 = subset(data2015_2.0)
retro2$pseq <- gsub("[.>123N+*]", "", retro2$PITCH_SEQ_TX)
retro2$c00 <- TRUE
retro2$c10 <- grepl("^[BIPV]", retro2$pseq)
retro2$c01 <- grepl("^[CFKLMOQRST]", retro2$pseq)
retro2$c20 <- grepl("^[BIPV]{2}", retro2$pseq)
retro2$c30 <- grepl("^[BIPV]{3}", retro2$pseq)
retro2$c02 <- grepl("^[CFKLMOQRST]{2}", retro2$pseq)
retro2$c11 <- grepl("^[CFKLMOQRST][BIPV]|[BIPV][CFKLMOQRST]", retro2$pseq)
retro2$c21 <- grepl("^[CFKLMOQRST][BIPV][BIPV]|[BIPV][CFKLMOQRST][BIPV]|[BIPV][BIPV][CFKLMOQRST]", retro2$pseq)
retro2$c31 <- grepl("^[CFKLMOQRST][BIPV][BIPV][BIPV]|[BIPV][CFKLMOQRST][BIPV][BIPV]|[BIPV][BIPV][CFKLMOQRST][BIPV]|
                    [BIPV][BIPV][BIPV][CFKLMOQRST]", retro2$pseq)
retro2$c12 <- grepl("^[CFKLMOQRST][CFKLMOQRST][FR]*[BIPV]|[BIPV][CFKLMOQRST][CFKLMOQRST]|
                    [CFKLMOQRST][BIPV][CFKLMOQRST]", retro2$pseq)
retro2$c22 <- grepl("^[CFKLMOQRST][CFKLMOQRST][FR]*[BIPV][FR}*[BIPV]|[BIPV][BIPV][CFKLMOQRST][CFKLMOQRST]|
                    [BIPV][CFKLMOQRST][BIPV][CFKLMOQRST]|[BIPV][CFKLMOQRST][CFKLMOQRST][FR]*[BIVP]|
                    [CFKLMOQRST][BIPV][CFKLMOQRST][FR]*[BIVP]|[CFKLMOQRST][BIPV][BIPV][CFKLMOQRST]", retro2$pseq)
retro2$c32 <- grepl("^[CFKLMOQRST]*[BIPV][CFKLMOQRST]*[BIPV][CFKLMOQRST]*[BIPV]", retro2$pseq) & grepl("^[BIPV]*[CFKLMOQRST][BIPV]*[CFKLMOQRST]", retro2$pseq)
retro01 <- subset(retro2, c01==TRUE)
mean.retro01 <- mean(retro01$RUNS.VALUE)
retro10 <- subset(retro2, c10==TRUE)
mean.retro10 <- mean(retro10$RUNS.VALUE)
retro20 <- subset(retro2, c20==TRUE)
mean.retro20 <- mean(retro20$RUNS.VALUE)
retro30 <- subset(retro2, c30==TRUE)
mean.retro30 <- mean(retro30$RUNS.VALUE)
retro02 <- subset(retro2, c02==TRUE)
mean.retro02 <- mean(retro02$RUNS.VALUE)
retro11 <- subset(retro2, c11==TRUE)
mean.retro11 <- mean(retro11$RUNS.VALUE)
retro21 <- subset(retro2, c21==TRUE)
mean.retro21 <- mean(retro21$RUNS.VALUE)
retro31 <- subset(retro2, c31==TRUE)
mean.retro31 <- mean(retro31$RUNS.VALUE)
retro12 <- subset(retro2, c12==TRUE)
mean.retro12 <- mean(retro12$RUNS.VALUE)
retro22 <- subset(retro2, c22==TRUE)
mean.retro22 <- mean(retro22$RUNS.VALUE)
retro32 <- subset(retro2, c32==TRUE)
mean.retro32 <- mean(retro32$RUNS.VALUE)
### Put count value into dataframe
count = c("0-0","0-1","1-0","2-0","3-0","0-2","1-1","2-1","3-1","1-2","2-2","3-2")
count.value = c(0, mean.retro01, mean.retro10, mean.retro20, mean.retro30, mean.retro02, mean.retro11,
                mean.retro21, mean.retro31, mean.retro12, mean.retro22, mean.retro32)
countvalue15 = data.frame(count, count.value)
countvalue15$year = 2015
countvalue15$count.value = round(countvalue15$count.value, 3)
### Add PA result values and count values to pitchdata
pitchdata2015$pa_result_value = resultvalues15$pa_result_value[match(pitchdata2015$paResult, resultvalues15$pa_result)]
pitchdata2015$start_count_value = countvalue15$count.value[match(pitchdata2015$count, countvalue15$count)]
pitchdata2015$end_count_value = countvalue15$count.value[match(pitchdata2015$count_after, countvalue15$count)]
### Final run value of the pitch
pitchdata2015$final_run_value = ifelse(is.na(pitchdata2015$pa_result_value), 
                                       pitchdata2015$end_count_value - pitchdata2015$start_count_value, 
                                       pitchdata2015$pa_result_value - pitchdata2015$start_count_value)
### Merge run values of paResults and count
resultvalues = rbind(resultvalues15, resultvalues14)
resultvalues = rbind(resultvalues, resultvalues13)
write.csv(resultvalues, file = "resultvalues.csv")
countvalue = rbind(countvalue15, countvalue14)
countvalue = rbind(countvalue, countvalue13)
write.csv(countvalue, file = "countvalue.csv")

#### Add game starting pitcher for ISSP ####
starters15 <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/starters15.csv")
pitchdata2015$string = paste(pitchdata2015$gameString, pitchdata2015$side, sep="")
pitchdata2015$starterId = starters15$pitcherId[match(pitchdata2015$string, starters15$new_string)]
pitchdata2015$starter = starters15$pitcher[match(pitchdata2015$string, starters15$new_string)]
#### Add ISSP for pitcher role ####
pitchdata2015$ISSP = ifelse(pitchdata2015$pitcherId == pitchdata2015$starterId, TRUE, FALSE)

##### Add Baseball Prospectus IDs for additional season long variables #####
master_id <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/master_id.csv")
pitchdata2015$pitcher_bp_id = master_id$bp_id[match(pitchdata2015$pitcherId, master_id$mlb_id)]

##### Add seasonIP and SP_IP_rate #####
bpstats_2015 <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/bpstats_2015.csv", header=FALSE, comment.char="#")
pitchdata2015$seasonIP = bpstats_2015$V12[match(pitchdata2015$pitcher_bp_id, bpstats_2015$V3)]
pitchdata2015$SP_IP_rate = bpstats_2015$V15[match(pitchdata2015$pitcher_bp_id, bpstats_2015$V3)]

##### Add month variable #####
pitchdata2015$month = as.integer(substr(as.character(pitchdata2015$gameDate), 6, 7))

##### Pitch was within Rulebook K-zone  ######
pitchdata2015$in_kzone = ifelse(pitchdata2015$px >= -0.833 & pitchdata2015$px <= 0.833 & pitchdata2015$pz >= pitchdata2015$szb & 
                                  pitchdata2015$pz <= pitchdata2015$szt, TRUE, FALSE)
pitchdata2015$no_kzone = ifelse(pitchdata2015$in_kzone == TRUE, FALSE, TRUE)

##### Add standard run environment park factors by batter handedness #####
pf_15 <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/pf_15.csv")
pitchdata2015$batterHand = ifelse(pitchdata2015$batterHand=="R", "RHB", "LHB")
pitchdata2015 = merge(pitchdata2015, pf_15, by.x=c("home", "batterHand"), by.y=c("TEAM","SIDE"))
pitchdata2015 <- pitchdata2015[order(pitchdata2015$season_seq), ]

##### Add a reverse PX for the pitcher's point of view in Tableau ######
pitchdata2015$rev_px = pitchdata2015$px * -1

#####  Called Strike #####
pitchdata2015$cs = ifelse(pitchdata2015$pitchResult == "SL", TRUE, FALSE)

#######  Was the pitch taken  #######
pitchdata2015$pitchtaken = ifelse(pitchdata2015$pitchResult == "SL" | pitchdata2015$pitchResult == "B" | pitchdata2015$pitchResult == "BID" | 
                                    pitchdata2015$pitchResult == "HBP" | pitchdata2015$pitchResult == "IB" | 
                                    pitchdata2015$pitchResult == "PO", TRUE, FALSE)

#####  Was the pitch swung at ######
pitchdata2015$swing = ifelse(pitchdata2015$pitchResult == "SS" | pitchdata2015$pitchResult == "F" | pitchdata2015$pitchResult == "FT" | 
                               pitchdata2015$pitchResult == "FB" | pitchdata2015$pitchResult == "MB" | pitchdata2015$pitchResult == "IP" | 
                               pitchdata2015$pitchResult == "CI", TRUE, FALSE)

######  Was the pitch swung at and missed ########
pitchdata2015$swing_and_miss = ifelse(pitchdata2015$pitchResult == "SS" | pitchdata2015$pitchResult == "MB", TRUE, FALSE)

######  Made contact with the pitch ######
pitchdata2015$contact = ifelse(pitchdata2015$pitchResult == "F" | pitchdata2015$pitchResult == "FT" | 
                                 pitchdata2015$pitchResult == "FB" | pitchdata2015$pitchResult == "IP", TRUE, FALSE)

######  Ball in Play ######
pitchdata2015$bip = ifelse(pitchdata2015$pitchResult == "IP", TRUE, FALSE)

##### Pitching team and batting team #####
pitchdata2015$pit_team = ifelse(pitchdata2015$side == "T", as.character(pitchdata2015$visitor), as.character(pitchdata2015$home))
pitchdata2015$bat_team = ifelse(pitchdata2015$side == "T", as.character(pitchdata2015$home), as.character(pitchdata2015$visitor))

#### Add batted ball spray ####
pitchdata2015$battedBallSpray = ifelse(pitchdata2015$batterHand == "RHB" & pitchdata2015$battedBallAngle >= -55 &
                                       pitchdata2015$battedBallAngle <=-15, "Pull", "")
pitchdata2015$battedBallSpray = ifelse(pitchdata2015$batterHand == "RHB" & pitchdata2015$battedBallAngle > -15 &
                                       pitchdata2015$battedBallAngle < 15, "Center", as.character(pitchdata2015$battedBallSpray))
pitchdata2015$battedBallSpray = ifelse(pitchdata2015$batterHand == "RHB" & pitchdata2015$battedBallAngle >= 15 &
                                       pitchdata2015$battedBallAngle <=55, "Opposite", as.character(pitchdata2015$battedBallSpray))
pitchdata2015$battedBallSpray = ifelse(pitchdata2015$batterHand == "LHB" & pitchdata2015$battedBallAngle >= -55 &
                                         pitchdata2015$battedBallAngle <=-15, "Opposite", as.character(pitchdata2015$battedBallSpray))
pitchdata2015$battedBallSpray = ifelse(pitchdata2015$batterHand == "LHB" & pitchdata2015$battedBallAngle > -15 &
                                         pitchdata2015$battedBallAngle < 15, "Center", as.character(pitchdata2015$battedBallSpray))
pitchdata2015$battedBallSpray = ifelse(pitchdata2015$batterHand == "LHB" & pitchdata2015$battedBallAngle >= 15 &
                                         pitchdata2015$battedBallAngle <=55, "Pull", as.character(pitchdata2015$battedBallSpray))
pitchdata2015$battedBallSpray = ifelse(pitchdata2015$battedBallAngle < -55, "Foul Territory", as.character(pitchdata2015$battedBallSpray))
pitchdata2015$battedBallSpray = ifelse(pitchdata2015$battedBallAngle > 55, "Foul Territory", as.character(pitchdata2015$battedBallSpray))
pitchdata2015$battedBallSpray = as.factor(pitchdata2015$battedBallSpray)

#### Creating pfx_x and pfx_z with Nathan Alan's spinrate formula ####
month_spinrate_reference <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/month_spinrate_reference.csv")
pitchdata2015 = merge(pitchdata2015, month_spinrate_reference, by.x=c("home","month"), by.y=c("home","month"))
#Calculate actual movement
mass.oz = 5.125
circ.in = 9.125
baro_press = 29.92
celcius = (5/9)*(pitchdata2015$temp-32)
elevation.m = pitchdata2015$elevation/3.2808
SVP = 4.5841*exp((18.687-celcius/234.5)*celcius/(257.14+celcius))
barometric = pitchdata2015$baro_press*1000/39.37
beta = 0.0001217
rho.kg = 1.2929*(273/(celcius+273)*(barometric*exp(-beta*elevation.m)-0.3783*pitchdata2015$RH*SVP/100)/760)
rho.lb = rho.kg*0.06261
constant = 0.07182*rho.lb*(5.125/mass.oz)*(circ.in/9.125)^2
vyf = -sqrt(pitchdata2015$vy0^2-2*pitchdata2015$ay*(pitchdata2015$y0-1.417))
time = (vyf-pitchdata2015$vy0)/pitchdata2015$ay
vxbar = (2*pitchdata2015$vx0+pitchdata2015$ax*time)/2
vybar = (2*pitchdata2015$vy0+pitchdata2015$ay*time)/2
vzbar = (2*pitchdata2015$vz0+pitchdata2015$az*time)/2
vbar = sqrt(vxbar^2+vybar^2+vzbar^2)
adrag = -(pitchdata2015$ax*vxbar+pitchdata2015$ay*vybar+(pitchdata2015$az+32.2)*vzbar)/vbar
Cd = adrag/(constant*vbar^2)
vbar2 = vbar/1.467
amagx = pitchdata2015$ax+adrag*vxbar/vbar
amagy = pitchdata2015$ay+adrag*vybar/vbar
amagz = pitchdata2015$az+adrag*vzbar/vbar+32.1
amag = sqrt(amagx^2+amagy^2+amagz^2)
Cl = amag/(constant*vbar^2)
wx = (vybar*amagz-vzbar*amagy)/(vbar*amag)
wy = (vzbar*amagx-vxbar*amagz)/(vbar*amag)
wz = (vxbar*amagy-vybar*amagx)/(vbar*amag)
phimag = ifelse(amagz>0,atan2(amagx,amagz)*180/pi,360+atan2(amagx,amagz)*180/pi)
phispin = ifelse(wz>0,atan2(wx,wz)*180/pi,360+atan2(wx,wz)*180/pi)
thetaspin = asin(wy)*180/pi
S = 0.4*Cl/(1-2.32*Cl)
spin.rpm = round(80*S*vbar, 0)
sig_spin.rpm = spin.rpm*(1/Cl+2.32/(1-2.32*Cl))*0.02
pfx_x = round((0.5*amagx*time^2*12),2)
pitchdata2015$pfx_x = pfx_x
pfx_z = round((0.5*amagz*time^2*12),2)
pitchdata2015$pfx_z = pfx_z
# Remove variables
pitchdata2015$elevation = NULL
pitchdata2015$baro_press = NULL
pitchdata2015$RH = NULL

##### Create Zones for static heat map ######
one_third = (pitchdata2015$szt - pitchdata2015$szb)/3
zone1_bot = pitchdata2015$szt - one_third
zone2_bot = pitchdata2015$szb + one_third
middle = (pitchdata2015$szt + pitchdata2015$szb)/2
pitchdata2015$zone = 0
pitchdata2015$zone = ifelse(pitchdata2015$rev_px >= -0.833 & pitchdata2015$rev_px < -0.2775 &
                               pitchdata2015$pz <= pitchdata2015$szt &
                               pitchdata2015$pz > zone1_bot, 1, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px > -0.2775 & pitchdata2015$rev_px < 0.2775 &
                               pitchdata2015$pz <= pitchdata2015$szt &
                               pitchdata2015$pz > zone1_bot, 2, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px > 0.2775 & pitchdata2015$rev_px <= 0.833 &
                               pitchdata2015$pz <= pitchdata2015$szt &
                               pitchdata2015$pz > zone1_bot, 3, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px >= -0.833 & pitchdata2015$rev_px < -0.2775 &
                               pitchdata2015$pz <= zone1_bot & pitchdata2015$pz >= zone2_bot, 4, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px > -0.2775 & pitchdata2015$rev_px < 0.2775 &
                               pitchdata2015$pz <= zone1_bot & pitchdata2015$pz >= zone2_bot, 5, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px > 0.2775 & pitchdata2015$rev_px <= 0.833 &
                               pitchdata2015$pz <= zone1_bot & pitchdata2015$pz >= zone2_bot, 6, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px >= -0.833 & pitchdata2015$rev_px < -0.2775 &
                               pitchdata2015$pz < zone2_bot &
                               pitchdata2015$pz >= pitchdata2015$szb, 7, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px > -0.2775 & pitchdata2015$rev_px < 0.2775 &
                               pitchdata2015$pz < zone2_bot &
                               pitchdata2015$pz >= pitchdata2015$szb, 8, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px > 0.2775 & pitchdata2015$rev_px <= 0.833 &
                               pitchdata2015$pz < zone2_bot &
                               pitchdata2015$pz >= pitchdata2015$szb, 9, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px < 0 & pitchdata2015$pz > pitchdata2015$szt |
                               pitchdata2015$rev_px < -0.833 & pitchdata2015$pz >= middle, 11, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px >= 0 & pitchdata2015$pz > pitchdata2015$szt |
                               pitchdata2015$rev_px > 0.833 & pitchdata2015$pz >= middle, 12, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px < 0 & pitchdata2015$pz < pitchdata2015$szb |
                               pitchdata2015$rev_px < -0.833 & pitchdata2015$pz < middle, 13, pitchdata2015$zone)
pitchdata2015$zone = ifelse(pitchdata2015$rev_px >= 0 & pitchdata2015$pz < pitchdata2015$szb |
                               pitchdata2015$rev_px > 0.833 & pitchdata2015$pz < middle, 14, pitchdata2015$zone)

#### Reclassify batted ball types ####
pitchdata2015$bb_type = ifelse(pitchdata2015$battedBallType == "BGB" | pitchdata2015$battedBallType == "GB", as.character("Ground Ball"), as.character(""))
pitchdata2015$bb_type = ifelse(pitchdata2015$battedBallType == "BPU" | pitchdata2015$battedBallType == "PU", as.character("Pop Fly"), as.character(pitchdata2015$bb_type))
pitchdata2015$bb_type = ifelse(pitchdata2015$battedBallType == "FB", as.character("Fly Ball"), as.character(pitchdata2015$bb_type))
pitchdata2015$bb_type = ifelse(pitchdata2015$battedBallType == "LD", as.character("Line Drive"), as.character(pitchdata2015$bb_type))
pitchdata2015$bb_type = as.factor(pitchdata2015$bb_type)

#### Add Game ID ####
pitchdata2015$gameId = paste(pitchdata2015$visitor, pitchdata2015$home, sep="@")
pitchdata2015$gameId = paste(pitchdata2015$gameId, substr(pitchdata2015$gameDate,1,10), sep=" ")

###### Reclassifying pitches ######
pitchdata2015$new_pitch_class = ifelse(pitchdata2015$pitchType == "FA" | pitchdata2015$pitchType == "FF",
                                       "Four-seam Fastball", "")
pitchdata2015$new_pitch_class = ifelse(pitchdata2015$pitchType == "SI" | pitchdata2015$pitchType == "FT", 
                                       "Two-seam Fastball", as.character(pitchdata2015$new_pitch_class))
pitchdata2015$new_pitch_class = ifelse(pitchdata2015$pitchType == "FC", "Cut Fastball", as.character(pitchdata2015$new_pitch_class))
pitchdata2015$new_pitch_class = ifelse(pitchdata2015$pitchType == "SL" | pitchdata2015$pitchType == "GY", 
                                       "Slider", as.character(pitchdata2015$new_pitch_class))
pitchdata2015$new_pitch_class = ifelse(pitchdata2015$pitchType == "CU" | pitchdata2015$pitchType == "KC" | 
                                         pitchdata2015$pitchType == "EP", "Curveball", as.character(pitchdata2015$new_pitch_class))
pitchdata2015$new_pitch_class = ifelse(pitchdata2015$pitchType == "CH" | pitchdata2015$pitchType == "SC",
                                       "Change-up", as.character(pitchdata2015$new_pitch_class))
pitchdata2015$new_pitch_class = ifelse(pitchdata2015$pitchType == "FS" | pitchdata2015$pitchType == "FO", 
                                       "Split-finger Fastball", as.character(pitchdata2015$new_pitch_class))
pitchdata2015$new_pitch_class = ifelse(pitchdata2015$pitchType == "KN", "Knuckleball", as.character(pitchdata2015$new_pitch_class))
pitchdata2015$new_pitch_class = ifelse(pitchdata2015$pitchType == "UN" | pitchdata2015$pitchType == "PO" |
                                       pitchdata2015$pitchType == "IN" | pitchdata2015$pitchType == "AB" |
                                       pitchdata2015$pitchType == "AS", "Other", as.character(pitchdata2015$new_pitch_class))
pitchdata2015$new_pitch_class = as.factor(pitchdata2015$new_pitch_class)
# 4-seam Fastball (FF)
# 2-seam Fastball (FT, SI, FA)
# Cut Fastball (FC)
# Slider (SL, GY)
# Curveball (CU, KC, EP)
# Change-up (CH, SC)
# Split-finger Fastball (FS, FO)
# Knuckleball (KN)
# Other (UN, PO, IN, AB, AS)
###### Very General pitch classifications ########
pitchdata2015$gen_pitch_class = "Other"
pitchdata2015$gen_pitch_class = ifelse(pitchdata2015$new_pitch_class == "Four-seam Fastball" | pitchdata2015$new_pitch_class == "Two-seam Fastball" |
                                       pitchdata2015$new_pitch_class == "Cut Fastball", "Fastball", pitchdata2015$gen_pitch_class)
pitchdata2015$gen_pitch_class = ifelse(pitchdata2015$new_pitch_class == "Slider" | pitchdata2015$new_pitch_class == "Curveball",
                                     "Breaking Ball", pitchdata2015$gen_pitch_class)
pitchdata2015$gen_pitch_class = ifelse(pitchdata2015$new_pitch_class == "Change-up" | pitchdata2015$new_pitch_class == "Split-finger Fastball",
                                     "Off-speed Pitch", pitchdata2015$gen_pitch_class)
pitchdata2015$gen_pitch_class = as.factor(pitchdata2015$gen_pitch_class)
# fastball (FF, FT, SI, FA, FC)
# breaking ball (SL, CU, KC, EP, GY)
# off-speed pitch (FS, CH, SC, FO)
# other (KN, UN, PO, IN, AB, AS)

###### Calculating the Effective Velocity of each pitch ######
pitchdata2015 <- pitchdata2015[order(pitchdata2015$season_seq), ]
# Find the slope of the zero line
zl_slope = ifelse(pitchdata2015$batterHand == "LHB", ((.833 +.833)/(pitchdata2015$szb - pitchdata2015$szt)),
                  ((-.833 - .833)/(pitchdata2015$szb - pitchdata2015$szt)))
# Find the y intercept of the zero line (b = y-mx)
zl_yint = ifelse(pitchdata2015$batterHand == "LHB", (pitchdata2015$szb - (.833 * zl_slope)), 
                 (pitchdata2015$szb - (-.833 * zl_slope)))
# Find the slope of the line perpendicular to the zero line
perp_slope = (-1/zl_slope)
# Find the y-intercept of the perpendicular line
perp_yint = pitchdata2015$pz - (perp_slope * pitchdata2015$px)
# Find new_X and new_y of perpendicular line
new_x = round(((zl_yint - perp_yint)/(perp_slope - zl_slope)), 3)
new_y = round(((perp_slope * new_x) + perp_yint), 3)
pitchdata2015$raw_EV = round((new_x - pitchdata2015$px) + (new_y - pitchdata2015$pz), 3)
pitchdata2015$EV_dist = round(sqrt((new_x - pitchdata2015$px)^2 + (new_y - pitchdata2015$pz)^2), 3)
# Create the additional value to be added for Effective releaseVelocity
pitchdata2015$plus_minus_EV = round(pitchdata2015$EV_dist * 5.5, 2)
# Create Effective releaseVelocity variable of the pitch
pitchdata2015$EV = pitchdata2015$releaseVelocity
pitchdata2015$EV = ifelse(pitchdata2015$batterHand == "RHB" & pitchdata2015$raw_EV < 0,
                          round(pitchdata2015$EV - pitchdata2015$plus_minus_EV, 1), pitchdata2015$EV)
pitchdata2015$EV = ifelse(pitchdata2015$batterHand == "RHB" & pitchdata2015$raw_EV > 0,
                          round(pitchdata2015$EV + pitchdata2015$plus_minus_EV, 1), pitchdata2015$EV)
pitchdata2015$EV = ifelse(pitchdata2015$batterHand == "LHB" & pitchdata2015$raw_EV < 0,
                          round(pitchdata2015$EV + pitchdata2015$plus_minus_EV, 1), pitchdata2015$EV)
pitchdata2015$EV = ifelse(pitchdata2015$batterHand == "LHB" & pitchdata2015$raw_EV > 0,
                          round(pitchdata2015$EV - pitchdata2015$plus_minus_EV, 1), pitchdata2015$EV)
pitchdata2015$EV = ifelse(pitchdata2015$batterHand=="RHB", ((pitchdata2015$releaseVelocity - pitchdata2015$EV) + pitchdata2015$releaseVelocity),
                          pitchdata2015$EV)
# Remove columns that are unneccessary
pitchdata2015$raw_EV = NULL
pitchdata2015$EV_dist = NULL
pitchdata2015$plus_minus_EV = NULL

#### Add score difference at the time of the pitch ####
pitchdata2015$score_diff = ifelse(pitchdata2015$side == "B", pitchdata2015$homeTeamCurrentRuns - pitchdata2015$visitingTeamCurrentRuns,
                                  pitchdata2015$visitingTeamCurrentRuns - pitchdata2015$homeTeamCurrentRuns)

#### Add hit location coordinates ####
pitchdata$bb_hit_x = sin(pitchdata$battedBallAngle*pi/180)*pitchdata$battedBallDistance
pitchdata$bb_hit_y = cos(pitchdata$battedBallAngle*pi/180)*pitchdata$battedBallDistance

#####  Add the final result of the pitch #####
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "SS", "Swinging Strike", "")
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "SL", "Strike Looking", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "F", "Foul", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "FT", "Foul Tip", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "FB", "Foul Bunt", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "MB", "Missed Bunt", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "B", "Ball", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "BID", "Ball in Dirt", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "HBP", "Hit By Pitch", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "IB", "Intentional Ball", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "PO", "Pitch Out", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "IP", "Ball in Play", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "AS", "Automatic Strike", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "AB", "Automatic Ball", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "CI", "Catcher Interference", as.character(pitchdata2015$pitchResult2))
pitchdata2015$pitchResult2 = ifelse(pitchdata2015$pitchResult == "UK", "Unknown", as.character(pitchdata2015$pitchResult2))
##
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "S", "Single", "")
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "D", "Double", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "T", "Triple", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "HR", "Home Run", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "BB", "Walk", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "IBB", "Intentional Walk", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "HBP", "Hit By Pitch", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "IP_OUT", "In Play Out", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "K", "Strikeout", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "FC", "Fielder's Choice", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "DP", "Double Play", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "TP", "Triple Play", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "SH", "Sacrifice Bunt", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "SF", "Sacrifice Fly", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "ROE", "Reached on Error", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "SH_ROE", "Sacrifice Bunt ROE", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "SF_ROE", "Sacrifice Fly ROE", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "BI", "Batter Interference", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "CI", "Catcher Interference", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "FI", "Fielder Interference", as.character(pitchdata2015$paResult2))
pitchdata2015$paResult2 = ifelse(pitchdata2015$paResult == "NO_PLAY", "No Play", as.character(pitchdata2015$paResult2))
##
pitchdata2015$result = ifelse(pitchdata2015$paResult == "", as.character(pitchdata2015$pitchResult2), 
                              paste(as.character(pitchdata2015$pitchResult2), as.character(pitchdata2015$paResult2), sep=" - "))
##
pitchdata2015$result = ifelse(pitchdata2015$paResult2 == "Hit By Pitch", "Hit By Pitch", as.character(pitchdata2015$result))
pitchdata2015$result = ifelse(pitchdata2015$paResult2 == "Catcher Interference", "Catcher Interference", as.character(pitchdata2015$result))
pitchdata2015$result = ifelse(pitchdata2015$paResult2 == "Batter Interference", "Batter Interference", as.character(pitchdata2015$result))


####################################################################################################################
########################### Bring all three datasets together for modeling #########################################
####################################################################################################################
####################################################################################################################

##### c-strike gam model - predict probabilty of a called strike #####
library(gam)
library(caTools)
library(ROCR)
library(akima)
# Subset only pitches that were taken
c.strike15 = subset(pitchdata2015, pitchtaken == TRUE & !is.na(px) & !is.na(pz))
c.strike14 = subset(pitchdata2014, pitchtaken == TRUE & !is.na(px) & !is.na(pz))
c.strike13 = subset(pitchdata2013, pitchtaken == TRUE & !is.na(px) & !is.na(pz))
c.strike = rbind(c.strike15, c.strike14)
c.strike = rbind(c.strike, c.strike13)
### GAM Model
c.strike.gam <- gam(cs ~ lo(px, span=0.05) + lo(pz, span=0.05) + count, family=binomial, data=c.strike)
summary(c.strike.gam)
predictStrike = predict(c.strike.gam, type="response")  
summary(predictStrike)
table(c.strike$cs, predictStrike > 0.5)
ROCRpred = prediction(predictStrike, c.strike$cs)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
as.numeric(performance(ROCRpred, "auc")@y.values)
### 10-fold CV
library(ecospat)
library(boot)
set.seed(100)
cv.error.10=rep(0,10)
for(i in 1:10){
  c.strike.gam <- gam(cs ~ lo(px, span=0.05) + lo(pz, span=0.05) + count, family=binomial, data=c.strike)
  cv.error.10[i]=cv.glm(c.strike,c.strike.gam,K=10)$delta[1]
}
### Create ROC Curve
png("c.strike ROC Curve FINAL.png", width = 600, height = 400)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.2), text.adj=c(-1,1), cex=0.5)
title("ROC Curve for 2013-2015 C-Strike Model", 
      cex.main = 1.5, font.main = 4, col.main = "darkblue")
text(0.5, 0.5, "AUC = 0.9747824", cex = 1, font = 3, col="darkred")
dev.off()
## Predict cs_prob back on all three datasets
pitchdata2015a = subset(pitchdata2015, is.na(px) & is.na(pz))
pitchdata2015b = subset(pitchdata2015, !is.na(px), !is.na(pz))
pitchdata2015a$cs_prob = NA
pitchdata2015b$cs_prob = round(predict(c.strike.gam, newdata = pitchdata2015b, type="response"),3)
pitchdata2015 = rbind(pitchdata2015a, pitchdata2015b)
pitchdata2014a = subset(pitchdata2014, is.na(px) & is.na(pz))
pitchdata2014b = subset(pitchdata2014, !is.na(px), !is.na(pz))
pitchdata2014a$cs_prob = NA
pitchdata2014b$cs_prob = round(predict(c.strike.gam, newdata = pitchdata2014b, type="response"),3)
pitchdata2014 = rbind(pitchdata2014a, pitchdata2014b)
pitchdata2013a = subset(pitchdata2013, is.na(px) & is.na(pz))
pitchdata2013b = subset(pitchdata2013, !is.na(px), !is.na(pz))
pitchdata2013a$cs_prob = NA
pitchdata2013b$cs_prob = round(predict(c.strike.gam, newdata = pitchdata2013b, type="response"),3)
pitchdata2013 = rbind(pitchdata2013a, pitchdata2013b)

########################################################################
#### Called Strikes Above Average model... Run each year separately ####
########################################################################
library(lme4)
library(lattice)
pitch_csaa_15 = subset(pitchdata2015, gameType == "REG" & pitchtaken == TRUE & !is.na(cs_prob))
csaa.model = glmer(cs ~ cs_prob + side + (1|catcherId) + (1|pitcherId) + (1|batterId) + (1|catcherId:cs_prob),
                   pitch_csaa_15, family=binomial(link = "probit"), nAGQ=0)
summary(csaa.model)
### Get the random effects of the participants
catcher.csaa <- ranef(csaa.model)$catcherId[,"(Intercept)"]
median.catcher = median(catcher.csaa)
catcher_csaa = coef(csaa.model)$catcherId
catcher_csaa$id = row.names(catcher_csaa)
##
batter.csaa <- ranef(csaa.model)$batterId[,"(Intercept)"]
median.batter = median(batter.csaa)
batter_csaa = coef(csaa.model)$batterId
batter_csaa$id = row.names(batter_csaa)
##
pitcher.csaa <- ranef(csaa.model)$pitcherId[,"(Intercept)"]
median.pitcher = median(pitcher.csaa)
pitcher_csaa = coef(csaa.model)$pitcherId
pitcher_csaa$id = row.names(pitcher_csaa)
##
cs_prob.csaa <- data.frame(coef(csaa.model)["catcherId:cs_prob"])
x_name <- "Intercept"
y_name <- "cs_prob"
z_name <- "half_inning"
names(cs_prob.csaa) <- c(x_name,y_name,z_name)
median.cs_prob = median(cs_prob.csaa$Intercept)
## Calculate null baseline prob - add fixed intercept to the median values of the continuous variables 
baseline = fixef(csaa.model)["(Intercept)"] + median.catcher + median.batter + median.pitcher + median.cs_prob
avg_prob = pnorm(baseline)
##
catcher_partic = catcher.csaa + avg_prob
catcher_prob = pnorm(catcher_partic)
catcher_final = (catcher_prob - avg_prob)
catch_csaa = scale(catcher_final)
catch_csaa = catch_csaa
catch_csaa_2 = catch_csaa * (as.numeric(VarCorr(csaa.model)$catcherId,comp="Variance")*100)
##
batter_partic = batter.csaa + avg_prob
batter_prob = pnorm(batter_partic)
batter_final = batter_prob - avg_prob
bat_csaa = scale(batter_final)
bat_csaa = bat_csaa 
bat_csaa_2 = bat_csaa * (as.numeric(VarCorr(csaa.model)$batterId,comp="Variance")*100)
##
pitcher_partic = pitcher.csaa + avg_prob
pitcher_prob = pnorm(pitcher_partic)
pitcher_final = pitcher_prob - avg_prob
pitch_csaa = scale(pitcher_final)
pitch_csaa = pitch_csaa
pitch_csaa_2 = pitch_csaa * (as.numeric(VarCorr(csaa.model)$pitcherId,comp="Variance")*100)
## Make the dataframes with CSAA values
catch = data.frame(row.names(catcher_csaa), round(catch_csaa,1), round(catch_csaa_2,1))
x_name <- "Player_ID"
y_name <- "CSAA"
z_name <- "Relative_CSAA"
names(catch) <- c(x_name,y_name,z_name)
catch$Year = 2015
catch$Position = "Catcher"
catch = catch[,c("Player_ID","Position","Year","CSAA", "Relative_CSAA")]
##
bat = data.frame(row.names(batter_csaa), round(bat_csaa,1), round(bat_csaa_2,1))
x_name <- "Player_ID"
y_name <- "CSAA"
z_name <- "Relative_CSAA"
names(bat) <- c(x_name,y_name, z_name)
bat$Year = 2015
bat$Position = "Batter"
bat = bat[,c("Player_ID","Position","Year","CSAA", "Relative_CSAA")]
##
pitch = data.frame(row.names(pitcher_csaa), round(pitch_csaa,1), round(pitch_csaa_2,1))
x_name <- "Player_ID"
y_name <- "CSAA"
z_name <- "Relative_CSAA"
names(pitch) <- c(x_name,y_name,z_name)
pitch$Year = 2015
pitch$Position = "Pitcher"
pitch = pitch[,c("Player_ID","Position","Year","CSAA", "Relative_CSAA")]
##
csaa_15 = rbind(catch, bat)
csaa_15 = rbind(csaa_15, pitch)
csaa_15$Player = master_id$mlb_name[match(csaa_15$Player_ID, master_id$mlb_id)]
write.csv(csaa_15, file = "csaa_15.csv")
### Match CSAA back onto pitch-by-pitch data
pitchdata2015$catcher_csaa = catch$Relative_CSAA[match(pitchdata2015$catcherId, catch$Player_ID)]
pitchdata2015$catcher_csaa = ifelse(is.na(pitchdata2015$catcher_csaa), 0, as.numeric(pitchdata2015$catcher_csaa))
pitchdata2015$pitcher_csaa = pitch$Relative_CSAA[match(pitchdata2015$pitcherId, pitch$Player_ID)]
pitchdata2015$pitcher_csaa = ifelse(is.na(pitchdata2015$pitcher_csaa), 0, as.numeric(pitchdata2015$pitcher_csaa))
pitchdata2015$batter_csaa = bat$Relative_CSAA[match(pitchdata2015$batterId, bat$Player_ID)]
pitchdata2015$batter_csaa = ifelse(is.na(pitchdata2015$batter_csaa), 0, as.numeric(pitchdata2015$batter_csaa))
summary(pitchdata2015$catcher_csaa)
summary(pitchdata2015$batter_csaa)
summary(pitchdata2015$pitcher_csaa)
### Calculate cumulative CSAA for each pitch
pitchdata2015$csaa = pitchdata2015$catcher_csaa + pitchdata2015$pitcher_csaa + pitchdata2015$batter_csaa
summary(pitchdata2015$csaa)
### Bring all CSAA years together
csaa_final = rbind(csaa_15, csaa_14)
csaa_final = rbind(csaa_final, csaa_13)
write.csv(csaa_final, file="csaa_final.csv")

###############################################################
#### Final data cleaning revisions - Run for all three sets ###
###############################################################

### Add number of RBIs for RBI situations ###
pitchdata2015$rbi_ct = 0
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "S", as.numeric(pitchdata2015$runsScoredOnPitch), 0)
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "D", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "T", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "HR", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "BB", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "IBB", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "HBP", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "IP_OUT", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "K", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "FC", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "SH", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "SF", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "ROE" & pitchdata2015$event_outs < 1, as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "SH_ROE" & pitchdata2015$event_outs < 1, as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "SF_ROE" & pitchdata2015$event_outs < 1, as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))
pitchdata2015$rbi_ct = ifelse(pitchdata2015$paResult == "CI", as.numeric(pitchdata2015$runsScoredOnPitch), as.numeric(pitchdata2015$runsScoredOnPitch))

### Add BIP event ###
pitchdata2015$bip_event = ifelse(pitchdata2015$bip == TRUE, as.character(pitchdata2015$paResult2), NA)

### batterHand back to R & L for same_hand variable ###
pitchdata2015$batterHand = ifelse(pitchdata2015$batterHand == "RHB", "R", "L")
pitchdata2015$same_hand = ifelse(pitchdata2015$batterHand == pitchdata2015$pitcherHand, TRUE, FALSE)

###########################################################################
############# Mixed Modeling: GLMER for BIP Likelihood Model ##############
###########################################################################
library(lme4)
library(gam)
library(caTools)
library(ROCR)
library(akima)
###
pitchdata2015a = subset(pitchdata2015, is.na(px) | is.na(pz))
pitchdata2015b = subset(pitchdata2015, !is.na(px) | !is.na(pz))
pitchdata2014a = subset(pitchdata2014, is.na(px) | is.na(pz))
pitchdata2014b = subset(pitchdata2014, !is.na(px) | !is.na(pz))
pitchdata2013a = subset(pitchdata2013, is.na(px) | is.na(pz))
pitchdata2013b = subset(pitchdata2013, !is.na(px) | !is.na(pz))
### 2015 BIP GLMER Model
bip.glmer <- glmer(bip ~ lo(pz,span=0.15) + lo(abs(px),span=0.15) + pfx_z + pfx_x:batterHand + cs_prob + count + TTO + 
                     (1 | batterId) + (1 | pitcherId), data=pitchdata2015b, family=binomial(link="probit"), nAGQ=0)
summary(bip.glmer)
###
predictBIP = predict(bip.glmer, type="response")  
summary(predictBIP)
table(pitchdata2015b$bip, predictBIP > 0.5)
###
ROCRpred = prediction(predictBIP, pitchdata2015b$bip)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
as.numeric(performance(ROCRpred, "auc")@y.values)
###
png("BIP ROC Curve 2015 FINAL.png", width = 600, height = 400)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.25), text.adj=c(-.5,1), cex=0.5)
title("ROC Curve for 2015 BIP Model", 
      cex.main = 1.5, font.main = 4, col.main = "darkblue")
text(0.5, 0.5, "AUC = 0.7786438", cex = 1, font = 3, col="darkred")
dev.off()
###
pitchdata2015a$bip_prob = NA
pitchdata2015a$lgavg_pitcher_bip_prob = NA
pitchdata2015a$lgavg_batter_bip_prob = NA
pitchdata2015b$bip_prob = round(predict(bip.glmer, newdata = pitchdata2015b, type="response"),3)
pitchdata2015b$lgavg_pitcher_bip_prob = round(predict(bip.glmer, newdata = pitchdata2015b, re.form=~(1|batterId), type="response"),3)
pitchdata2015b$lgavg_batter_bip_prob = round(predict(bip.glmer, newdata = pitchdata2015b, re.form=~(1|pitcherId), type="response"),3)
pitchdata2015 = rbind(pitchdata2015a, pitchdata2015b)
pitchdata2015$no_bip_prob = 1 - pitchdata2015$bip_prob
pitchdata2015$lgavg_pitcher_no_bip_prob = 1 - pitchdata2015$lgavg_pitcher_bip_prob
pitchdata2015$lgavg_batter_no_bip_prob = 1 - pitchdata2015$lgavg_batter_bip_prob
pitchdata2015 <- pitchdata2015[order(pitchdata2015$season_seq), ]
### 2014 BIP GLMER Model
bip.glmer <- glmer(bip ~ lo(pz,span=0.15) + lo(abs(px),span=0.15) + pfx_z + pfx_x:batterHand + abs(pfx_x) + new_pitch_class*scale(spinRate) + 
                     batterHand:pitcherHand + cs_prob*count + TTO + base_out_state + new_pitch_class*scale(releaseVelocity) + 
                     (1 | batterId) + (1 | pitcherId), data=pitchdata2014b, family=binomial(link="probit"), nAGQ=0)
summary(bip.glmer)
###
predictBIP = predict(bip.glmer, type="response")  
summary(predictBIP)
table(pitchdata2014b$bip, predictBIP > 0.5)
###
ROCRpred = prediction(predictBIP, pitchdata2014b$bip)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
as.numeric(performance(ROCRpred, "auc")@y.values)
###
png("BIP ROC Curve 2014 FINAL.png", width = 600, height = 400)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.25), text.adj=c(-.5,1), cex=0.5)
title("ROC Curve for 2014 BIP Model", 
      cex.main = 1.5, font.main = 4, col.main = "darkblue")
text(0.5, 0.5, "AUC = 0.7844914", cex = 1, font = 3, col="darkred")
dev.off()
###
pitchdata2014a$bip_prob = NA
pitchdata2014a$lgavg_pitcher_bip_prob = NA
pitchdata2014a$lgavg_batter_bip_prob = NA
pitchdata2014b$bip_prob = round(predict(bip.glmer, newdata = pitchdata2014b, type="response"),3)
pitchdata2014b$lgavg_pitcher_bip_prob = round(predict(bip.glmer, newdata = pitchdata2014b, re.form=~(1|batterId), type="response"),3)
pitchdata2014b$lgavg_batter_bip_prob = round(predict(bip.glmer, newdata = pitchdata2014b, re.form=~(1|pitcherId), type="response"),3)
pitchdata2014 = rbind(pitchdata2014a, pitchdata2014b)
pitchdata2014$no_bip_prob = 1 - pitchdata2014$bip_prob
pitchdata2014$lgavg_pitcher_no_bip_prob = 1 - pitchdata2014$lgavg_pitcher_bip_prob
pitchdata2014$lgavg_batter_no_bip_prob = 1 - pitchdata2014$lgavg_batter_bip_prob
pitchdata2014 <- pitchdata2014[order(pitchdata2014$season_seq), ]
### 2013 BIP GLMER Model
bip.glmer <- glmer(bip ~ lo(pz,span=0.15) + lo(abs(px),span=0.15) + pfx_z + pfx_x:batterHand + abs(pfx_x) + new_pitch_class*scale(spinRate) + 
                     same_hand + batterHand:pitcherHand + cs_prob*count + TTO + base_out_state + new_pitch_class*scale(releaseVelocity) +
                     (1 | batterId) + (1 | pitcherId), data=pitchdata2013b, family=binomial(link="probit"), nAGQ=0)
summary(bip.glmer)
###
predictBIP = predict(bip.glmer, type="response")  
summary(predictBIP)
table(pitchdata2013b$bip, predictBIP > 0.5)
###
ROCRpred = prediction(predictBIP, pitchdata2013b$bip)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
as.numeric(performance(ROCRpred, "auc")@y.values)
###
png("BIP ROC Curve 2013 FINAL.png", width = 600, height = 400)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.25), text.adj=c(-.5,1), cex=0.5)
title("ROC Curve for 2013 BIP Model", 
      cex.main = 1.5, font.main = 4, col.main = "darkblue")
text(0.5, 0.5, "AUC = 0.7851618", cex = 1, font = 3, col="darkred")
dev.off()
### 
pitchdata2013a$bip_prob = NA
pitchdata2013a$lgavg_pitcher_bip_prob = NA
pitchdata2013a$lgavg_batter_bip_prob = NA
pitchdata2013b$bip_prob = round(predict(bip.glmer, newdata = pitchdata2013b, type="response"),3)
pitchdata2013b$lgavg_pitcher_bip_prob = round(predict(bip.glmer, newdata = pitchdata2013b, re.form=~(1|batterId), type="response"),3)
pitchdata2013b$lgavg_batter_bip_prob = round(predict(bip.glmer, newdata = pitchdata2013b, re.form=~(1|pitcherId), type="response"),3)
pitchdata2013 = rbind(pitchdata2013a, pitchdata2013b)
pitchdata2013$no_bip_prob = 1 - pitchdata2013$bip_prob
pitchdata2013lgavg_pitcher_no_bip_prob = 1 - pitchdata2013$lgavg_pitcher_bip_prob
pitchdata2013$lgavg_batter_no_bip_prob = 1 - pitchdata2013$lgavg_batter_bip_prob
pitchdata2013 <- pitchdata2013[order(pitchdata2013$season_seq), ]

##################################################################################
############## FINAL LINEAR MIXED MODELS - NIP Model & BIP Model #################
##################################################################################
library(lme4)
###################################
### 2015 Ball not in play model ###
nip15 = subset(pitchdata2015, bip == FALSE & !is.na(px) & !is.na(pz))
nip14 = subset(pitchdata2014, bip == FALSE & !is.na(px) & !is.na(pz))
nip = rbind(nip15, nip14)
### Create 2015 NIP LMER model
nip.model = lmer(final_run_value ~ inning*score_diff + same_hand + px:batterHand + abs(px)*pz + base_out_state + count + csaa + new_pitch_class + 
                   TTO + cs_prob + side + home + seasonIP*ISSP + (1 | catcherId) + (1 | pitcherId) + (1 | batterId), data = nip)
summary(nip.model)
### Predict back onto data set for situation and league-averages
pitchdata2015$nip_cLWTS = round(predict(nip.model, newdata=pitchdata2015, allow.new.levels=TRUE), 3)
pitchdata2015$lgavg_pit_nip_cLWTS = round(predict(nip.model, newdata=pitchdata2015, allow.new.levels=TRUE,
                                                  re.form=~(1|catcherId) + (1|batterId)), 3)
pitchdata2015$lgavg_bat_nip_cLWTS = round(predict(nip.model, newdata=pitchdata2015, allow.new.levels=TRUE,
                                                  re.form=~(1|catcherId) + (1|pitcherId)), 3)
summary(pitchdata2015$nip_cLWTS)
summary(pitchdata2015$lgavg_pit_nip_cLWTS)
summary(pitchdata2015$lgavg_bat_nip_cLWTS)
### 2015 Ball not in play model
bip15 = subset(pitchdata2015, bip == TRUE & !is.na(px) & !is.na(pz))
bip15$nip_cLWTS = NULL
bip15$lgavg_pit_nip_cLWTS = NULL
bip15$lgavg_bat_nip_cLWTS = NULL
bip14 = subset(pitchdata2014, bip == TRUE & !is.na(px) & !is.na(pz))
bip = rbind(bip15, bip14)
### Create 2015 BIP LMER model
bip.model = lmer(final_run_value ~ inning + score_diff + Runs.Factor + side + home + base_out_state + new_pitch_class + count +
                   px:batterHand + abs(px):pz + cs_prob + same_hand + TTO + seasonIP*ISSP + (1 | pitcherId) + (1 | batterId), data=bip)
summary(bip.model)
### Predict back onto data set for situation and league-averages
pitchdata2015$bip_cLWTS = round(predict(bip.model, newdata=pitchdata2015, allow.new.levels=TRUE), 3)
pitchdata2015$lgavg_pit_bip_cLWTS = round(predict(bip.model, newdata=pitchdata2015, allow.new.levels=TRUE,
                                                  re.form=~(1|batterId)), 3)
pitchdata2015$lgavg_bat_bip_cLWTS = round(predict(bip.model, newdata=pitchdata2015, allow.new.levels=TRUE,
                                                  re.form=~(1|pitcherId)), 3)
summary(pitchdata2015$bip_cLWTS)
summary(pitchdata2015$lgavg_pit_bip_cLWTS)
summary(pitchdata2015$lgavg_bat_bip_cLWTS)
### Bring both models together for final contextual pitch type linear weight
pitchdata2015$LWTS_context = ((pitchdata2015$bip_prob * pitchdata2015$bip_cLWTS) +
                                (pitchdata2015$no_bip_prob * pitchdata2015$nip_cLWTS))
pitchdata2015$lgavg_pit_LWTS_context = ((pitchdata2015$lgavg_pitcher_bip_prob * pitchdata2015$lgavg_pit_bip_cLWTS) +
                                          (pitchdata2015$lgavg_pitcher_no_bip_prob * pitchdata2015$lgavg_pit_nip_cLWTS))
pitchdata2015$lgavg_bat_LWTS_context = ((pitchdata2015$lgavg_batter_bip_prob * pitchdata2015$lgavg_bat_bip_cLWTS) +
                                          (pitchdata2015$lgavg_batter_no_bip_prob * pitchdata2015$lgavg_bat_nip_cLWTS))
summary(pitchdata2015$LWTS_context)
summary(pitchdata2015$lgavg_pit_LWTS_context)
summary(pitchdata2015$lgavg_bat_LWTS_context)
pitchdata2015$pit_LWTS_AA = round(pitchdata2015$LWTS_context - pitchdata2015$lgavg_pit_LWTS_context, 3)
pitchdata2015$bat_LWTS_AA = round(pitchdata2015$LWTS_context - pitchdata2015$lgavg_bat_LWTS_context, 3)
summary(pitchdata2015$pit_LWTS_AA)
summary(pitchdata2015$bat_LWTS_AA)
### Save file
write.csv(pitchdata2015, file = "pitchdata2015.csv")
####################################################
### 2014 Ball not in play model ###
nip14 = subset(pitchdata2014, bip == FALSE & !is.na(px) & !is.na(pz))
nip13 = subset(pitchdata2013, bip == FALSE & !is.na(px) & !is.na(pz))
nip = rbind(nip14, nip13)
### Create 2014 NIP LMER model
nip.model = lmer(final_run_value ~ inning*score_diff + same_hand + px:batterHand + abs(px):pz + abs(px) + base_out_state + count + csaa + 
                   new_pitch_class + TTO + cs_prob + side + home + seasonIP*ISSP + (1 | catcherId) + (1 | pitcherId) + (1 | batterId), data = nip)
summary(nip.model)
### Predict back onto data set for situation and league-averages
pitchdata2014$nip_cLWTS = round(predict(nip.model, newdata=pitchdata2014, allow.new.levels=TRUE), 3)
pitchdata2014$lgavg_pit_nip_cLWTS = round(predict(nip.model, newdata=pitchdata2014, allow.new.levels=TRUE,
                                                  re.form=~(1|catcherId) + (1|batterId)), 3)
pitchdata2014$lgavg_bat_nip_cLWTS = round(predict(nip.model, newdata=pitchdata2014, allow.new.levels=TRUE,
                                                  re.form=~(1|catcherId) + (1|pitcherId)), 3)
summary(pitchdata2014$nip_cLWTS)
summary(pitchdata2014$lgavg_pit_nip_cLWTS)
summary(pitchdata2014$lgavg_bat_nip_cLWTS)
### 2014 Ball not in play model
bip14 = subset(pitchdata2014, bip == TRUE & !is.na(px) & !is.na(pz))
bip14$nip_cLWTS = NULL
bip14$lgavg_pit_nip_cLWTS = NULL
bip14$lgavg_bat_nip_cLWTS = NULL
bip13 = subset(pitchdata2013, bip == TRUE & !is.na(px) & !is.na(pz))
bip = rbind(bip14, bip13)
### Create 2014 BIP LMER model
bip.model = lmer(final_run_value ~ inning + Runs.Factor + side + home + base_out_state + new_pitch_class + count + px:batterHand + 
                   abs(px):pz + cs_prob + same_hand + TTO + seasonIP*ISSP + (1 | pitcherId) + (1 | batterId), data=bip)
summary(bip.model)
### Predict back onto data set for situation and league-averages
pitchdata2014$bip_cLWTS = round(predict(bip.model, newdata=pitchdata2014, allow.new.levels=TRUE), 3)
pitchdata2014$lgavg_pit_bip_cLWTS = round(predict(bip.model, newdata=pitchdata2014, allow.new.levels=TRUE,
                                                  re.form=~(1|batterId)), 3)
pitchdata2014$lgavg_bat_bip_cLWTS = round(predict(bip.model, newdata=pitchdata2014, allow.new.levels=TRUE,
                                                  re.form=~(1|pitcherId)), 3)
summary(pitchdata2014$bip_cLWTS)
summary(pitchdata2014$lgavg_pit_bip_cLWTS)
summary(pitchdata2014$lgavg_bat_bip_cLWTS)
### Bring both models together for final contextual pitch type linear weight
pitchdata2014$LWTS_context = ((pitchdata2014$bip_prob * pitchdata2014$bip_cLWTS) +
                                (pitchdata2014$no_bip_prob * pitchdata2014$nip_cLWTS))
pitchdata2014$lgavg_pit_LWTS_context = ((pitchdata2014$lgavg_pitcher_bip_prob * pitchdata2014$lgavg_pit_bip_cLWTS) +
                                          (pitchdata2014$lgavg_pitcher_no_bip_prob * pitchdata2014$lgavg_pit_nip_cLWTS))
pitchdata2014$lgavg_bat_LWTS_context = ((pitchdata2014$lgavg_batter_bip_prob * pitchdata2014$lgavg_bat_bip_cLWTS) +
                                          (pitchdata2014$lgavg_batter_no_bip_prob * pitchdata2014$lgavg_bat_nip_cLWTS))
summary(pitchdata2014$LWTS_context)
summary(pitchdata2014$lgavg_pit_LWTS_context)
summary(pitchdata2014$lgavg_bat_LWTS_context)
pitchdata2014$pit_LWTS_AA = round(pitchdata2014$LWTS_context - pitchdata2014$lgavg_pit_LWTS_context, 3)
pitchdata2014$bat_LWTS_AA = round(pitchdata2014$LWTS_context - pitchdata2014$lgavg_bat_LWTS_context, 3)
summary(pitchdata2014$pit_LWTS_AA)
summary(pitchdata2014$bat_LWTS_AA)
### Save file
write.csv(pitchdata2014, file = "pitchdata2014.csv")
####################################################
### 2013 Ball not in play model ###
nip = subset(pitchdata2013, bip == FALSE & !is.na(px) & !is.na(pz))
### Create 2013 NIP LMER model
nip.model = lmer(final_run_value ~ inning*score_diff + same_hand + px:batterHand + abs(px)*pz + base_out_state + count + csaa + new_pitch_class + 
                   TTO + cs_prob + side + home + seasonIP + ISSP + (1 | catcherId) + (1 | pitcherId) + (1 | batterId), data = nip)
summary(nip.model)
### Predict back onto data set for situation and league-averages
pitchdata2013$nip_cLWTS = round(predict(nip.model, newdata=pitchdata2013, allow.new.levels=TRUE), 3)
pitchdata2013$lgavg_pit_nip_cLWTS = round(predict(nip.model, newdata=pitchdata2013, allow.new.levels=TRUE,
                                                  re.form=~(1|catcherId) + (1|batterId)), 3)
pitchdata2013$lgavg_bat_nip_cLWTS = round(predict(nip.model, newdata=pitchdata2013, allow.new.levels=TRUE,
                                                  re.form=~(1|catcherId) + (1|pitcherId)), 3)
summary(pitchdata2013$nip_cLWTS)
summary(pitchdata2013$lgavg_pit_nip_cLWTS)
summary(pitchdata2013$lgavg_bat_nip_cLWTS)
### 2013 Ball not in play model
bip13 = subset(pitchdata2013, bip == TRUE & !is.na(px) & !is.na(pz))
bip13$nip_cLWTS = NULL
bip13$lgavg_pit_nip_cLWTS = NULL
bip13$lgavg_bat_nip_cLWTS = NULL
### Create 2013 BIP LMER model
bip.model = lmer(final_run_value ~ inning + Runs.Factor + side + home + base_out_state + new_pitch_class + count + px:batterHand + 
                   abs(px):pz + cs_prob + same_hand + TTO + seasonIP*ISSP + (1 | pitcherId) + (1 | batterId), data=bip)
summary(bip.model)
### Predict back onto data set for situation and league-averages
pitchdata2013$bip_cLWTS = round(predict(bip.model, newdata=pitchdata2013, allow.new.levels=TRUE), 3)
pitchdata2013$lgavg_pit_bip_cLWTS = round(predict(bip.model, newdata=pitchdata2013, allow.new.levels=TRUE,
                                                  re.form=~(1|batterId)), 3)
pitchdata2013$lgavg_bat_bip_cLWTS = round(predict(bip.model, newdata=pitchdata2013, allow.new.levels=TRUE,
                                                  re.form=~(1|pitcherId)), 3)
summary(pitchdata2013$bip_cLWTS)
summary(pitchdata2013$lgavg_pit_bip_cLWTS)
summary(pitchdata2013$lgavg_bat_bip_cLWTS)
### Bring both models together for final contextual pitch type linear weight
pitchdata2013$LWTS_context = ((pitchdata2013$bip_prob * pitchdata2013$bip_cLWTS) +
                                (pitchdata2013$no_bip_prob * pitchdata2013$nip_cLWTS))
pitchdata2013$lgavg_pit_LWTS_context = ((pitchdata2013$lgavg_pitcher_bip_prob * pitchdata2013$lgavg_pit_bip_cLWTS) +
                                          (pitchdata2013$lgavg_pitcher_no_bip_prob * pitchdata2013$lgavg_pit_nip_cLWTS))
pitchdata2013$lgavg_bat_LWTS_context = ((pitchdata2013$lgavg_batter_bip_prob * pitchdata2013$lgavg_bat_bip_cLWTS) +
                                          (pitchdata2013$lgavg_batter_no_bip_prob * pitchdata2013$lgavg_bat_nip_cLWTS))
summary(pitchdata2013$LWTS_context)
summary(pitchdata2013$lgavg_pit_LWTS_context)
summary(pitchdata2013$lgavg_bat_LWTS_context)
pitchdata2013$pit_LWTS_AA = round(pitchdata2013$LWTS_context - pitchdata2013$lgavg_pit_LWTS_context, 3)
pitchdata2013$bat_LWTS_AA = round(pitchdata2013$LWTS_context - pitchdata2013$lgavg_bat_LWTS_context, 3)
summary(pitchdata2013$pit_LWTS_AA)
summary(pitchdata2013$bat_LWTS_AA)
### Save file
write.csv(pitchdata2013, file = "pitchdata2013.csv")
####################################################

### Add home team ballpark variable to manipulate in Tableau
pitchdata2015$PARK_ID = pitchdata2015$home
pitchdata2014$PARK_ID = ifelse(pitchdata2014$month == 3 & pitchdata2014$home == "ARI" & pitchdata2014$visitor == "LAD",
                               "SYD", as.character(pitchdata2014$home))
pitchdata2014$PARK_ID = as.factor(pitchdata2014$PARK_ID)
pitchdata2013$PARK_ID = pitchdata2013$home

##############################################################################################
######## Merge all 3 seasons of pitch-by-pitch data for Tableau BvP Visualization ############
##############################################################################################
write.csv(pitchdata2015, file = "pitchdata2015.csv")
write.csv(pitchdata2014, file = "pitchdata2014.csv")
write.csv(pitchdata2013, file = "pitchdata2013.csv")

pitchdata = rbind(pitchdata2015, pitchdata2014)
pitchdata = rbind(pitchdata, pitchdata2013)


write.csv(pitchdata, file="pitchdata_final.csv")

###################################################################################################
###############   Aggregate pitcher and batter cLWTS to test relationship   #######################
###############         and then create final cLWTS leaderboards            #######################
###################################################################################################
pitchdata2015 = subset(pitchdata2015, gameType == "REG" & !is.na(pit_LWTS_AA) & !is.na(bat_LWTS_AA))
pitchdata2014 = subset(pitchdata2014, gameType == "REG" & !is.na(pit_LWTS_AA) & !is.na(bat_LWTS_AA))
pitchdata2013 = subset(pitchdata2013, gameType == "REG" & !is.na(pit_LWTS_AA) & !is.na(bat_LWTS_AA))

### New pitch classification subsets for correlation testing ###
allpitches2015 = subset(pitchdata2015)
fourseamers2015 = subset(pitchdata2015, new_pitch_class == "Four-seam Fastball")
twoseamers2015 = subset(pitchdata2015, new_pitch_class == "Two-seam Fastball")
cutters2015 = subset(pitchdata2015, new_pitch_class == "Cut Fastball")
splitters2015 = subset(pitchdata2015, new_pitch_class == "Split-finger Fastball")
sliders2015 = subset(pitchdata2015, new_pitch_class == "Slider")
curves2015 = subset(pitchdata2015, new_pitch_class == "Curveball")
changeups2015 = subset(pitchdata2015, new_pitch_class == "Change-up")
knucklers2015 = subset(pitchdata2015, new_pitch_class == "Knuckleball")
fastballs2015 = subset(pitchdata2015, gen_pitch_class == "Fastball")
breakballs2015 = subset(pitchdata2015, gen_pitch_class == "Breaking Ball")
offspeeds2015 = subset(pitchdata2015, gen_pitch_class == "Off-speed Pitch")
###
allpitches2014 = subset(pitchdata2014)
fourseamers2014 = subset(pitchdata2014, new_pitch_class == "Four-seam Fastball")
twoseamers2014 = subset(pitchdata2014, new_pitch_class == "Two-seam Fastball")
cutters2014 = subset(pitchdata2014, new_pitch_class == "Cut Fastball")
splitters2014 = subset(pitchdata2014, new_pitch_class == "Split-finger Fastball")
sliders2014 = subset(pitchdata2014, new_pitch_class == "Slider")
curves2014 = subset(pitchdata2014, new_pitch_class == "Curveball")
changeups2014 = subset(pitchdata2014, new_pitch_class == "Change-up")
knucklers2014 = subset(pitchdata2014, new_pitch_class == "Knuckleball")
fastballs2014 = subset(pitchdata2014, gen_pitch_class == "Fastball")
breakballs2014 = subset(pitchdata2014, gen_pitch_class == "Breaking Ball")
offspeeds2014 = subset(pitchdata2014, gen_pitch_class == "Off-speed Pitch")
###
allpitches2013 = subset(pitchdata2013)
fourseamers2013 = subset(pitchdata2013, new_pitch_class == "Four-seam Fastball")
twoseamers2013 = subset(pitchdata2013, new_pitch_class == "Two-seam Fastball")
cutters2013 = subset(pitchdata2013, new_pitch_class == "Cut Fastball")
splitters2013 = subset(pitchdata2013, new_pitch_class == "Split-finger Fastball")
sliders2013 = subset(pitchdata2013, new_pitch_class == "Slider")
curves2013 = subset(pitchdata2013, new_pitch_class == "Curveball")
changeups2013 = subset(pitchdata2013, new_pitch_class == "Change-up")
knucklers2013 = subset(pitchdata2013, new_pitch_class == "Knuckleball")
fastballs2013 = subset(pitchdata2013, gen_pitch_class == "Fastball")
breakballs2013 = subset(pitchdata2013, gen_pitch_class == "Breaking Ball")
offspeeds2013 = subset(pitchdata2013, gen_pitch_class == "Off-speed Pitch")

##############################################################################
### Aggregate Pitcher cLWTS - Rerun for all three seasons before moving on ###
##############################################################################
allpitches2015.values <- aggregate(allpitches2015$pit_LWTS_AA, list(allpitches2015$pitcherId), sum)
allpitches2015.pitches <- aggregate(allpitches2015$pit_LWTS_AA, list(allpitches2015$pitcherId), length)
names(allpitches2015.values) <- c("Pitcher", "w_pitches15")
names(allpitches2015.pitches) <- c("Pitcher", "Pitches15")
allpitches2015.p100 <- merge(allpitches2015.values, allpitches2015.pitches)
allpitches2015.p100$w_pitches15_p100 = round(((allpitches2015.p100$w_pitches15/allpitches2015.p100$Pitches15)*100),3)

fourseamer15.values <- aggregate(fourseamers2015$pit_LWTS_AA, list(fourseamers2015$pitcherId), sum)
fourseamer15.pitches <- aggregate(fourseamers2015$pit_LWTS_AA, list(fourseamers2015$pitcherId), length)
names(fourseamer15.values) <- c("Pitcher", "w_4seamer15")
names(fourseamer15.pitches) <- c("Pitcher", "Pitches15")
fourseamers15.p100 <- merge(fourseamer15.values, fourseamer15.pitches)
fourseamers15.p100$w_4seamer15_p100 = round(((fourseamers15.p100$w_4seamer15/fourseamers15.p100$Pitches15)*100),3)

twoseamer15.values <- aggregate(twoseamers2015$pit_LWTS_AA, list(twoseamers2015$pitcherId), sum)
twoseamer15.pitches <- aggregate(twoseamers2015$pit_LWTS_AA, list(twoseamers2015$pitcherId), length)
names(twoseamer15.values) <- c("Pitcher", "w_2seamer15")
names(twoseamer15.pitches) <- c("Pitcher", "Pitches15")
twoseamers15.p100 <- merge(twoseamer15.values, twoseamer15.pitches)
twoseamers15.p100$w_2seamer15_p100 = round(((twoseamers15.p100$w_2seamer15/twoseamers15.p100$Pitches15)*100),3)

cutters15.values <- aggregate(cutters2015$pit_LWTS_AA, list(cutters2015$pitcherId), sum)
cutters15.pitches <- aggregate(cutters2015$pit_LWTS_AA, list(cutters2015$pitcherId), length)
names(cutters15.values) <- c("Pitcher", "w_cutter15")
names(cutters15.pitches) <- c("Pitcher", "Pitches15")
cutters15.p100 <- merge(cutters15.values, cutters15.pitches)
cutters15.p100$w_cutters15_p100 = round(((cutters15.p100$w_cutter15/cutters15.p100$Pitches15)*100),3)

splitters15.values <- aggregate(splitters2015$pit_LWTS_AA, list(splitters2015$pitcherId), sum)
splitters15.pitches <- aggregate(splitters2015$pit_LWTS_AA, list(splitters2015$pitcherId), length)
names(splitters15.values) <- c("Pitcher", "w_splitter15")
names(splitters15.pitches) <- c("Pitcher", "Pitches15")
splitters15.p100 <- merge(splitters15.values, splitters15.pitches)
splitters15.p100$w_splitters15_p100 = round(((splitters15.p100$w_splitter15/splitters15.p100$Pitches15)*100),3)

sliders15.values <- aggregate(sliders2015$pit_LWTS_AA, list(sliders2015$pitcherId), sum)
sliders15.pitches <- aggregate(sliders2015$pit_LWTS_AA, list(sliders2015$pitcherId), length)
names(sliders15.values) <- c("Pitcher", "w_slider15")
names(sliders15.pitches) <- c("Pitcher", "Pitches15")
sliders15.p100 <- merge(sliders15.values, sliders15.pitches)
sliders15.p100$w_sliders15_p100 = round(((sliders15.p100$w_slider15/sliders15.p100$Pitches15)*100),3)

curves15.values <- aggregate(curves2015$pit_LWTS_AA, list(curves2015$pitcherId), sum)
curves15.pitches <- aggregate(curves2015$pit_LWTS_AA, list(curves2015$pitcherId), length)
names(curves15.values) <- c("Pitcher", "w_curve15")
names(curves15.pitches) <- c("Pitcher", "Pitches15")
curves15.p100 <- merge(curves15.values, curves15.pitches)
curves15.p100$w_curves15_p100 = round(((curves15.p100$w_curve15/curves15.p100$Pitches15)*100),3)

changeups15.values <- aggregate(changeups2015$pit_LWTS_AA, list(changeups2015$pitcherId), sum)
changeups15.pitches <- aggregate(changeups2015$pit_LWTS_AA, list(changeups2015$pitcherId), length)
names(changeups15.values) <- c("Pitcher", "w_change15")
names(changeups15.pitches) <- c("Pitcher", "Pitches15")
changeups15.p100 <- merge(changeups15.values, changeups15.pitches)
changeups15.p100$w_changeups15_p100 = round(((changeups15.p100$w_change15/changeups15.p100$Pitches15)*100),3)

knucklers15.values <- aggregate(knucklers2015$pit_LWTS_AA, list(knucklers2015$pitcherId), sum)
knucklers15.pitches <- aggregate(knucklers2015$pit_LWTS_AA, list(knucklers2015$pitcherId), length)
names(knucklers15.values) <- c("Pitcher", "w_knuckle15")
names(knucklers15.pitches) <- c("Pitcher", "Pitches15")
knucklers15.p100 <- merge(knucklers15.values, knucklers15.pitches)
knucklers15.p100$w_knucklers15_p100 = round(((knucklers15.p100$w_knuckle15/knucklers15.p100$Pitches15)*100),3)

fastballs2015.values <- aggregate(fastballs2015$pit_LWTS_AA, list(fastballs2015$pitcherId), sum)
fastballs2015.pitches <- aggregate(fastballs2015$pit_LWTS_AA, list(fastballs2015$pitcherId), length)
names(fastballs2015.values) <- c("Pitcher", "w_fastball15")
names(fastballs2015.pitches) <- c("Pitcher", "Pitches15")
fastballs2015.p100 <- merge(fastballs2015.values, fastballs2015.pitches)
fastballs2015.p100$w_fastball15_p100 = round(((fastballs2015.p100$w_fastball15/fastballs2015.p100$Pitches15)*100),3)

breakballs2015.values <- aggregate(breakballs2015$pit_LWTS_AA, list(breakballs2015$pitcherId), sum)
breakballs2015.pitches <- aggregate(breakballs2015$pit_LWTS_AA, list(breakballs2015$pitcherId), length)
names(breakballs2015.values) <- c("Pitcher", "w_breakball15")
names(breakballs2015.pitches) <- c("Pitcher", "Pitches15")
breakballs2015.p100 <- merge(breakballs2015.values, breakballs2015.pitches)
breakballs2015.p100$w_breakball15_p100 = round(((breakballs2015.p100$w_breakball15/breakballs2015.p100$Pitches15)*100),3)

offspeeds2015.values <- aggregate(offspeeds2015$pit_LWTS_AA, list(offspeeds2015$pitcherId), sum)
offspeeds2015.pitches <- aggregate(offspeeds2015$pit_LWTS_AA, list(offspeeds2015$pitcherId), length)
names(offspeeds2015.values) <- c("Pitcher", "w_offspeed15")
names(offspeeds2015.pitches) <- c("Pitcher", "Pitches15")
offspeeds2015.p100 <- merge(offspeeds2015.values, offspeeds2015.pitches)
offspeeds2015.p100$w_offspeed15_p100 = round(((offspeeds2015.p100$w_offspeed15/offspeeds2015.p100$Pitches15)*100),3)
##### MAKE SURE YOU RAN ON ALL THREE SEASONS #####

#### Merging and testing of correlations for cumulative values ####
allpitches.final = merge(allpitches2013.values, allpitches2014.values, all = TRUE)
allpitches.final = merge(allpitches.final, allpitches2015.values, all = TRUE)
cor(allpitches.final$w_pitches13, allpitches.final$w_pitches14, use="pairwise.complete.obs")
cor(allpitches.final$w_pitches14, allpitches.final$w_pitches15, use="pairwise.complete.obs")
cor(allpitches.final$w_pitches13, allpitches.final$w_pitches15, use="pairwise.complete.obs")
nrow(allpitches2013.values)
nrow(allpitches2014.values)
nrow(allpitches2015.values)

fourseamers.final = merge(fourseamer13.values, fourseamer14.values, all = TRUE)
fourseamers.final = merge(fourseamers.final, fourseamer15.values, all = TRUE)
cor(fourseamers.final$w_4seamer13, fourseamers.final$w_4seamer14, use="pairwise.complete.obs")
cor(fourseamers.final$w_4seamer14, fourseamers.final$w_4seamer15, use="pairwise.complete.obs")
cor(fourseamers.final$w_4seamer13, fourseamers.final$w_4seamer15, use="pairwise.complete.obs")
nrow(fourseamer13.values)
nrow(fourseamer14.values)
nrow(fourseamer15.values)

twoseamers.final = merge(twoseamer13.values, twoseamer14.values, all = TRUE)
twoseamers.final = merge(twoseamers.final, twoseamer15.values, all = TRUE)
cor(twoseamers.final$w_2seamer13, twoseamers.final$w_2seamer14, use="pairwise.complete.obs")
cor(twoseamers.final$w_2seamer14, twoseamers.final$w_2seamer15, use="pairwise.complete.obs")
cor(twoseamers.final$w_2seamer13, twoseamers.final$w_2seamer15, use="pairwise.complete.obs")
nrow(twoseamer13.values)
nrow(twoseamer14.values)
nrow(twoseamer15.values)

cutters.final = merge(cutters13.values, cutters14.values, all = TRUE)
cutters.final = merge(cutters.final, cutters15.values, all = TRUE)
cor(cutters.final$w_cutter13, cutters.final$w_cutter14, use="pairwise.complete.obs")
cor(cutters.final$w_cutter14, cutters.final$w_cutter15, use="pairwise.complete.obs")
cor(cutters.final$w_cutter13, cutters.final$w_cutter15, use="pairwise.complete.obs")
nrow(cutters13.values)
nrow(cutters14.values)
nrow(cutters15.values)

splitters.final = merge(splitters13.values, splitters14.values, all = TRUE)
splitters.final = merge(splitters.final, splitters15.values, all = TRUE)
cor(splitters.final$w_splitter13, splitters.final$w_splitter14, use="pairwise.complete.obs")
cor(splitters.final$w_splitter14, splitters.final$w_splitter15, use="pairwise.complete.obs")
cor(splitters.final$w_splitter13, splitters.final$w_splitter15, use="pairwise.complete.obs")
nrow(splitters13.values)
nrow(splitters14.values)
nrow(splitters15.values)

sliders.final = merge(sliders13.values, sliders14.values, all = TRUE)
sliders.final = merge(sliders.final, sliders15.values, all = TRUE)
cor(sliders.final$w_slider13, sliders.final$w_slider14, use="pairwise.complete.obs")
cor(sliders.final$w_slider14, sliders.final$w_slider15, use="pairwise.complete.obs")
cor(sliders.final$w_slider13, sliders.final$w_slider15, use="pairwise.complete.obs")
nrow(sliders13.values)
nrow(sliders14.values)
nrow(sliders15.values)

curves.final = merge(curves13.values, curves14.values, all = TRUE)
curves.final = merge(curves.final, curves15.values, all = TRUE)
cor(curves.final$w_curve13, curves.final$w_curve14, use="pairwise.complete.obs")
cor(curves.final$w_curve14, curves.final$w_curve15, use="pairwise.complete.obs")
cor(curves.final$w_curve13, curves.final$w_curve15, use="pairwise.complete.obs")
nrow(curves13.values)
nrow(curves14.values)
nrow(curves15.values)

changeups.final = merge(changeups13.values, changeups14.values, all = TRUE)
changeups.final = merge(changeups.final, changeups15.values, all = TRUE)
cor(changeups.final$w_change13, changeups.final$w_change14, use="pairwise.complete.obs")
cor(changeups.final$w_change14, changeups.final$w_change15, use="pairwise.complete.obs")
cor(changeups.final$w_change13, changeups.final$w_change15, use="pairwise.complete.obs")
nrow(changeups13.values)
nrow(changeups14.values)
nrow(changeups15.values)

fastballs.final = merge(fastballs2013.values, fastballs2014.values, all = TRUE)
fastballs.final = merge(fastballs.final, fastballs2015.values, all = TRUE)
cor(fastballs.final$w_fastball13, fastballs.final$w_fastball14, use="pairwise.complete.obs")
cor(fastballs.final$w_fastball14, fastballs.final$w_fastball15, use="pairwise.complete.obs")
cor(fastballs.final$w_fastball13, fastballs.final$w_fastball15, use="pairwise.complete.obs")
nrow(fastballs2013.values)
nrow(fastballs2014.values)
nrow(fastballs2015.values)

breakballs.final = merge(breakballs2013.values, breakballs2014.values, all = TRUE)
breakballs.final = merge(breakballs.final, breakballs2015.values, all = TRUE)
cor(breakballs.final$w_breakball13, breakballs.final$w_breakball14, use="pairwise.complete.obs")
cor(breakballs.final$w_breakball14, breakballs.final$w_breakball15, use="pairwise.complete.obs")
cor(breakballs.final$w_breakball13, breakballs.final$w_breakball15, use="pairwise.complete.obs")
nrow(breakballs2013.values)
nrow(breakballs2014.values)
nrow(breakballs2015.values)

offspeeds.final = merge(offspeeds2013.values, offspeeds2014.values, all = TRUE)
offspeeds.final = merge(offspeeds.final, offspeeds2015.values, all = TRUE)
cor(offspeeds.final$w_offspeed13, offspeeds.final$w_offspeed14, use="pairwise.complete.obs")
cor(offspeeds.final$w_offspeed14, offspeeds.final$w_offspeed15, use="pairwise.complete.obs")
cor(offspeeds.final$w_offspeed13, offspeeds.final$w_offspeed15, use="pairwise.complete.obs")
nrow(offspeeds2013.values)
nrow(offspeeds2014.values)
nrow(offspeeds2015.values)

#### Merging and testing of correlations per 100 pitches ####
allpitches.p100.final = merge(allpitches2013.p100, allpitches2014.p100, all = TRUE)
allpitches.p100.final = merge(allpitches.p100.final, allpitches2015.p100, all = TRUE)
cor(allpitches.p100.final$w_pitches13_p100, allpitches.p100.final$w_pitches14_p100, use="pairwise.complete.obs")
cor(allpitches.p100.final$w_pitches14_p100, allpitches.p100.final$w_pitches15_p100, use="pairwise.complete.obs")
cor(allpitches.p100.final$w_pitches13_p100, allpitches.p100.final$w_pitches15_p100, use="pairwise.complete.obs")
nrow(allpitches2013.p100)
nrow(allpitches2014.p100)
nrow(allpitches2015.p100)

fourseamers.p100.final = merge(fourseamers13.p100, fourseamers14.p100, all = TRUE)
fourseamers.p100.final = merge(fourseamers.p100.final, fourseamers15.p100, all = TRUE)
cor(fourseamers.p100.final$w_4seamer13_p100, fourseamers.p100.final$w_4seamer14_p100, use="pairwise.complete.obs")
cor(fourseamers.p100.final$w_4seamer14_p100, fourseamers.p100.final$w_4seamer15_p100, use="pairwise.complete.obs")
cor(fourseamers.p100.final$w_4seamer13_p100, fourseamers.p100.final$w_4seamer15_p100, use="pairwise.complete.obs")
nrow(fourseamers13.p100)
nrow(fourseamers14.p100)
nrow(fourseamers15.p100)

twoseamers.p100.final = merge(twoseamers13.p100, twoseamers14.p100, all = TRUE)
twoseamers.p100.final = merge(twoseamers.p100.final, twoseamers15.p100, all = TRUE)
cor(twoseamers.p100.final$w_2seamer13_p100, twoseamers.p100.final$w_2seamer14_p100, use="pairwise.complete.obs")
cor(twoseamers.p100.final$w_2seamer14_p100, twoseamers.p100.final$w_2seamer15_p100, use="pairwise.complete.obs")
cor(twoseamers.p100.final$w_2seamer13_p100, twoseamers.p100.final$w_2seamer15_p100, use="pairwise.complete.obs")
nrow(twoseamers13.p100)
nrow(twoseamers14.p100)
nrow(twoseamers15.p100)

cutters.p100.final = merge(cutters13.p100, cutters14.p100, all = TRUE)
cutters.p100.final = merge(cutters.p100.final, cutters15.p100, all = TRUE)
cor(cutters.p100.final$w_cutters13_p100, cutters.p100.final$w_cutters14_p100, use="pairwise.complete.obs")
cor(cutters.p100.final$w_cutters14_p100, cutters.p100.final$w_cutters15_p100, use="pairwise.complete.obs")
cor(cutters.p100.final$w_cutters13_p100, cutters.p100.final$w_cutters15_p100, use="pairwise.complete.obs")
nrow(cutters13.p100)
nrow(cutters14.p100)
nrow(cutters15.p100)

splitters.p100.final = merge(splitters13.p100, splitters14.p100, all = TRUE)
splitters.p100.final = merge(splitters.p100.final, splitters15.p100, all = TRUE)
cor(splitters.p100.final$w_splitters13_p100, splitters.p100.final$w_splitters14_p100, use="pairwise.complete.obs")
cor(splitters.p100.final$w_splitters14_p100, splitters.p100.final$w_splitters15_p100, use="pairwise.complete.obs")
cor(splitters.p100.final$w_splitters13_p100, splitters.p100.final$w_splitters15_p100, use="pairwise.complete.obs")
nrow(splitters13.p100)
nrow(splitters14.p100)
nrow(splitters15.p100)

sliders.p100.final = merge(sliders13.p100, sliders14.p100, all = TRUE)
sliders.p100.final = merge(sliders.p100.final, sliders15.p100, all = TRUE)
cor(sliders.p100.final$w_sliders13_p100, sliders.p100.final$w_sliders14_p100, use="pairwise.complete.obs")
cor(sliders.p100.final$w_sliders14_p100, sliders.p100.final$w_sliders15_p100, use="pairwise.complete.obs")
cor(sliders.p100.final$w_sliders13_p100, sliders.p100.final$w_sliders15_p100, use="pairwise.complete.obs")
nrow(sliders13.p100)
nrow(sliders14.p100)
nrow(sliders15.p100)

curves.p100.final = merge(curves13.p100, curves14.p100, all = TRUE)
curves.p100.final = merge(curves.p100.final, curves15.p100, all = TRUE)
cor(curves.p100.final$w_curves13_p100, curves.p100.final$w_curves14_p100, use="pairwise.complete.obs")
cor(curves.p100.final$w_curves14_p100, curves.p100.final$w_curves15_p100, use="pairwise.complete.obs")
cor(curves.p100.final$w_curves13_p100, curves.p100.final$w_curves15_p100, use="pairwise.complete.obs")
nrow(curves13.p100)
nrow(curves14.p100)
nrow(curves15.p100)

changeups.p100.final = merge(changeups13.p100, changeups14.p100, all = TRUE)
changeups.p100.final = merge(changeups.p100.final, changeups15.p100, all = TRUE)
cor(changeups.p100.final$w_changeups13_p100, changeups.p100.final$w_changeups14_p100, use="pairwise.complete.obs")
cor(changeups.p100.final$w_changeups14_p100, changeups.p100.final$w_changeups15_p100, use="pairwise.complete.obs")
cor(changeups.p100.final$w_changeups13_p100, changeups.p100.final$w_changeups15_p100, use="pairwise.complete.obs")
nrow(changeups13.p100)
nrow(changeups14.p100)
nrow(changeups15.p100)

fastballs.p100.final = merge(fastballs2013.p100, fastballs2014.p100, all = TRUE)
fastballs.p100.final = merge(fastballs.p100.final, fastballs2015.p100, all = TRUE)
cor(fastballs.p100.final$w_fastball13_p100, fastballs.p100.final$w_fastball14_p100, use="pairwise.complete.obs")
cor(fastballs.p100.final$w_fastball14_p100, fastballs.p100.final$w_fastball15_p100, use="pairwise.complete.obs")
cor(fastballs.p100.final$w_fastball13_p100, fastballs.p100.final$w_fastball15_p100, use="pairwise.complete.obs")
nrow(fastballs2013.p100)
nrow(fastballs2014.p100)
nrow(fastballs2015.p100)

breakballs.p100.final = merge(breakballs2013.p100, breakballs2014.p100, all = TRUE)
breakballs.p100.final = merge(breakballs.p100.final, breakballs2015.p100, all = TRUE)
cor(breakballs.p100.final$w_breakball13_p100, breakballs.p100.final$w_breakball14_p100, use="pairwise.complete.obs")
cor(breakballs.p100.final$w_breakball14_p100, breakballs.p100.final$w_breakball15_p100, use="pairwise.complete.obs")
cor(breakballs.p100.final$w_breakball13_p100, breakballs.p100.final$w_breakball15_p100, use="pairwise.complete.obs")
nrow(breakballs2013.p100)
nrow(breakballs2014.p100)
nrow(breakballs2015.p100)

offspeeds.p100.final = merge(offspeeds2013.p100, offspeeds2014.p100, all = TRUE)
offspeeds.p100.final = merge(offspeeds.p100.final, offspeeds2015.p100, all = TRUE)
cor(offspeeds.p100.final$w_offspeed13_p100, offspeeds.p100.final$w_offspeed14_p100, use="pairwise.complete.obs")
cor(offspeeds.p100.final$w_offspeed14_p100, offspeeds.p100.final$w_offspeed15_p100, use="pairwise.complete.obs")
cor(offspeeds.p100.final$w_offspeed13_p100, offspeeds.p100.final$w_offspeed15_p100, use="pairwise.complete.obs")
nrow(offspeeds2013.p100)
nrow(offspeeds2014.p100)
nrow(offspeeds2015.p100)

#### Import Baseball Prospectus and FanGraphs pitching stats to test correlation to cLWTS ####
metrics15 <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/metrics15.csv")
metrics14 <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/metrics14.csv")
metrics13 <- read.csv("~/Documents/final-hackathon-project/mlb-hackathon-2015/metrics13.csv")

### Test correlation to 2015 pitching metrics
allpitches2015.metrics = allpitches2015.p100
allpitches2015.metrics$RE24 = as.numeric(as.character(metrics15$fg_RE24[match(allpitches2015.metrics$Pitcher, metrics15$mlb_id)]))
allpitches2015.metrics$ERA = as.numeric(as.character(metrics15$bp_ERA[match(allpitches2015.metrics$Pitcher, metrics15$mlb_id)]))
allpitches2015.metrics$FIP = as.numeric(as.character(metrics15$bp_FIP[match(allpitches2015.metrics$Pitcher, metrics15$mlb_id)]))
allpitches2015.metrics$xFIP = as.numeric(as.character(metrics15$fg_xFIP[match(allpitches2015.metrics$Pitcher, metrics15$mlb_id)]))
allpitches2015.metrics$cFIP = as.numeric(as.character(metrics15$bp_cFIP[match(allpitches2015.metrics$Pitcher, metrics15$mlb_id)]))
allpitches2015.metrics$DRA = as.numeric(as.character(metrics15$bp_DRA[match(allpitches2015.metrics$Pitcher, metrics15$mlb_id)]))
allpitches2015.metrics$SIERA = as.numeric(as.character(metrics15$fg_SIERA[match(allpitches2015.metrics$Pitcher, metrics15$mlb_id)]))

round(cor(allpitches2015.metrics$w_pitches15, allpitches2015.metrics$RE24, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15, allpitches2015.metrics$ERA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15, allpitches2015.metrics$FIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15, allpitches2015.metrics$xFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15, allpitches2015.metrics$cFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15, allpitches2015.metrics$DRA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15, allpitches2015.metrics$SIERA, use="pairwise.complete.obs"), 2)

round(cor(allpitches2015.metrics$w_pitches15_p100, allpitches2015.metrics$RE24, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15_p100, allpitches2015.metrics$ERA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15_p100, allpitches2015.metrics$FIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15_p100, allpitches2015.metrics$xFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15_p100, allpitches2015.metrics$cFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15_p100, allpitches2015.metrics$DRA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2015.metrics$w_pitches15_p100, allpitches2015.metrics$SIERA, use="pairwise.complete.obs"), 2)

### Test correlation to 2014 pitching metrics
allpitches2014.metrics = allpitches2014.p100
allpitches2014.metrics$RE24 = as.numeric(as.character(metrics14$fg_RE24[match(allpitches2014.metrics$Pitcher, metrics14$mlb_id)]))
allpitches2014.metrics$ERA = as.numeric(as.character(metrics14$bp_ERA[match(allpitches2014.metrics$Pitcher, metrics14$mlb_id)]))
allpitches2014.metrics$FIP = as.numeric(as.character(metrics14$bp_FIP[match(allpitches2014.metrics$Pitcher, metrics14$mlb_id)]))
allpitches2014.metrics$xFIP = as.numeric(as.character(metrics14$fg_xFIP[match(allpitches2014.metrics$Pitcher, metrics14$mlb_id)]))
allpitches2014.metrics$cFIP = as.numeric(as.character(metrics14$bp_cFIP[match(allpitches2014.metrics$Pitcher, metrics14$mlb_id)]))
allpitches2014.metrics$DRA = as.numeric(as.character(metrics14$bp_DRA[match(allpitches2014.metrics$Pitcher, metrics14$mlb_id)]))
allpitches2014.metrics$SIERA = as.numeric(as.character(metrics14$fg_SIERA[match(allpitches2014.metrics$Pitcher, metrics14$mlb_id)]))

round(cor(allpitches2014.metrics$w_pitches14, allpitches2014.metrics$RE24, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14, allpitches2014.metrics$ERA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14, allpitches2014.metrics$FIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14, allpitches2014.metrics$xFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14, allpitches2014.metrics$cFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14, allpitches2014.metrics$DRA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14, allpitches2014.metrics$SIERA, use="pairwise.complete.obs"), 2)

round(cor(allpitches2014.metrics$w_pitches14_p100, allpitches2014.metrics$RE24, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14_p100, allpitches2014.metrics$ERA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14_p100, allpitches2014.metrics$FIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14_p100, allpitches2014.metrics$xFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14_p100, allpitches2014.metrics$cFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14_p100, allpitches2014.metrics$DRA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2014.metrics$w_pitches14_p100, allpitches2014.metrics$SIERA, use="pairwise.complete.obs"), 2)

### Test correlation to 2013 pitching metrics
allpitches2013.metrics = allpitches2013.p100
allpitches2013.metrics$RE24 = as.numeric(as.character(metrics13$fg_RE24[match(allpitches2013.metrics$Pitcher, metrics13$mlb_id)]))
allpitches2013.metrics$ERA = as.numeric(as.character(metrics13$bp_ERA[match(allpitches2013.metrics$Pitcher, metrics13$mlb_id)]))
allpitches2013.metrics$FIP = as.numeric(as.character(metrics13$bp_FIP[match(allpitches2013.metrics$Pitcher, metrics13$mlb_id)]))
allpitches2013.metrics$xFIP = as.numeric(as.character(metrics13$fg_xFIP[match(allpitches2013.metrics$Pitcher, metrics13$mlb_id)]))
allpitches2013.metrics$cFIP = as.numeric(as.character(metrics13$bp_cFIP[match(allpitches2013.metrics$Pitcher, metrics13$mlb_id)]))
allpitches2013.metrics$DRA = as.numeric(as.character(metrics13$bp_DRA[match(allpitches2013.metrics$Pitcher, metrics13$mlb_id)]))
allpitches2013.metrics$SIERA = as.numeric(as.character(metrics13$fg_SIERA[match(allpitches2013.metrics$Pitcher, metrics13$mlb_id)]))

round(cor(allpitches2013.metrics$w_pitches13, allpitches2013.metrics$RE24, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13, allpitches2013.metrics$ERA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13, allpitches2013.metrics$FIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13, allpitches2013.metrics$xFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13, allpitches2013.metrics$cFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13, allpitches2013.metrics$DRA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13, allpitches2013.metrics$SIERA, use="pairwise.complete.obs"), 2)

round(cor(allpitches2013.metrics$w_pitches13_p100, allpitches2013.metrics$RE24, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13_p100, allpitches2013.metrics$ERA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13_p100, allpitches2013.metrics$FIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13_p100, allpitches2013.metrics$xFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13_p100, allpitches2013.metrics$cFIP, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13_p100, allpitches2013.metrics$DRA, use="pairwise.complete.obs"), 2)
round(cor(allpitches2013.metrics$w_pitches13_p100, allpitches2013.metrics$SIERA, use="pairwise.complete.obs"), 2)

#### Create 2015 cLWTS Pitcher Leaderboard ####
cLWTS15 = subset(allpitches2015.p100)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'AllPitches'
cLWTS15 = merge(cLWTS15, fourseamers15.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'FourSeamers'
cLWTS15 = merge(cLWTS15, twoseamers15.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'TwoSeamers'
cLWTS15 = merge(cLWTS15, cutters15.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Cutters'
cLWTS15 = merge(cLWTS15, splitters15.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Splitters'
cLWTS15 = merge(cLWTS15, sliders15.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Sliders'
cLWTS15 = merge(cLWTS15, curves15.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Curves'
cLWTS15 = merge(cLWTS15, changeups15.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Changeups'
cLWTS15 = merge(cLWTS15, knucklers15.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Knuckleballs'
names(cLWTS15)[names(cLWTS15) == 'w_pitches15'] <- 'wALL'
names(cLWTS15)[names(cLWTS15) == 'w_pitches15_p100'] <- 'wALL_100'
names(cLWTS15)[names(cLWTS15) == 'w_4seamer15'] <- 'wFF'
names(cLWTS15)[names(cLWTS15) == 'w_4seamer15_p100'] <- 'wFF_100'
names(cLWTS15)[names(cLWTS15) == 'w_2seamer15'] <- 'wFT'
names(cLWTS15)[names(cLWTS15) == 'w_2seamer15_p100'] <- 'wFT_100'
names(cLWTS15)[names(cLWTS15) == 'w_cutter15'] <- 'wFC'
names(cLWTS15)[names(cLWTS15) == 'w_cutters15_p100'] <- 'wFC_100'
names(cLWTS15)[names(cLWTS15) == 'w_splitter15'] <- 'wFS'
names(cLWTS15)[names(cLWTS15) == 'w_splitters15_p100'] <- 'wFS_100'
names(cLWTS15)[names(cLWTS15) == 'w_slider15'] <- 'wSL'
names(cLWTS15)[names(cLWTS15) == 'w_sliders15_p100'] <- 'wSL_100'
names(cLWTS15)[names(cLWTS15) == 'w_curve15'] <- 'wCU'
names(cLWTS15)[names(cLWTS15) == 'w_curves15_p100'] <- 'wCU_100'
names(cLWTS15)[names(cLWTS15) == 'w_change15'] <- 'wCH'
names(cLWTS15)[names(cLWTS15) == 'w_changeups15_p100'] <- 'wCH_100'
names(cLWTS15)[names(cLWTS15) == 'w_knuckle15'] <- 'wKN'
names(cLWTS15)[names(cLWTS15) == 'w_knucklers15_p100'] <- 'wKN_100'
cLWTS15$Year = 2015
cLWTS15$Name = master_id$mlb_name[match(cLWTS15$Pitcher, master_id$mlb_id)]
cLWTS15 <- cLWTS15[,c("Name","Pitcher", "Year", "wALL", "wFF", "wFT", "wFC", "wFS", "wSL", "wCU", "wCH", "wKN", "wALL_100",
                      "wFF_100", "wFT_100", "wFC_100", "wFS_100", "wSL_100", "wCU_100", "wCH_100", "wKN_100",
                      "AllPitches", "FourSeamers", "TwoSeamers", "Cutters", "Splitters", "Sliders", "Curves", 
                      "Changeups", "Knuckleballs")]
cLWTS15 = cLWTS15[(order(cLWTS15$wALL)), ]

#### Create 2014 cLWTS Pitcher Leaderboard ####
cLWTS14 = subset(allpitches2014.p100)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'AllPitches'
cLWTS14 = merge(cLWTS14, fourseamers14.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'FourSeamers'
cLWTS14 = merge(cLWTS14, twoseamers14.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'TwoSeamers'
cLWTS14 = merge(cLWTS14, cutters14.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Cutters'
cLWTS14 = merge(cLWTS14, splitters14.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Splitters'
cLWTS14 = merge(cLWTS14, sliders14.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Sliders'
cLWTS14 = merge(cLWTS14, curves14.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Curves'
cLWTS14 = merge(cLWTS14, changeups14.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Changeups'
cLWTS14 = merge(cLWTS14, knucklers14.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Knuckleballs'
names(cLWTS14)[names(cLWTS14) == 'w_pitches14'] <- 'wALL'
names(cLWTS14)[names(cLWTS14) == 'w_pitches14_p100'] <- 'wALL_100'
names(cLWTS14)[names(cLWTS14) == 'w_4seamer14'] <- 'wFF'
names(cLWTS14)[names(cLWTS14) == 'w_4seamer14_p100'] <- 'wFF_100'
names(cLWTS14)[names(cLWTS14) == 'w_2seamer14'] <- 'wFT'
names(cLWTS14)[names(cLWTS14) == 'w_2seamer14_p100'] <- 'wFT_100'
names(cLWTS14)[names(cLWTS14) == 'w_cutter14'] <- 'wFC'
names(cLWTS14)[names(cLWTS14) == 'w_cutters14_p100'] <- 'wFC_100'
names(cLWTS14)[names(cLWTS14) == 'w_splitter14'] <- 'wFS'
names(cLWTS14)[names(cLWTS14) == 'w_splitters14_p100'] <- 'wFS_100'
names(cLWTS14)[names(cLWTS14) == 'w_slider14'] <- 'wSL'
names(cLWTS14)[names(cLWTS14) == 'w_sliders14_p100'] <- 'wSL_100'
names(cLWTS14)[names(cLWTS14) == 'w_curve14'] <- 'wCU'
names(cLWTS14)[names(cLWTS14) == 'w_curves14_p100'] <- 'wCU_100'
names(cLWTS14)[names(cLWTS14) == 'w_change14'] <- 'wCH'
names(cLWTS14)[names(cLWTS14) == 'w_changeups14_p100'] <- 'wCH_100'
names(cLWTS14)[names(cLWTS14) == 'w_knuckle14'] <- 'wKN'
names(cLWTS14)[names(cLWTS14) == 'w_knucklers14_p100'] <- 'wKN_100'
cLWTS14$Year = 2014
cLWTS14$Name = master_id$mlb_name[match(cLWTS14$Pitcher, master_id$mlb_id)]
cLWTS14 <- cLWTS14[,c("Name","Pitcher", "Year", "wALL", "wFF", "wFT", "wFC", "wFS", "wSL", "wCU", "wCH", "wKN", "wALL_100",
                      "wFF_100", "wFT_100", "wFC_100", "wFS_100", "wSL_100", "wCU_100", "wCH_100", "wKN_100",
                      "AllPitches", "FourSeamers", "TwoSeamers", "Cutters", "Splitters", "Sliders", "Curves", 
                      "Changeups", "Knuckleballs")]
cLWTS14 = cLWTS14[(order(cLWTS14$wALL)), ]

#### Create 2013 cLWTS Pitcher Leaderboard ####
cLWTS13 = subset(allpitches2013.p100)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'AllPitches'
cLWTS13 = merge(cLWTS13, fourseamers13.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'FourSeamers'
cLWTS13 = merge(cLWTS13, twoseamers13.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'TwoSeamers'
cLWTS13 = merge(cLWTS13, cutters13.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Cutters'
cLWTS13 = merge(cLWTS13, splitters13.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Splitters'
cLWTS13 = merge(cLWTS13, sliders13.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Sliders'
cLWTS13 = merge(cLWTS13, curves13.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Curves'
cLWTS13 = merge(cLWTS13, changeups13.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Changeups'
cLWTS13 = merge(cLWTS13, knucklers13.p100, by = "Pitcher", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Knuckleballs'
names(cLWTS13)[names(cLWTS13) == 'w_pitches13'] <- 'wALL'
names(cLWTS13)[names(cLWTS13) == 'w_pitches13_p100'] <- 'wALL_100'
names(cLWTS13)[names(cLWTS13) == 'w_4seamer13'] <- 'wFF'
names(cLWTS13)[names(cLWTS13) == 'w_4seamer13_p100'] <- 'wFF_100'
names(cLWTS13)[names(cLWTS13) == 'w_2seamer13'] <- 'wFT'
names(cLWTS13)[names(cLWTS13) == 'w_2seamer13_p100'] <- 'wFT_100'
names(cLWTS13)[names(cLWTS13) == 'w_cutter13'] <- 'wFC'
names(cLWTS13)[names(cLWTS13) == 'w_cutters13_p100'] <- 'wFC_100'
names(cLWTS13)[names(cLWTS13) == 'w_splitter13'] <- 'wFS'
names(cLWTS13)[names(cLWTS13) == 'w_splitters13_p100'] <- 'wFS_100'
names(cLWTS13)[names(cLWTS13) == 'w_slider13'] <- 'wSL'
names(cLWTS13)[names(cLWTS13) == 'w_sliders13_p100'] <- 'wSL_100'
names(cLWTS13)[names(cLWTS13) == 'w_curve13'] <- 'wCU'
names(cLWTS13)[names(cLWTS13) == 'w_curves13_p100'] <- 'wCU_100'
names(cLWTS13)[names(cLWTS13) == 'w_change13'] <- 'wCH'
names(cLWTS13)[names(cLWTS13) == 'w_changeups13_p100'] <- 'wCH_100'
names(cLWTS13)[names(cLWTS13) == 'w_knuckle13'] <- 'wKN'
names(cLWTS13)[names(cLWTS13) == 'w_knucklers13_p100'] <- 'wKN_100'
cLWTS13$Year = 2013
cLWTS13$Name = master_id$mlb_name[match(cLWTS13$Pitcher, master_id$mlb_id)]
cLWTS13 <- cLWTS13[,c("Name","Pitcher", "Year", "wALL", "wFF", "wFT", "wFC", "wFS", "wSL", "wCU", "wCH", "wKN", "wALL_100",
                      "wFF_100", "wFT_100", "wFC_100", "wFS_100", "wSL_100", "wCU_100", "wCH_100", "wKN_100",
                      "AllPitches", "FourSeamers", "TwoSeamers", "Cutters", "Splitters", "Sliders", "Curves", 
                      "Changeups", "Knuckleballs")]
cLWTS13 = cLWTS13[(order(cLWTS13$wALL)), ]
### Merge three leaderboards together for Tableau ###
cLWTS_Pitcher = rbind(cLWTS15, cLWTS14)
cLWTS_Pitcher = rbind(cLWTS_Pitcher, cLWTS13)
write.csv(cLWTS_Pitcher, file = "cLWTS_Pitcher.csv")

##### RERUN nearly everything for batter cLWTS ######

##############################################################################
### Aggregate Batter cLWTS - Rerun for all three seasons before moving on ###
##############################################################################
allpitches2015.values <- aggregate(allpitches2015$bat_LWTS_AA, list(allpitches2015$batterId), sum)
allpitches2015.pitches <- aggregate(allpitches2015$bat_LWTS_AA, list(allpitches2015$batterId), length)
names(allpitches2015.values) <- c("Batter", "w_pitches15")
names(allpitches2015.pitches) <- c("Batter", "Pitches15")
allpitches2015.p100 <- merge(allpitches2015.values, allpitches2015.pitches)
allpitches2015.p100$w_pitches15_p100 = round(((allpitches2015.p100$w_pitches15/allpitches2015.p100$Pitches15)*100),3)

fourseamer15.values <- aggregate(fourseamers2015$bat_LWTS_AA, list(fourseamers2015$batterId), sum)
fourseamer15.pitches <- aggregate(fourseamers2015$bat_LWTS_AA, list(fourseamers2015$batterId), length)
names(fourseamer15.values) <- c("Batter", "w_4seamer15")
names(fourseamer15.pitches) <- c("Batter", "Pitches15")
fourseamers15.p100 <- merge(fourseamer15.values, fourseamer15.pitches)
fourseamers15.p100$w_4seamer15_p100 = round(((fourseamers15.p100$w_4seamer15/fourseamers15.p100$Pitches15)*100),3)

twoseamer15.values <- aggregate(twoseamers2015$bat_LWTS_AA, list(twoseamers2015$batterId), sum)
twoseamer15.pitches <- aggregate(twoseamers2015$bat_LWTS_AA, list(twoseamers2015$batterId), length)
names(twoseamer15.values) <- c("Batter", "w_2seamer15")
names(twoseamer15.pitches) <- c("Batter", "Pitches15")
twoseamers15.p100 <- merge(twoseamer15.values, twoseamer15.pitches)
twoseamers15.p100$w_2seamer15_p100 = round(((twoseamers15.p100$w_2seamer15/twoseamers15.p100$Pitches15)*100),3)

cutters15.values <- aggregate(cutters2015$bat_LWTS_AA, list(cutters2015$batterId), sum)
cutters15.pitches <- aggregate(cutters2015$bat_LWTS_AA, list(cutters2015$batterId), length)
names(cutters15.values) <- c("Batter", "w_cutter15")
names(cutters15.pitches) <- c("Batter", "Pitches15")
cutters15.p100 <- merge(cutters15.values, cutters15.pitches)
cutters15.p100$w_cutters15_p100 = round(((cutters15.p100$w_cutter15/cutters15.p100$Pitches15)*100),3)

splitters15.values <- aggregate(splitters2015$bat_LWTS_AA, list(splitters2015$batterId), sum)
splitters15.pitches <- aggregate(splitters2015$bat_LWTS_AA, list(splitters2015$batterId), length)
names(splitters15.values) <- c("Batter", "w_splitter15")
names(splitters15.pitches) <- c("Batter", "Pitches15")
splitters15.p100 <- merge(splitters15.values, splitters15.pitches)
splitters15.p100$w_splitters15_p100 = round(((splitters15.p100$w_splitter15/splitters15.p100$Pitches15)*100),3)

sliders15.values <- aggregate(sliders2015$bat_LWTS_AA, list(sliders2015$batterId), sum)
sliders15.pitches <- aggregate(sliders2015$bat_LWTS_AA, list(sliders2015$batterId), length)
names(sliders15.values) <- c("Batter", "w_slider15")
names(sliders15.pitches) <- c("Batter", "Pitches15")
sliders15.p100 <- merge(sliders15.values, sliders15.pitches)
sliders15.p100$w_sliders15_p100 = round(((sliders15.p100$w_slider15/sliders15.p100$Pitches15)*100),3)

curves15.values <- aggregate(curves2015$bat_LWTS_AA, list(curves2015$batterId), sum)
curves15.pitches <- aggregate(curves2015$bat_LWTS_AA, list(curves2015$batterId), length)
names(curves15.values) <- c("Batter", "w_curve15")
names(curves15.pitches) <- c("Batter", "Pitches15")
curves15.p100 <- merge(curves15.values, curves15.pitches)
curves15.p100$w_curves15_p100 = round(((curves15.p100$w_curve15/curves15.p100$Pitches15)*100),3)

changeups15.values <- aggregate(changeups2015$bat_LWTS_AA, list(changeups2015$batterId), sum)
changeups15.pitches <- aggregate(changeups2015$bat_LWTS_AA, list(changeups2015$batterId), length)
names(changeups15.values) <- c("Batter", "w_change15")
names(changeups15.pitches) <- c("Batter", "Pitches15")
changeups15.p100 <- merge(changeups15.values, changeups15.pitches)
changeups15.p100$w_changeups15_p100 = round(((changeups15.p100$w_change15/changeups15.p100$Pitches15)*100),3)

knucklers15.values <- aggregate(knucklers2015$bat_LWTS_AA, list(knucklers2015$batterId), sum)
knucklers15.pitches <- aggregate(knucklers2015$bat_LWTS_AA, list(knucklers2015$batterId), length)
names(knucklers15.values) <- c("Batter", "w_knuckle15")
names(knucklers15.pitches) <- c("Batter", "Pitches15")
knucklers15.p100 <- merge(knucklers15.values, knucklers15.pitches)
knucklers15.p100$w_knucklers15_p100 = round(((knucklers15.p100$w_knuckle15/knucklers15.p100$Pitches15)*100),3)

fastballs2015.values <- aggregate(fastballs2015$bat_LWTS_AA, list(fastballs2015$batterId), sum)
fastballs2015.pitches <- aggregate(fastballs2015$bat_LWTS_AA, list(fastballs2015$batterId), length)
names(fastballs2015.values) <- c("Batter", "w_fastball15")
names(fastballs2015.pitches) <- c("Batter", "Pitches15")
fastballs2015.p100 <- merge(fastballs2015.values, fastballs2015.pitches)
fastballs2015.p100$w_fastball15_p100 = round(((fastballs2015.p100$w_fastball15/fastballs2015.p100$Pitches15)*100),3)

breakballs2015.values <- aggregate(breakballs2015$bat_LWTS_AA, list(breakballs2015$batterId), sum)
breakballs2015.pitches <- aggregate(breakballs2015$bat_LWTS_AA, list(breakballs2015$batterId), length)
names(breakballs2015.values) <- c("Batter", "w_breakball15")
names(breakballs2015.pitches) <- c("Batter", "Pitches15")
breakballs2015.p100 <- merge(breakballs2015.values, breakballs2015.pitches)
breakballs2015.p100$w_breakball15_p100 = round(((breakballs2015.p100$w_breakball15/breakballs2015.p100$Pitches15)*100),3)

offspeeds2015.values <- aggregate(offspeeds2015$bat_LWTS_AA, list(offspeeds2015$batterId), sum)
offspeeds2015.pitches <- aggregate(offspeeds2015$bat_LWTS_AA, list(offspeeds2015$batterId), length)
names(offspeeds2015.values) <- c("Batter", "w_offspeed15")
names(offspeeds2015.pitches) <- c("Batter", "Pitches15")
offspeeds2015.p100 <- merge(offspeeds2015.values, offspeeds2015.pitches)
offspeeds2015.p100$w_offspeed15_p100 = round(((offspeeds2015.p100$w_offspeed15/offspeeds2015.p100$Pitches15)*100),3)
##### MAKE SURE YOU RAN ON ALL THREE SEASONS #####

#### Create 2015 cLWTS Batter Leaderboard ####
cLWTS15 = subset(allpitches2015.p100)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'AllPitches'
cLWTS15 = merge(cLWTS15, fourseamers15.p100, by = "Batter", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'FourSeamers'
cLWTS15 = merge(cLWTS15, twoseamers15.p100, by = "Batter", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'TwoSeamers'
cLWTS15 = merge(cLWTS15, cutters15.p100, by = "Batter", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Cutters'
cLWTS15 = merge(cLWTS15, splitters15.p100, by = "Batter", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Splitters'
cLWTS15 = merge(cLWTS15, sliders15.p100, by = "Batter", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Sliders'
cLWTS15 = merge(cLWTS15, curves15.p100, by = "Batter", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Curves'
cLWTS15 = merge(cLWTS15, changeups15.p100, by = "Batter", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Changeups'
cLWTS15 = merge(cLWTS15, knucklers15.p100, by = "Batter", all.x = TRUE)
names(cLWTS15)[names(cLWTS15) == 'Pitches15'] <- 'Knuckleballs'
names(cLWTS15)[names(cLWTS15) == 'w_pitches15'] <- 'wALL'
names(cLWTS15)[names(cLWTS15) == 'w_pitches15_p100'] <- 'wALL_100'
names(cLWTS15)[names(cLWTS15) == 'w_4seamer15'] <- 'wFF'
names(cLWTS15)[names(cLWTS15) == 'w_4seamer15_p100'] <- 'wFF_100'
names(cLWTS15)[names(cLWTS15) == 'w_2seamer15'] <- 'wFT'
names(cLWTS15)[names(cLWTS15) == 'w_2seamer15_p100'] <- 'wFT_100'
names(cLWTS15)[names(cLWTS15) == 'w_cutter15'] <- 'wFC'
names(cLWTS15)[names(cLWTS15) == 'w_cutters15_p100'] <- 'wFC_100'
names(cLWTS15)[names(cLWTS15) == 'w_splitter15'] <- 'wFS'
names(cLWTS15)[names(cLWTS15) == 'w_splitters15_p100'] <- 'wFS_100'
names(cLWTS15)[names(cLWTS15) == 'w_slider15'] <- 'wSL'
names(cLWTS15)[names(cLWTS15) == 'w_sliders15_p100'] <- 'wSL_100'
names(cLWTS15)[names(cLWTS15) == 'w_curve15'] <- 'wCU'
names(cLWTS15)[names(cLWTS15) == 'w_curves15_p100'] <- 'wCU_100'
names(cLWTS15)[names(cLWTS15) == 'w_change15'] <- 'wCH'
names(cLWTS15)[names(cLWTS15) == 'w_changeups15_p100'] <- 'wCH_100'
names(cLWTS15)[names(cLWTS15) == 'w_knuckle15'] <- 'wKN'
names(cLWTS15)[names(cLWTS15) == 'w_knucklers15_p100'] <- 'wKN_100'
cLWTS15$Year = 2015
cLWTS15$Name = master_id$mlb_name[match(cLWTS15$Batter, master_id$mlb_id)]
cLWTS15 <- cLWTS15[,c("Name","Batter", "Year", "wALL", "wFF", "wFT", "wFC", "wFS", "wSL", "wCU", "wCH", "wKN", "wALL_100",
                      "wFF_100", "wFT_100", "wFC_100", "wFS_100", "wSL_100", "wCU_100", "wCH_100", "wKN_100",
                      "AllPitches", "FourSeamers", "TwoSeamers", "Cutters", "Splitters", "Sliders", "Curves", 
                      "Changeups", "Knuckleballs")]
cLWTS15 = cLWTS15[(order(-cLWTS15$wALL)), ]        

#### Create 2014 cLWTS Batter Leaderboard ####
cLWTS14 = subset(allpitches2014.p100)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'AllPitches'
cLWTS14 = merge(cLWTS14, fourseamers14.p100, by = "Batter", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'FourSeamers'
cLWTS14 = merge(cLWTS14, twoseamers14.p100, by = "Batter", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'TwoSeamers'
cLWTS14 = merge(cLWTS14, cutters14.p100, by = "Batter", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Cutters'
cLWTS14 = merge(cLWTS14, splitters14.p100, by = "Batter", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Splitters'
cLWTS14 = merge(cLWTS14, sliders14.p100, by = "Batter", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Sliders'
cLWTS14 = merge(cLWTS14, curves14.p100, by = "Batter", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Curves'
cLWTS14 = merge(cLWTS14, changeups14.p100, by = "Batter", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Changeups'
cLWTS14 = merge(cLWTS14, knucklers14.p100, by = "Batter", all.x = TRUE)
names(cLWTS14)[names(cLWTS14) == 'Pitches14'] <- 'Knuckleballs'
names(cLWTS14)[names(cLWTS14) == 'w_pitches14'] <- 'wALL'
names(cLWTS14)[names(cLWTS14) == 'w_pitches14_p100'] <- 'wALL_100'
names(cLWTS14)[names(cLWTS14) == 'w_4seamer14'] <- 'wFF'
names(cLWTS14)[names(cLWTS14) == 'w_4seamer14_p100'] <- 'wFF_100'
names(cLWTS14)[names(cLWTS14) == 'w_2seamer14'] <- 'wFT'
names(cLWTS14)[names(cLWTS14) == 'w_2seamer14_p100'] <- 'wFT_100'
names(cLWTS14)[names(cLWTS14) == 'w_cutter14'] <- 'wFC'
names(cLWTS14)[names(cLWTS14) == 'w_cutters14_p100'] <- 'wFC_100'
names(cLWTS14)[names(cLWTS14) == 'w_splitter14'] <- 'wFS'
names(cLWTS14)[names(cLWTS14) == 'w_splitters14_p100'] <- 'wFS_100'
names(cLWTS14)[names(cLWTS14) == 'w_slider14'] <- 'wSL'
names(cLWTS14)[names(cLWTS14) == 'w_sliders14_p100'] <- 'wSL_100'
names(cLWTS14)[names(cLWTS14) == 'w_curve14'] <- 'wCU'
names(cLWTS14)[names(cLWTS14) == 'w_curves14_p100'] <- 'wCU_100'
names(cLWTS14)[names(cLWTS14) == 'w_change14'] <- 'wCH'
names(cLWTS14)[names(cLWTS14) == 'w_changeups14_p100'] <- 'wCH_100'
names(cLWTS14)[names(cLWTS14) == 'w_knuckle14'] <- 'wKN'
names(cLWTS14)[names(cLWTS14) == 'w_knucklers14_p100'] <- 'wKN_100'
cLWTS14$Year = 2014
cLWTS14$Name = master_id$mlb_name[match(cLWTS14$Batter, master_id$mlb_id)]
cLWTS14 <- cLWTS14[,c("Name","Batter", "Year", "wALL", "wFF", "wFT", "wFC", "wFS", "wSL", "wCU", "wCH", "wKN", "wALL_100",
                      "wFF_100", "wFT_100", "wFC_100", "wFS_100", "wSL_100", "wCU_100", "wCH_100", "wKN_100",
                      "AllPitches", "FourSeamers", "TwoSeamers", "Cutters", "Splitters", "Sliders", "Curves", 
                      "Changeups", "Knuckleballs")]
cLWTS14 = cLWTS14[(order(-cLWTS14$wALL)), ]        

#### Create 2013 cLWTS Batter Leaderboard ####
cLWTS13 = subset(allpitches2013.p100)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'AllPitches'
cLWTS13 = merge(cLWTS13, fourseamers13.p100, by = "Batter", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'FourSeamers'
cLWTS13 = merge(cLWTS13, twoseamers13.p100, by = "Batter", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'TwoSeamers'
cLWTS13 = merge(cLWTS13, cutters13.p100, by = "Batter", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Cutters'
cLWTS13 = merge(cLWTS13, splitters13.p100, by = "Batter", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Splitters'
cLWTS13 = merge(cLWTS13, sliders13.p100, by = "Batter", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Sliders'
cLWTS13 = merge(cLWTS13, curves13.p100, by = "Batter", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Curves'
cLWTS13 = merge(cLWTS13, changeups13.p100, by = "Batter", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Changeups'
cLWTS13 = merge(cLWTS13, knucklers13.p100, by = "Batter", all.x = TRUE)
names(cLWTS13)[names(cLWTS13) == 'Pitches13'] <- 'Knuckleballs'
names(cLWTS13)[names(cLWTS13) == 'w_pitches13'] <- 'wALL'
names(cLWTS13)[names(cLWTS13) == 'w_pitches13_p100'] <- 'wALL_100'
names(cLWTS13)[names(cLWTS13) == 'w_4seamer13'] <- 'wFF'
names(cLWTS13)[names(cLWTS13) == 'w_4seamer13_p100'] <- 'wFF_100'
names(cLWTS13)[names(cLWTS13) == 'w_2seamer13'] <- 'wFT'
names(cLWTS13)[names(cLWTS13) == 'w_2seamer13_p100'] <- 'wFT_100'
names(cLWTS13)[names(cLWTS13) == 'w_cutter13'] <- 'wFC'
names(cLWTS13)[names(cLWTS13) == 'w_cutters13_p100'] <- 'wFC_100'
names(cLWTS13)[names(cLWTS13) == 'w_splitter13'] <- 'wFS'
names(cLWTS13)[names(cLWTS13) == 'w_splitters13_p100'] <- 'wFS_100'
names(cLWTS13)[names(cLWTS13) == 'w_slider13'] <- 'wSL'
names(cLWTS13)[names(cLWTS13) == 'w_sliders13_p100'] <- 'wSL_100'
names(cLWTS13)[names(cLWTS13) == 'w_curve13'] <- 'wCU'
names(cLWTS13)[names(cLWTS13) == 'w_curves13_p100'] <- 'wCU_100'
names(cLWTS13)[names(cLWTS13) == 'w_change13'] <- 'wCH'
names(cLWTS13)[names(cLWTS13) == 'w_changeups13_p100'] <- 'wCH_100'
names(cLWTS13)[names(cLWTS13) == 'w_knuckle13'] <- 'wKN'
names(cLWTS13)[names(cLWTS13) == 'w_knucklers13_p100'] <- 'wKN_100'
cLWTS13$Year = 2013
cLWTS13$Name = master_id$mlb_name[match(cLWTS13$Batter, master_id$mlb_id)]
cLWTS13 <- cLWTS13[,c("Name","Batter", "Year", "wALL", "wFF", "wFT", "wFC", "wFS", "wSL", "wCU", "wCH", "wKN", "wALL_100",
                      "wFF_100", "wFT_100", "wFC_100", "wFS_100", "wSL_100", "wCU_100", "wCH_100", "wKN_100",
                      "AllPitches", "FourSeamers", "TwoSeamers", "Cutters", "Splitters", "Sliders", "Curves", 
                      "Changeups", "Knuckleballs")]
cLWTS13 = cLWTS13[(order(-cLWTS13$wALL)), ]        
### Merge three leaderboards together for Tabluea ###
cLWTS_Batter = rbind(cLWTS15, cLWTS14)
cLWTS_Batter = rbind(cLWTS_Batter, cLWTS13)
write.csv(cLWTS_Batter, file = "cLWTS_Batter.csv")









