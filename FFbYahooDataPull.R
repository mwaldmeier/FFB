library("XML")
library("stringr")
library("plyr")

#converts a DF to CSV 
toCSV <- function(dataOut, fileName) {
  fileName <- paste(fileName, ".csv", sep="")
  write.csv(dataOut, file=fileName)
  print(paste(fileName, "created."))
}

GetYahooData <- function(year, week){
  #all positions
  playerList <- c("QB", "RB", "WR", "TE","K", "DEF")
  #set the last page per positon
  pageCnt <- c(125, 275, 425, 225, 50, 25)
  #use this for testing, pulls small sample
  #pageCnt <- c(25, 25, 25, 25, 25, 25)
  #cols to be dropped
  drops <- c("drop1", "drop2", "drop3")
  
  for (i in 1:length(playerList)) { 
    x <- 1
    repeat{
      if (x==1) {
        out <- paste("http://football.fantasysports.yahoo.com/f1/955145/players?&sort=PR&sdir=1&status=ALL&pos=", playerList[i],"&stat1=S_W_", week, sep="")
      } else {
        out <- paste("http://football.fantasysports.yahoo.com/f1/955145/players?status=ALL&pos=", playerList[i],"&cut_type=9&stat1=S_W_", week,"&myteam=1&sort=PR&sdir=1&count=", x, sep="")
      }
      #pull data from web
      tempPull <- readHTMLTable(out, stringsAsFactors = FALSE)[2]$'NULL'
      #set column names depending on position of players
      if (playerList[i] %in% c("QB", "RB", "WR", "TE")) {
        colnames(tempPull) <- c("drop1", "Player", "drop2", "Owner", "Fan_Pts", "Owned", "Proj", "Actual", "Pass_Yds", "Pass_TD", "Pass_Int", "Pass_sack", "Rush_Att", "Rush_Yds", "Rush_TD", "Rece_Tgt", "Rece_Rec", "Rece_Yds", "Rece_TD", "Ret_Yds", "Ret_TD", "Mis_2PT", "Fum_Lost", "drop3")
      } else if (playerList[i] == "K") {
        colnames(tempPull) <- c("drop1", "Player", "drop2", "Owner", "Fan_Pts", "Owned", "Proj", "Actual", "FG_0_19", "FG_20_29", "FG_30_39", "FG_40_49", "FG_50", "Made", "drop3")
      } else {
        colnames(tempPull) <- c("drop1", "Player", "drop2", "Owner", "Fan_Pts", "Owned", "Proj","Actual", "Tac_Pts_Allow", "Tac_Sack",  "Tac_Safe",  "Tac_Int",  "Trn_Fum_Rec", "TD_TD", "Misc_Blk_Kick", "Yds_alwd", "Def_Ret_TD", "drop3")
      }
      #drop empty columns
      tempPull <- tempPull[, !(names(tempPull) %in% drops)]
      #get opponent
      tempPull$Opt <- str_sub(tempPull$Player, -3,-1)
      #add a test for away game
      tempPull$AwayTest <- str_sub(tempPull$Player, -7,-1)
      #create temp Player string
      tempPull$Player <- str_trim(sapply(str_split(tempPull$Player, "\n"), "[[", 2))
      #get players position
      tempPull$Pos <- str_trim(str_sub(tempPull$Player, start= -2))
      #get players team
      tempPull$Team <- toupper(str_trim(str_sub(tempPull$Player, start=str_locate(tempPull$Player, "-")[,1]-4, end=str_locate(tempPull$Player, "-")[,1]-2)))
      #must be set last
      tempPull$Player<- str_trim(str_sub(tempPull$Player, start=0, end=nchar(tempPull$Player)-8))
      #add week and year of data
      tempPull$Year <- rep(year,nrow(tempPull)) 
      tempPull$Week <- rep(week,nrow(tempPull))
      
      #creat new temp DF if one doesn't exist else union
      if (x > 1) {
        tempCat1 <- rbind(tempCat1, tempPull)
      } else {
        tempCat1 <- tempPull
      }
      #increas page incraments
      if (x == 1) {
        x = 25
      } else {
        x = x + 25
      }   
      if (x > pageCnt[i]){
        break
      }
    }
    #creat new temp DF if one doesn't exist else union
    if (i > 1) {
      tempCat <- rbind.fill(tempCat, tempCat1)
    } else {
      tempCat <- tempCat1
    }
  }
  #convert char to numeric
  cols.num <-c("Fan_Pts", "Proj", "Actual", "Pass_Yds", "Pass_TD", "Pass_Int", "Rush_Att", "Rush_Yds", "Rush_TD", "Rece_Tgt", "Rece_Rec", "Rece_Yds", "Rece_TD", "Ret_TD", "Mis_2PT", "Fum_Lost", "FG_0_19", "FG_20_29", "FG_30_39", "FG_40_49", "FG_50", "Made","Tac_Pts_Allow", "Tac_Sack",  "Tac_Safe",  "Tac_Int",  "Trn_Fum_Rec", "TD_TD", "Misc_Blk_Kick", "Def_Ret_TD")
  tempCat[cols.num] <- sapply(tempCat[cols.num], as.numeric)
  #test if it was an away game
  tempCat$Away <- ifelse(grepl("@", tempCat$AwayTest), 1, 0)
  return(tempCat)
}

yahooWk1 <- GetYahooData(2014, 1)
yahooWk2 <- GetYahooData(2014, 2)
yahooWk3 <- GetYahooData(2014, 3)
yahooWk4 <- GetYahooData(2014, 4)
yahooWk5 <- GetYahooData(2014, 5)
yahooWk6 <- GetYahooData(2014, 6)
yahooWk7 <- GetYahooData(2014, 7)
yahooWk8 <- GetYahooData(2014, 8)

#union all weeks
yahooAllWks <- rbind(yahooWk1,yahooWk2,yahooWk3,yahooWk4,yahooWk5,yahooWk6,yahooWk7,yahooWk8)


toCSV(yahooWk1, "yahooWk1")
toCSV(yahooWk2, "yahooWk2")
toCSV(yahooWk3, "yahooWk3")
toCSV(yahooWk4, "yahooWk4")
toCSV(yahooWk5, "yahooWk5")
toCSV(yahooWk6, "yahooWk6")
toCSV(yahooWk7, "yahooWk7")
toCSV(yahooWk8, "yahooWk8")
toCSV(yahooAllWks, "yahooAllWks")