setwd("C:/Users/iandb/OneDrive/Documents/R/")
Footy <- read.csv("EPL 1819.csv", header = TRUE, sep = ",")

library(ggplot2)
library(ggalt)
library(dplyr)
library(data.table)

#Extracts the max and min average odds for home teams, and groups by the Home Team
EPL <- Footy %>%
  group_by(HomeTeam) %>%
  summarise(Avmax=max(BbAvH), Avmin=min(BbAvH))
#Adds new column to DF, then adds Final league position for each team
EPL <- mutate(EPL, FinalPos=rownames(EPL))
setDT(EPL)[HomeTeam =="Man City", FinalPos:= "1"]
setDT(EPL)[HomeTeam =="Man United", FinalPos:= "8"]
setDT(EPL)[HomeTeam =="Tottenham", FinalPos:= "4"]
setDT(EPL)[HomeTeam =="Liverpool", FinalPos:= "2"]
setDT(EPL)[HomeTeam =="Chelsea", FinalPos:= "3"]
setDT(EPL)[HomeTeam =="Arsenal", FinalPos:= "5"]
setDT(EPL)[HomeTeam =="Burnley", FinalPos:= "15"]
setDT(EPL)[HomeTeam =="Everton", FinalPos:= "9"]
setDT(EPL)[HomeTeam =="Leicester", FinalPos:= "10"]
setDT(EPL)[HomeTeam =="Newcastle", FinalPos:= "14"]
setDT(EPL)[HomeTeam =="Crystal Palace", FinalPos:= "16"]
setDT(EPL)[HomeTeam =="Bournemouth", FinalPos:= "6"]
setDT(EPL)[HomeTeam =="West Ham", FinalPos:= "13"]
setDT(EPL)[HomeTeam =="Watford", FinalPos:= "7"]
setDT(EPL)[HomeTeam =="Brighton", FinalPos:= "12"]
setDT(EPL)[HomeTeam =="Huddersfield", FinalPos:= "19"]
setDT(EPL)[HomeTeam =="Southampton", FinalPos:= "17"]
setDT(EPL)[HomeTeam =="Wolves", FinalPos:= "11"]
setDT(EPL)[HomeTeam =="Fulham", FinalPos:= "20"]
setDT(EPL)[HomeTeam =="Cardiff", FinalPos:= "18"]

EPL$FinalPos <- as.numeric(as.character(EPL$FinalPos))

#Sorts teams into final position 
EPL <- EPL[order(FinalPos),]
EPL$HomeTeam <- factor(EPL$HomeTeam, as.character(EPL$HomeTeam))

#Plots chart
gg <- ggplot(EPL, aes(x=EPL$Avmax, xend= EPL$Avmin, y = EPL$HomeTeam))
gg <- gg + geom_dumbbell(colour="#00ff85",
                         colour_x = "#38003c",
                         colour_xend ="#38003c",
                         size_x = 2.5,
                         size_xend = 2.5,
                         size=1)
gg <- gg + scale_y_discrete(limits = rev(levels(as.factor(EPL$HomeTeam))))
gg <- gg + labs(x="Average Home Win Odds", y = NULL, title = "Spread of Home Win Odds", caption = "Data from football-data.co.uk", colour = "#04f5ff")
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.title.x=element_text(hjust=1, face="italic", margin=margin(t=-24)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=24)))
gg <- gg + theme(axis.text = element_text(colour = "#e90052"))
gg

#Sorts by AVmin, adds final position for reference
EPL <- EPL[order(Avmin),]
EPL$HomeTeam <- factor(EPL$HomeTeam, as.character(EPL$HomeTeam))
EPL$HomeTeam <- paste(EPL$HomeTeam, "(", EPL$FinalPos,")")
EPL$HomeTeam <- factor(EPL$HomeTeam, as.character(EPL$HomeTeam))

