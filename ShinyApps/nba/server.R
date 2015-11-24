

library(shiny)
library(XML)
library(RMySQL)
library(data.table)
library(stringr)


shinyServer(function(input, output) {
  
  
  
  theurl <- "http://www.nba.com/standings/team_record_comparison/conferenceNew_Std_Div.html"
  tablesset <- readHTMLTable(theurl)
  
  
  espn <- 'http://espn.go.com/nba/statistics/team/_/stat/team-comparison-per-game/sort/avgPoints'
  tablesset.espn <- readHTMLTable(espn)
  tablesset.espn <- data.table(tablesset.espn[[1]])
  
  
  tablesset.espn <- tablesset.espn[is.na(V8)==F]
  tablesset.espn <- tablesset.espn[(V2) != 'TEAM']
  tablesset.espn <- tablesset.espn[,list(team=as.character(V2), diff=V5)]
  tablesset.espn[,pos_neg:=substring(diff,1,1)]
  tablesset.espn[,value:=as.numeric(ifelse(pos_neg=='0', str_sub(diff,1),str_sub(diff,2))) ]
  tablesset.espn[,pos_neg:=ifelse(pos_neg=='+',1,-1)]
  tablesset.espn[,value:=(value*pos_neg)]
  tablesset.espn <- tablesset.espn[,list(team,value)]
  tablesset.espn[,team:=ifelse(team=='LA Clippers','L.A. Clippers', ifelse(team== 'LA Lakers', 'L.A. Lakers', team))]
  tablesset.espn[,estimated_wins:=82 * (1 / (1 + exp(-0.13959 * value))) ]
  
  
  east <- data.table(tablesset[[1]])
  east <- east[is.na(V2)==F]
  east <- east[(V2)!='W']
  east <- east[,list(V1=as.character(V1),V2,V3)]
  east[,num:=as.numeric(substring(V1, nchar(V1)))]
  east[,new_name:=ifelse(is.na(num), V1, substring(V1, 1, nchar(V1)-1))]
  east[,V1:=new_name]
  east <- east[,list(V1=as.character(V1),V2,V3)]
  setnames(east, c('team','wins','losses'))
  
  bids <- read.csv('/home/ubuntu/over_under_draft.csv')
  bids <- data.table(merge(bids, east, by='team'))
  bids <- bids[,list(team=as.character(team), 
                     Team2=as.character(Team), 
                     Vegas, 
                     CBS, 
                     ESPN=as.numeric(as.character(ESPN)), 
                     Average=as.numeric(as.character(Average)), 
                     Over=(as.character(Over)), 
                     Under=(as.character(Under)),
                     actual_wins=as.numeric(as.character(wins)), 
                     actual_losses=as.numeric(as.character(losses)),
                     timestamp=as.character(Sys.time()))]
  
  bids[,estimated_wins:=ifelse(actual_wins+actual_losses ==0, 0, actual_wins*82/(actual_wins+actual_losses))]
  bids[,estimated_under_over_winner:=ifelse(estimated_wins < Average, 'Under', 'Over')]
  bids[,estimated_winner:=ifelse(estimated_wins < Average, Under, Over)]
  bids[,estimated_loser:=ifelse(estimated_wins < Average, Over, Under)]
  bids[,estimated_winnings:=ifelse(estimated_wins < Average, (Average-estimated_wins)*3, (estimated_wins-Average)*3)]
  bids[,estimated_losses:=ifelse(Average < estimated_wins, (Average-estimated_wins)*3, (estimated_wins-Average)*3)]
  
  
  #sqlFromRObj(getLstConn()$conn_nav4, bids,  'furman_5374.nba')
  
  total <- merge(bids[,list(winnings=sum(estimated_winnings)), by=list(participant=estimated_winner)], bids[,list(losses=sum(estimated_losses)), by=list(participant=estimated_loser)], by='participant')
  total[,total:=winnings+losses]
  total <- total[order(-total)]
  
  
  
  
  bids.espn <- read.csv('/home/ubuntu/over_under_draft.csv')
  bids.espn <- data.table(merge(bids.espn, east, by='team'))
  bids.espn <- bids.espn[,list(team=as.character(team), 
                               Team2=as.character(Team), 
                               Vegas, 
                               CBS, 
                               ESPN=as.numeric(as.character(ESPN)), 
                               Average=as.numeric(as.character(Average)), 
                               Over=(as.character(Over)), 
                               Under=(as.character(Under)),
                               actual_wins=as.numeric(as.character(wins)), 
                               actual_losses=as.numeric(as.character(losses)),
                               timestamp=as.character(Sys.time()))]
  
  bids.espn <- merge(bids.espn, tablesset.espn, by='team')
  bids.espn[,estimated_under_over_winner:=ifelse(estimated_wins < Average, 'Under', 'Over')]
  bids.espn[,estimated_winner:=ifelse(estimated_wins < Average, Under, Over)]
  bids.espn[,estimated_loser:=ifelse(estimated_wins < Average, Over, Under)]
  bids.espn[,estimated_winnings:=ifelse(estimated_wins < Average, (Average-estimated_wins)*3, (estimated_wins-Average)*3)]
  bids.espn[,estimated_losses:=ifelse(Average < estimated_wins, (Average-estimated_wins)*3, (estimated_wins-Average)*3)]
  
  total.espn <- merge(bids.espn[,list(winnings=sum(estimated_winnings)), by=list(participant=estimated_winner)], bids.espn[,list(losses=sum(estimated_losses)), by=list(participant=estimated_loser)], by='participant')
  total.espn[,total:=winnings+losses]
  total.espn <- total.espn[order(-total)]
  
  total_all <- merge(total, total.espn, by='participant')[,list(participant, 
                                                                winnings_using_percent=round(total.x), 
                                                                dumb_todd_way=round(total.y))][order(-winnings_using_percent)]
  
  
  
  
  
  
  # display 10 rows initially
  output$ex1 <- renderDataTable(total_all)
  
  # -1 means no pagination; the 2nd element contains menu labels
  output$ex2 <- renderDataTable(bids,options = list(paging = FALSE))
  
 
  # write literal JS code in I()

})

