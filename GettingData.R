library(tidyverse)
library(rvest)
## Get NBA draft data


offset <- seq(100, 1000, by = 100)
Data <- list()
for(i in 1:(length(offset) + 1)){
  if(i == 1){
    NBADraftData <- read_html("https://www.basketball-reference.com/play-index/draft_finder.cgi?request=1&year_min=1985&college_id=0&pick_overall_min=1&pick_overall_max=30&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&order_by=year_id") %>%  html_nodes("#stats") %>% html_table()
  }
  else{
    NBADraftData <- read_html(paste0("https://www.basketball-reference.com/play-index/draft_finder.cgi?request=1&year_min=1985&year_max=&round_min=&round_max=&pick_overall_min=1&pick_overall_max=30&franch_id=&college_id=0&is_active=&is_hof=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&c1stat=&c1comp=&c1val=&c2stat=&c2comp=&c2val=&c3stat=&c3comp=&c3val=&c4stat=&c4comp=&c4val=&order_by=year_id&order_by_asc=&offset=",offset[i-1])) %>%  html_nodes("#stats") %>% html_table()
  }
  colnames(NBADraftData[[1]]) <- NBADraftData[[1]][1,]
  NBADraftData[[1]] <- NBADraftData[[1]][-1,]
  
  Data[[i]] <- NBADraftData[[1]] %>% dplyr::filter(Rk != "" & Rk != "Rk") %>% mutate_at(c("Rk", "Year", "Rd", "Pk", "Age", "From", "To", "G", "MP", "PTS", "TRB", "AST", "STL", "BLK", "FG%", "2P%", "3P%", "FT%", "WS", "WS/48"), as.numeric)
  message(paste(i, "of", length(offset + 1), "ready!!"))
}

Data <- bind_rows(Data)

saveRDS(Data, "DraftData.rds")

TeamCode <- unique(Data$Tm)

TeamData <- list()

for(i in 1:length(TeamCode)){
  
  Temp <-  read_html(paste0("https://www.basketball-reference.com/teams/", TeamCode[i],"/stats_per_game_totals.html")) %>%  html_nodes("#stats") %>% html_table() 
  try(TeamData[[i]] <- Temp[[1]][,-c(7,11)] %>% dplyr::filter(W != "W") %>% mutate(Season = str_replace(Season,"\\-[0-9][0-9]","")) %>% mutate_at(c("Season","W", "L", "Finish", "Age", "Wt.","G", "MP", "FG", "FGA", "FG%", "3P", "3PA", "3P%", "2P", "2PA", "2P%", "FT", "FTA", "FT%", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS"), as.numeric))
  message(paste(i, "of", length(TeamCode), "ready!!"))
}


cond <- sapply(TeamData, function(x) !is.null(x))
TeamData <- TeamData[cond]

TeamData2 <- bind_rows(TeamData) %>% rename(Year = Season) %>% select(-Age, -Ht., -Wt., -G, -MP, -`FG%`, -`2P%`,-`3P%`, -`3P%`, -`FT%`, -TRB, -AST, -STL, -BLK, -PTS)
Data2 <- Data %>% left_join(TeamData2)
saveRDS(Data2, "Data.rds")

DataByTeam <- Data2 %>% group_split(Tm) %>% purrr::map(~arrange(.x, desc(Year)))
