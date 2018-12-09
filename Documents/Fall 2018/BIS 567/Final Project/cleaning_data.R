### Create Training Data Set
library(dplyr)
library(readr)
library(stringdist)
file_names <- c(dir(path = "pbp_2016_17", pattern = "*.csv",full.names = T),
                dir(path = "pbp_2017_18", pattern = "*.csv",full.names = T))

train <- read_csv(file_names[1], col_types = cols(play_id = "i",
                                                  half = "i",
                                                  time_remaining_half = "c",
                                                  secs_remaining = "i",
                                                  description = "c",
                                                  home_score = "i",
                                                  away_score = "i",
                                                  away = "c",
                                                  home = "c",
                                                  home_favored_by = "d",
                                                  game_id = "i")) %>%
  mutate("year" = 2016)


for(i in 2:length(file_names)) {
  cat("Reading File: ", i, "\n")
  train <- bind_rows(train,   read_csv(file_names[i], col_types = cols(play_id = "i",
                                                                       half = "i",
                                                                       time_remaining_half = "c",
                                                                       secs_remaining = "i",
                                                                       description = "c",
                                                                       home_score = "i",
                                                                       away_score = "i",
                                                                       away = "c",
                                                                       home = "c",
                                                                       home_favored_by = "d",
                                                                       game_id = "i")) %>%
                       mutate(year = ifelse(grepl("2016", file_names[i]), 2016, 2017)))
}

train <- filter(train, !duplicated(paste(game_id, play_id)))
write.csv(train, "train.csv", row.names = F)

x <- filter(train, game_id == game_ids[1])

train <- read.csv("train.csv", as.is = T)
game_ids <- unique(train$game_id)
dict <- read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops_Play_By_Play/2da84515a6ffdb48a553acbfa2989862a9fbce84/ESPN_NCAA_Dict.csv", as.is = T)
games_2016 <- read.csv("NCAA_Hoops_Results_2017_Final.csv", as.is = T)
games_2017 <- read.csv("NCAA_Hoops_Results_2018_Final.csv", as.is = T)

train <- full_join(train, select(dict, NCAA, ESPN_PBP),  by = c("home" = "ESPN_PBP")) %>%
  full_join(select(dict, NCAA, ESPN_PBP), by = c("away" = "ESPN_PBP")) %>%
  rename(home_ncaa = NCAA.x, away_ncaa = NCAA.y)


### Line Function
get_line <- function(ID) {
  tmp <- filter(train, game_id == ID)
  if(is.na(tmp$home_favored_by[1])) {
    if(is.na(tmp$home_ncaa[1]) | is.na(tmp$away_ncaa[1])) {
      tmp$home_favored_by <- 0 
    }else{
      if(tmp$year[1] == 2016) {
        game <- filter(games_2016, team == tmp$home_ncaa[1], 
                       opponent == tmp$away_ncaa[1], 
                       location == "H")
        if(nrow(game) == 0) {
          game <- filter(games_2016, team == tmp$home_ncaa[1], 
                         opponent == tmp$away_ncaa[1], 
                         location == "N")
        }
        favored_by <- game$predscorediff[1]
        
      }
      else if(tmp$year[1] == 2017) {
        game <- filter(games_2017, team == tmp$home_ncaa[1], 
                       opponent == tmp$away_ncaa[1], 
                       location == "H")
        if(nrow(game) == 0) {
          game <- filter(games_2017, team == tmp$home_ncaa[1], 
                         opponent == tmp$away_ncaa[1], 
                         location == "N")
        }
        favored_by <- game$pred_score_diff[1]
        
      }
    }
  }
  else{
    favored_by <- tmp$home_favored_by[1] 
  }
  return(ifelse(is.na(favored_by), 0, favored_by))
}

lines <- sapply(game_ids, get_line)

### Get Score Diff
n <- length(game_ids)
for(i in 1:n) {
  cat("Cleaning Game:", i, "of", n, "\n")
  tmp <- filter(train, game_id == game_ids[i])
  
  ### Line
  tmp$home_favored_by <- lines[i]
  
  ### Seconds
  msec <- max(tmp$secs_remaining)
  
  if(msec > 2400 & msec <= 2700) {
    tmp$secs_remaining[tmp$secs_remaining >= 300] <- 
      tmp$secs_remaining[tmp$secs_remaining >= 300] - 300
  }else if(msec > 2700 & msec <= 3000) {
    tmp$secs_remaining[tmp$secs_remaining >= 600] <- tmp$secs_remaining[tmp$secs_remaining >= 600] - 600
    tmp$secs_remaining[tmp$secs_remaining >= 300 & tmp$secs_remaining < 600] <- 
      tmp$secs_remaining[tmp$secs_remaining >= 300 & tmp$secs_remaining < 600] - 300
  }else if(msec > 3000 & msec <= 3300){
    tmp$secs_remaining[tmp$secs_remaining >= 900] <- tmp$secs_remaining[tmp$secs_remaining >= 900] - 900
    tmp$secs_remaining[tmp$secs_remaining >= 600 & tmp$secs_remaining < 900] <- 
      tmp$secs_remaining[tmp$secs_remaining >= 600 & tmp$secs_remaining < 900] - 600
    tmp$secs_remaining[tmp$secs_remaining >= 300 & tmp$secs_remaining < 600] <- 
      tmp$secs_remaining[tmp$secs_remaining >= 300 & tmp$secs_remaining < 600] - 300
  }else if(msec > 3300 & msec <= 3600){
    tmp$secs_remaining[tmp$secs_remaining >= 1200] <- tmp$secs_remaining[tmp$secs_remaining >= 1200] - 1200
    tmp$secs_remaining[tmp$secs_remaining >= 900 & tmp$secs_remaining < 1200] <- 
      tmp$secs_remaining[tmp$secs_remaining >= 900 & tmp$secs_remaining < 1200] - 900
    tmp$secs_remaining[tmp$secs_remaining >= 600 & tmp$secs_remaining < 900] <- 
      tmp$secs_remaining[tmp$secs_remaining >= 600 & tmp$secs_remaining < 900] - 600
    tmp$secs_remaining[tmp$secs_remaining >= 300 & tmp$secs_remaining < 600] <- 
      tmp$secs_remaining[tmp$secs_remaining >= 300 & tmp$secs_remaining < 600] - 300
  }
  
  timeout <- filter(tmp, sapply(tmp$description, grepl, pattern = "Timeout")) %>%
    filter(description != "Official TV Timeout")
  
  timeout$team <- sapply(timeout$description, function(z) gsub("\\s* Timeout", "", z))
  teams <- unique(timeout$team)
  pos_teams <- c(tmp$home[1], tmp$away[1])
  if(nrow(timeout) > 0) {
    home <- pos_teams[which.min(stringdist(teams, tmp$home[1]))]
    away <- setdiff(pos_teams, home)
  }else{
    home <- pos_teams[1]
    away <- pos_teams[2]
  }
  ### TimeOuts
  tmp$home_time_out_remaining <- 4
  tmp$away_time_out_remaining <- 4
  tmp$home_timeout_ind <- 0
  tmp$away_timeout_ind <- 0
  nplay <- nrow(tmp)
  if(nrow(timeout) > 0) {
    for(j in 1:nrow(timeout)) {
      play_id <- timeout$play_id[j]
      secs_remaining <- timeout$secs_remaining[j]
      half <- timeout$half[j]
      
      if(timeout$team[j] == home) {
        tmp$home_time_out_remaining[play_id:nplay] <- tmp$home_time_out_remaining[play_id:nplay] - 1
        tmp$home_timeout_ind[tmp$secs_remaining <= secs_remaining & tmp$secs_remaining >= secs_remaining - 60
                             & tmp$half == half] <- 1
      }else {
        tmp$away_time_out_remaining[play_id:nplay] <- tmp$away_time_out_remaining[play_id:nplay] - 1
        tmp$away_timeout_ind[tmp$secs_remaining <= secs_remaining & tmp$secs_remaining >= secs_remaining - 60
                             & tmp$half == half] <- 1
      }
      
    }
  }  
  tmp$home_time_out_remaining[tmp$half > 2] <- 
    tmp$home_time_out_remaining[tmp$half > 2] + (tmp$half[tmp$half > 2] - 2)
  tmp$away_time_out_remaining[tmp$half > 2] <- 
    tmp$away_time_out_remaining[tmp$half > 2] + (tmp$half[tmp$half > 2] - 2)
  
  
  if(any(tmp$home_time_out_remaining < 0) | any(tmp$away_time_out_remaining < 0)) {
    tmp$home_time_out_remaining <- tmp$home_time_out_remaining + 2
    tmp$away_time_out_remaining <- tmp$away_time_out_remaining + 2
  }else{  
    if(max(tmp$home_time_out_remaining[tmp$half == 2]) < 4) {
      tmp$home_time_out_remaining[tmp$half >= 2] <- 
        tmp$home_time_out_remaining[tmp$half >= 2] + 1
    }
    if(max(tmp$away_time_out_remaining[tmp$half == 2]) < 4) {
      tmp$away_time_out_remaining[tmp$half >= 2] <- 
        tmp$away_time_out_remaining[tmp$half >= 2] + 1
    }
  }
  
  tmp$win <- ifelse(tmp$home_score[nplay] > tmp$away_score[nplay], 1, 0)
  
  if(i == 1) {
    train_clean <- tmp
  }else{
    train_clean <- bind_rows(train_clean, tmp)
  }
}

write.csv(train_clean, "train_clean.csv", row.names = F)

for(i in 1:n) {
  cat("Cleaning Game:", i, "of", n, "\n")
  y <- filter(train_clean, game_id == game_ids[i]) 
  y$score_diff <- y$home_score - y$away_score
  y$home <- 1
  
  y <- select(y, win, score_diff, home_favored_by,
              secs_remaining, home_time_out_remaining, home_timeout_ind, home) %>%
    rename(favored_by = home_favored_by, timeout_remaining = home_time_out_remaining,
           timeout_ind = home_timeout_ind)
  
  z <- filter(train_clean, game_id == game_ids[i]) 
  z$score_diff <- z$away_score - z$home_score
  z$home_favored_by <- -z$home_favored_by
  z$win <- abs(1-z$win)
  z$home <- 0
  
  z <- select(z, win, score_diff, home_favored_by,
              secs_remaining, away_time_out_remaining, away_timeout_ind, home) %>%
    rename(favored_by = home_favored_by, timeout_remaining = away_time_out_remaining,
           timeout_ind = away_timeout_ind)
  
  t <- bind_rows(y,z)
  
  if(i == 1) {
    x <- t
  }else{
    x <- bind_rows(x,t) 
  }
}

write.csv(x, "matrix_data.csv", row.names = F)
x <- read.csv("matrix_data.csv", as.is = T)
