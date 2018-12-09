library(dplyr)
library(ggplot2)
library(tidyr)
library(ncaahoopR)

file_names <- c(dir(path = "mcmc_draws", pattern = "*.csv", full.names = T))
times <- unname(sapply(file_names, function(x) {as.numeric(unlist(strsplit(gsub("\\..*", "", x), "_"))[3:4])}))
directory <- data.frame("file" = file_names,
                        "min_time" = times[1,],
                        "max_time" = times[2,],
                        stringsAsFactors = F) %>%
  arrange(desc(min_time)) %>%
  mutate(midpoint_time = 0.5 * (max_time + min_time))


### Posterior Distributions for a single time points
beta <- read.csv(directory$file[1], as.is = T)[-c(1:1000),] %>%
  select(-secs_remaining, -intercept)
x <- gather(beta, coefficient, value)

ggplot(x, aes(x = value, fill = coefficient)) + 
  facet_wrap(~coefficient, ncol = 2, scales = "free") +
  geom_histogram(bins = 100) + 
  theme_bw() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "none") + 
  labs(x = "Value", 
       y = "Count", 
       title = "Posterior Distributions",
       subtitle = "Time Slice 1: (2300, 2400] Seconds Remaining")


beta_master <- read.csv(directory$file[1], as.is = T)[-c(1:1000),] %>%
  mutate(time = directory$max_time[1]) %>% select(score_diff, favored_by, timeout_remaining,
                                                  timeout_ind, home, time)
for(i in 2:length(file_names)) {
  beta_master <- rbind(beta_master, read.csv(directory$file[i], as.is = T)[-c(1:1000),] %>%
                         mutate(time = directory$max_time[i]) 
                       %>% select(score_diff, favored_by, 
                                  timeout_remaining,timeout_ind, home, time))
}

beta_master <- gather(beta_master, coefficient, value, -time)
sum_stats <- group_by(beta_master, coefficient, time) %>%
  summarise("posterior_mean" = mean(value),
            "posterior_sd" = sd(value),
            "lower_quantile" = quantile(value, 0.025),
            "upper_quantile" = quantile(value, 0.975)) %>%
  ungroup()


### 95% Credible Intervals By Time
ggplot(sum_stats, aes(x = time, y = posterior_mean)) + 
  facet_wrap(~coefficient, scales = "free") + 
  geom_point() + 
  geom_ribbon(aes(ymin = lower_quantile, ymax = upper_quantile), alpha = 0.3, fill = "skyblue") +
  geom_vline(xintercept = 1200, lty = 2, size = 1.2, col = "orange") + 
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        legend.position = "none") + 
  labs(x = "Seconds Remaining", 
       y = "Coefficient Value", 
       title = "95% Credibile Intervals for Beta Over Time")

### Focus on Timeout Effect
ggplot(filter(sum_stats, coefficient == "timeout_ind"), 
       aes(x = time, y = posterior_mean)) +
  geom_point() + 
  geom_ribbon(aes(ymin = lower_quantile, ymax = upper_quantile), alpha = 0.3, fill = "skyblue") +
  geom_vline(xintercept = 1200, lty = 2, size = 1.2, col = "orange") + 
  theme_bw()  + 
  geom_vline(xintercept = 240, lty = 2, alpha = 0.8, col = "orange") + 
  geom_vline(xintercept = 480, lty = 2, alpha = 0.8, col = "orange") + 
  geom_vline(xintercept = 720, lty = 2, alpha = 0.8, col = "orange") + 
  geom_vline(xintercept = 960, lty = 2, alpha = 0.8, col = "orange") + 
  geom_vline(xintercept = 1440, lty = 2, alpha = 0.8, col = "orange") + 
  geom_vline(xintercept = 1680, lty = 2, alpha = 0.8, col = "orange") + 
  geom_vline(xintercept = 1920, lty = 2, alpha = 0.8, col = "orange") +
  geom_vline(xintercept = 2160, lty = 2, alpha = 0.8, col = "orange") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) + 
  labs(x = "Seconds Remaining", 
       y = "Coefficient Value", 
       title = "95% Credibile Intervals for Timeout Indicator Coefficient Over Time")


#### Posterior Correlations Over Time
for(i in 1:(nrow(directory) - 1)) {
  x <- read.csv(directory$file[i], as.is = T)[-c(1:1000),] %>%
    select(score_diff, favored_by, timeout_remaining,timeout_ind, home)
  y <- expand.grid(names(x), names(x)) 
  y$correlation <- NA
  y$time <- directory$max_time[i]
  for(j in 1:nrow(y)) {
    y$correlation[j] <- cor(x[,y[j,1]], x[,y[j,2]])
  }
  if(i == 1) {
    correlations <- y
  }else{
    correlations <- rbind(correlations, y)
  }
}

names(correlations)[1:2] <- c("variable_1", "variable_2")

ggplot(filter(correlations, variable_1 != variable_2), 
       aes(x = time, y = correlation, col = correlation)) + 
  facet_grid(vars(variable_1), vars(variable_2)) + 
  geom_point() +
  geom_line(size = 1.2) + 
  theme_bw() + 
  scale_color_continuous(low = "blue", high = "red") + 
  geom_hline(yintercept = 0, lty = 2) + 
  ylim(c(-0.5, 0.5)) + geom_vline(xintercept = 1200) +
theme(plot.title = element_text(size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 6, angle = 45),
      axis.title = element_text(size = 14)) + 
  labs(x = "Seconds Remaining", 
       y = "Correlation", 
       col = "Correlation",
       title = "Posterior Correlations")

############################# WP Chart W/ Uncertainity #########################
y <- get_pbp_game(401096923)

### Get Score Diff
n <- length(game_ids)

### Seconds
msec <- max(y$secs_remaining)

if(msec > 2400 & msec <= 2700) {
  y$secs_remaining[y$secs_remaining >= 300] <- 
    y$secs_remaining[y$secs_remaining >= 300] - 300
}else if(msec > 2700 & msec <= 3000) {
  y$secs_remaining[y$secs_remaining >= 600] <- y$secs_remaining[y$secs_remaining >= 600] - 600
  y$secs_remaining[y$secs_remaining >= 300 & y$secs_remaining < 600] <- 
    y$secs_remaining[y$secs_remaining >= 300 & y$secs_remaining < 600] - 300
}else if(msec > 3000 & msec <= 3300){
  y$secs_remaining[y$secs_remaining >= 900] <- y$secs_remaining[y$secs_remaining >= 900] - 900
  y$secs_remaining[y$secs_remaining >= 600 & y$secs_remaining < 900] <- 
    y$secs_remaining[y$secs_remaining >= 600 & y$secs_remaining < 900] - 600
  y$secs_remaining[y$secs_remaining >= 300 & y$secs_remaining < 600] <- 
    y$secs_remaining[y$secs_remaining >= 300 & y$secs_remaining < 600] - 300
}else if(msec > 3300 & msec <= 3600){
  y$secs_remaining[y$secs_remaining >= 1200] <- y$secs_remaining[y$secs_remaining >= 1200] - 1200
  y$secs_remaining[y$secs_remaining >= 900 & y$secs_remaining < 1200] <- 
    y$secs_remaining[y$secs_remaining >= 900 & y$secs_remaining < 1200] - 900
  y$secs_remaining[y$secs_remaining >= 600 & y$secs_remaining < 900] <- 
    y$secs_remaining[y$secs_remaining >= 600 & y$secs_remaining < 900] - 600
  y$secs_remaining[y$secs_remaining >= 300 & y$secs_remaining < 600] <- 
    y$secs_remaining[y$secs_remaining >= 300 & y$secs_remaining < 600] - 300
}

timeout <- filter(y, sapply(y$description, grepl, pattern = "Timeout")) %>%
  filter(description != "Official TV Timeout")

timeout$team <- sapply(timeout$description, function(z) gsub("\\s* Timeout", "", z))
teams <- unique(timeout$team)
pos_teams <- c(y$home[1], y$away[1])
if(nrow(timeout) > 0) {
  home <- pos_teams[which.min(stringdist(teams, y$home[1]))]
  away <- setdiff(pos_teams, home)
}else{
  home <- pos_teams[1]
  away <- pos_teams[2]
}
### TimeOuts
y$home_time_out_remaining <- 4
y$away_time_out_remaining <- 4
y$home_timeout_ind <- 0
y$away_timeout_ind <- 0
nplay <- nrow(y)
if(nrow(timeout) > 0) {
  for(j in 1:nrow(timeout)) {
    play_id <- timeout$play_id[j]
    secs_remaining <- timeout$secs_remaining[j]
    half <- timeout$half[j]
    
    if(timeout$team[j] == home) {
      y$home_time_out_remaining[play_id:nplay] <- y$home_time_out_remaining[play_id:nplay] - 1
      y$home_timeout_ind[y$secs_remaining <= secs_remaining & y$secs_remaining >= secs_remaining - 60
                         & y$half == half] <- 1
    }else {
      y$away_time_out_remaining[play_id:nplay] <- y$away_time_out_remaining[play_id:nplay] - 1
      y$away_timeout_ind[y$secs_remaining <= secs_remaining & y$secs_remaining >= secs_remaining - 60
                         & y$half == half] <- 1
    }
    
  }
}  
y$home_time_out_remaining[y$half > 2] <- 
  y$home_time_out_remaining[y$half > 2] + (y$half[y$half > 2] - 2)
y$away_time_out_remaining[y$half > 2] <- 
  y$away_time_out_remaining[y$half > 2] + (y$half[y$half > 2] - 2)


if(any(y$home_time_out_remaining < 0) | any(y$away_time_out_remaining < 0)) {
  y$home_time_out_remaining <- y$home_time_out_remaining + 2
  y$away_time_out_remaining <- y$away_time_out_remaining + 2
}else{  
  if(max(y$home_time_out_remaining[y$half == 2]) < 4) {
    y$home_time_out_remaining[y$half >= 2] <- 
      y$home_time_out_remaining[y$half >= 2] + 1
  }
  if(max(y$away_time_out_remaining[y$half == 2]) < 4) {
    y$away_time_out_remaining[y$half >= 2] <- 
      y$away_time_out_remaining[y$half >= 2] + 1
  }
}

y$win <- ifelse(y$home_score[nplay] > y$away_score[nplay], 1, 0)


y$score_diff <- y$home_score - y$away_score
y$home <- 0

y <- select(y, win, score_diff, home_favored_by,
            secs_remaining, home_time_out_remaining, home_timeout_ind, home) %>%
  rename(favored_by = home_favored_by, timeout_remaining = home_time_out_remaining,
         timeout_ind = home_timeout_ind)

x <- select(y, -win)
for(i in 1:nrow(x)) {
  print(i)
  time <- x$secs_remaining[i]
  file <- filter(directory, min_time < time, max_time >= time) %>% pull(file)
  if(time == 0) {
    file <- "mcmc_draws/beta_0_1.csv"
  }
  beta <- read.csv(file, as.is = T)[-c(1:1000),] %>% 
    select(score_diff, favored_by, timeout_remaining,timeout_ind, home)
  beta <- as.matrix(beta)
  rownames(beta) <- c()
  z <- as.matrix(select(x, -secs_remaining))
  rownames(z) <- c()
  p <- rep(NA, nrow(beta))
  v <- z[i,]
  for(j in 1:nrow(beta)) {
    v[2] <- rnorm(1, x$favored_by[1], 2)
    m <- as.vector(v %*% beta[j,])
    p[j] <- exp(m)/(1 + exp(m))
  }
  
  df <- data.frame("win_prob" = p,
                   "time" = time)
  if(i == 1) {
    wp_post <- df 
  }else{
    wp_post <- rbind(wp_post, df)
  }
}
game <- 
  group_by(wp_post, time) %>%
  summarise("wp" = mean(win_prob),
            "upper" = quantile(win_prob, 0.99),
            "lower" = quantile(win_prob, 0.01))
game[1,-1] <- 1

game$team <- "Gonzaga"
game <- rbind(game, mutate(game, wp = 1 - wp, upper = 1 - upper, lower = 1 - lower, team = "Duke"))

ggplot(game, aes(x = max(time) - time, y = wp, group = team, fill = team)) + 
  geom_line(aes(col = team)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) + 
  theme_bw() + 
  ylim(c(0,1)) +
  scale_x_continuous(breaks = seq(0, 2400, 400)) + 
  labs(x = "Seconds Elapsed", y = "Win Probability") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) + 
  labs(x = "Seconds Elapsed", 
       y = "Win Probability",
       fill = "Team",
       color = "Team",
       title = "Duke vs. Gonzaga Win Probability Chart",
       subtitle = "November 21, 2018") +
  scale_color_manual(values = c("blue", "#041E42"), labels = c("Duke", "Gonzaga")) + 
  scale_fill_manual(values = c("blue", "#041E42"), labels = c("Duke", "Gonzaga"))

