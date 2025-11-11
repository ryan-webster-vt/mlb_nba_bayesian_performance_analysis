# Included libraries
library(dplyr)
library(baseballr)
library(hoopR)
library(lubridate)
library(ProbBayes)

#Finds each pitchers with at least the minimum requirement   

#to be a qualified pitcher for each season, extract only their

#name and id.
pitchers_2016 <- fg_pitcher_leaders(startseason = 2016, endseason = 2016,
                                    qual = 162) %>% 
  select("PlayerName", "playerid")

pitchers_2017 <- fg_pitcher_leaders(startseason = 2017, endseason = 2017,
                                    qual = 162) %>% 
  select("PlayerName", "playerid")

pitchers_2018 <- fg_pitcher_leaders(startseason = 2018, endseason = 2018,
                                    qual = 162) %>% 
  select("PlayerName", "playerid")

pitchers_2019 <- fg_pitcher_leaders(startseason = 2019, endseason = 2019,
                                    qual = 162) %>% 
  select("PlayerName", "playerid")

pitchers_2020 <- fg_pitcher_leaders(startseason = 2020, endseason = 2020,
                                    qual = 60) %>% 
  select("PlayerName", "playerid")

pitchers_2021 <- fg_pitcher_leaders(startseason = 2021, endseason = 2021,
                                    qual = 162) %>% 
  select("PlayerName", "playerid")

# Bind together
pitchers <- rbind(pitchers_2016, pitchers_2017, pitchers_2018, pitchers_2019,
                  pitchers_2020, pitchers_2021)

# Eliminate duplicates
unique_pitchers <- pitchers %>% 
  distinct(playerid, .keep_all = TRUE)

# Initialize data frame which will keep track of cumulative stats
pitchers_cummulative <- data.frame(PlayerName = character(), 
                                   cum_ip = numeric(), WHIP = numeric())

years <- c(2016, 2017, 2018, 2019, 2020, 2021)

# Loops through each pitcher and extracts their box score for each game. 
# Cummulates the innings pitched, hits, and walks and calculates WHIP 
for (i in 1:nrow(unique_pitchers)) {
  for( r in 1:length(years)) { 
    data <- fg_pitcher_game_logs(unique_pitchers[i, 2], years[r])
    if (nrow(data) == 0) { next }
    data <- data %>% 
      select(PlayerName, Date, BB, IBB, H, IP) %>% 
      mutate(cum_ip = cumsum(IP)) %>% 
      mutate(cum_h = cumsum(H)) %>% 
      mutate(cum_bb = cumsum(BB)) %>% 
      mutate(cum_ibb = cumsum(IBB)) %>% 
      mutate(WHIP = (cum_h + cum_bb + cum_ibb) / cum_ip) %>% 
      mutate(date = rev(Date)) %>% 
      select(PlayerName, date, WHIP, cum_ip)
    # Adds onto data frame which will contain every pitcher/year with his stats
    pitchers_cummulative <- bind_rows(pitchers_cummulative, data)
  }
}

# Same idea as pitchers, but for batters. 
# Minimum 502 PA for qualified hitter
hitters_2016 <- fg_batter_leaders(startseason = 2016, endseason = 2016, 
                                  qual = 502) %>% 
  select("PlayerName", "playerid")
hitters_2017 <- fg_batter_leaders(startseason = 2017, endseason = 2017,
                                  qual = 502) %>% 
  select("PlayerName", "playerid")
hitters_2018 <- fg_batter_leaders(startseason = 2018, endseason = 2018,
                                  qual = 502) %>% 
  select("PlayerName", "playerid")
hitters_2019 <- fg_batter_leaders(startseason = 2019, endseason = 2019,
                                  qual = 502) %>% 
  select("PlayerName", "playerid")
hitters_2020 <- fg_batter_leaders(startseason = 2020, endseason = 2020,
                                  qual = 185) %>% 
  select("PlayerName", "playerid")
hitters_2021 <- fg_batter_leaders(startseason = 2021, endseason = 2021,
                                  qual = 502) %>% 
  select("PlayerName", "playerid")

# Bind together
hitters <- rbind(hitters_2016, hitters_2017, hitters_2018, hitters_2019,
                 hitters_2020, hitters_2021)

# Eliminate duplicates
unique_hitters <- hitters %>% 
  distinct(playerid, .keep_all = TRUE)

# Initialize data frame which will keep track of cumulative stats
hitters_cummulative <- data.frame(PlayerName = character(), 
                                  cum_ab = numeric(), ops = numeric())

# Loops through each pitcher and extracts their box score for each game. 
# Cummulates the at bats, hits, type of hit, walks, etc. to calculate obs
for (i in 1:nrow(unique_hitters)) {
  for( r in 1:length(years)) {
    data <- fg_batter_game_logs(unique_hitters[i, 2], years[r])
    if (nrow(data) == 0) { next }
    data <- data %>% 
      select(PlayerName, Date, AB, H, `1B`, `2B`, `3B`, 
             HR, BB, IBB, HBP, SF) %>% 
      mutate(cum_ab = cumsum(AB)) %>% 
      mutate(cum_h = cumsum(H)) %>% 
      mutate(cum_1b = cumsum(`1B`)) %>% 
      mutate(cum_2b = cumsum(`2B`)) %>% 
      mutate(cum_3b = cumsum(`3B`)) %>% 
      mutate(cum_hr = cumsum(HR)) %>% 
      mutate(cum_bb = cumsum(BB)) %>% 
      mutate(cum_ibb = cumsum(IBB)) %>% 
      mutate(cum_hbp = cumsum(HBP)) %>% 
      mutate(cum_sf = cumsum(SF)) %>% 
      # total bases calculation
      mutate(cum_tb = cum_h + (2 * cum_2b) + (3 * cum_3b) + (4 * cum_hr)) %>% 
      # slugging calculation
      mutate(slg = cum_h / cum_ab) %>% 
      # obp calculation
      mutate(obp = (cum_h + cum_bb + cum_ibb + cum_hbp) / 
               (cum_ab + cum_bb + cum_ibb + cum_hbp + cum_sf)) %>% 
      # ops calculation
      mutate(ops = slg + obp) %>% 
      mutate(date = rev(Date)) %>% 
      select(PlayerName, date, cum_ab, ops)
    # Adds onto data frame which will contain every batter/year with his stats
    hitters_cummulative <- bind_rows(hitters_cummulative, data)
  }
}

# Filters cummulative data to only contain post 2019 and before the end of May
hitter_predictive <- hitters_cummulative %>% 
  filter(year(date) >= 2019) %>% 
  filter(month(date) < 06) %>% 
  filter(!ops == 0)

# Used to measure prediction with end of season results
end_season_ops <- hitters_cummulative %>% 
  filter(year(date) >= 2019) %>% 
  filter(month(date) >= 06) %>% 
  filter(!ops == 0)

# "Average" baseball player's OPS and estimated standard deviation for the prior
mu_ops = 0.65
sd_ops = 0.1

# Data used 
data_mu_ops = mean(hitter_predictive$ops, na.rm = TRUE) 
data_sd_ops = sd(hitter_predictive$ops, na.rm = TRUE)

# Use ProbBayes library to produce posterior
prior_ops <- c(mu_ops, sd_ops)
data_ops <- c(data_mu, data_sd)
result_ops <- normal_update(prior_ops, data_ops, teach = TRUE) 
#posterior = 0.633

end_mean_obp <- mean(end_season$ops) #0.59

# Filters cummulative data to only contain 
# post 2019 and before the end of May
pitcher_predictive <- pitchers_cummulative %>% 
  filter(year(date) >= 2019) %>% 
  filter(month(date) < 06)

# Used to measure prediction with end of season results
end_season_whip <- pitchers_cummulative %>% 
  filter(year(date) >= 2019) %>% 
  filter(month(date) >= 06)

# Remove infinite WHIP's
pitcher_predictive <- pitcher_predictive[!is.infinite(pitcher_predictive$WHIP), ]
end_season_whip <- end_season_whip[!is.infinite(end_season_whip$WHIP), ]

# "Average" baseball player's whip and estimated 
# standard deviation for the prior
mu_whip = 1.2
sd_whip = 0.6

# Data used 
data_mu_whip = mean(pitcher_predictive$WHIP) 
data_sd_whip = sd(pitcher_predictive$WHIP)

# Use ProbBayes library to produce posterior
prior_whip <- c(mu_whip, sd_whip)
data_whip <- c(data_mu_whip, data_sd_whip)
result_whip <- normal_update(prior_whip, data_whip, teach = TRUE)
# posterior = 1.23 WHIP

end_mean_whip <- mean(end_season_whip$WHIP, na.rm = TRUE) #1.39

# Creates empty data frame
prior_player <- data.frame(player = unique_hitters$PlayerName)

# Takes data from before 2019 to use as prior, 
# uses mean/sd obs for the bayes
prior_obs <- hitters_cummulative %>% 
  filter(year(date) < 2019) %>% 
  filter(!ops == 0) %>% 
  group_by(PlayerName) %>% 
  summarise(mean = mean(ops),
            sd = sd(ops))

# Applies the prior and data through the normal bayes model. 
# each result is then assigned to the respected players row
for (i in 1:nrow(prior_obs)) {
  prior <- c(prior_obs$mean[i], prior_obs$sd[i])
  result <- normal_update(prior, data_ops, teach = TRUE)
  prior_obs$prediction[i] <- result[3, 2]
}

# finds each players obs at the end of the 2019 season
end_2019_season_batter <- hitters_cummulative %>% 
  filter(year(date) == 2019) %>% 
  filter(month(date) >= 6) %>% 
  group_by(PlayerName) %>%
  slice(n())

# Merge to put end of season OPS onto the dataset with the predictions
prior_obs <- merge(prior_obs, end_2019_season_batter, 
                   by = "PlayerName", all.x = T) %>% 
  select(PlayerName, mean, sd, prediction, ops)

# absolute difference is then calculated
prior_obs$difference <- abs(prior_obs$prediction - prior_obs$ops)

mean(prior_obs$difference, na.rm = TRUE)

prior_pitchers <- data.frame(player = unique_pitchers$PlayerName)

# Takes data from before 2019 to use as prior, 
# uses mean/sd OPS for the bayes
prior_whip <- pitchers_cummulative %>% 
  filter(year(date) < 2019) %>% 
  group_by(PlayerName) %>% 
  summarise(mean = mean(WHIP),
            sd = sd(WHIP))

prior_whip <- prior_whip[!is.infinite(prior_whip$mean), ]
prior_whip <- na.omit(prior_whip)

# Applies the prior and data through the normal bayes model. 
# each result is then assigned to the respected players row
for (i in 1:nrow(prior_whip)) {
  prior <- c(prior_whip$mean[i], prior_whip$sd[i])
  result <- normal_update(prior, data_whip, teach = TRUE)
  prior_whip$prediction[i] <- result[3, 2]
}

# finds each players obs at the end of the 2019 season
end_2019_season_pitchers <- pitchers_cummulative %>% 
  filter(year(date) == 2019) %>% 
  filter(month(date) >= 6) %>% 
  group_by(PlayerName) %>%
  slice(n())

# Merge to put end of season obs onto the dataset with the predictions
prior_whip <- merge(prior_whip, end_2019_season_pitchers, 
                    by = "PlayerName", all.x = T) %>% 
  select(PlayerName, mean, sd, prediction, WHIP.y)

# absolute difference is then calculated
prior_whip$difference <- abs(prior_whip$prediction - prior_whip$WHIP.y)

mean(prior_whip$difference, na.rm = TRUE)

# Creates dataset which only has stats in the end of May 
# and finds their mean and sd.
end_may_obs <- hitters_cummulative %>%
  filter(year(date) == 2019) %>% 
  filter(month(date) == 5) %>% 
  group_by(PlayerName) %>% 
  summarise(mean = mean(ops),
            sd = sd(ops))

# Uses the normal bayes posterior to create predictions
end_may_obs$prediction <- NA
for (i in 1:nrow(end_may_obs)) {
  prior <- c(end_may_obs$mean[i], end_may_obs$sd[i])
  result <- normal_update(prior, data_ops, teach = TRUE)
  end_may_obs$prediction[i] <- result[3, 2]
}

# Merge predictions with batter obs
end_may_obs <- merge(end_may_obs, end_2019_season_batter, 
                     by = "PlayerName", all.x = T) %>% 
  select(PlayerName, mean, sd, prediction, ops.y)

# Finds difference 
end_may_obs$difference <- abs(end_may_obs$prediction - end_may_obs$ops.y)

mean(end_may_obs$difference, na.rm = TRUE)

# Creates dataset which only has stats in the end of May 
# and finds their mean and sd.
end_may_whip <- pitchers_cummulative %>% 
  filter(year(date) == 2019) %>% 
  filter(month(date) == 5) %>% 
  group_by(PlayerName) %>% 
  summarise(mean = mean(WHIP),
            sd = sd(WHIP))

# Uses the normal bayes posterior to create predictions
end_may_whip$prediction <- NA
for (i in 1:nrow(end_may_whip)) {
  prior <- c(end_may_whip$mean[i], end_may_whip$sd[i])
  result <- normal_update(prior, data_whip, teach = TRUE)
  end_may_whip$prediction[i] <- result[3, 2]
}

# Merge predictions with pitcher whip
end_may_whip <- merge(end_may_whip, end_2019_season_pitchers, 
                      by = "PlayerName", all.x = T) %>% 
  select(PlayerName, mean, sd, prediction, WHIP.y)

# Finds difference 
end_may_whip$difference <- abs(end_may_whip$prediction - end_may_whip$WHIP.y)

mean(end_may_whip$difference, na.rm = TRUE)




