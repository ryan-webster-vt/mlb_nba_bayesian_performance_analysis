# Included libraries
library(dplyr)
library(baseballr)
library(hoopR)
library(lubridate)
library(ProbBayes)

nba_pbp <- hoopR::load_nba_pbp(2016:2021)

ft_nba <- nba_pbp %>% 
  # Filters pbp to only include ft
  filter(type_id %in% c(97:108, 157, 165, 166)) 
total_fts <- nrow(ft_nba) # Count total number of fts

# Fitlers pbp to only include ft trips that end 

# possesion (so no and one's, technicals, flagrant)
ft_trips <- nba_pbp %>% 
  filter(type_id %in% c(98, 100)) 
total_ft_trips <- nrow(ft_trips) # Counts ft trips
result <- (total_ft_trips / total_fts)

# Load player boxscore of every game from 2016 to 2021
player_stats <- hoopR::load_nba_player_box(year = 2016:2021) 

# groups box scores by players and sums up their

# total points, fga, and fta
season_stats <- player_stats %>% 
  group_by(athlete_display_name) %>% 
  summarise(
    total_points = sum(points, na.rm = TRUE),
    total_fga = sum(field_goals_attempted, na.rm = TRUE),
    total_fta = sum(free_throws_attempted, na.rm = TRUE))

# Creates a true shooting percentage variable for 
# the whole 5 years using both coeff
season_stats$tsa_0.44 <- (season_stats$total_points) /
  (2*(season_stats$total_fga + 0.44*season_stats$total_fta))

season_stats$tsa_0.425 <- (season_stats$total_points) /
  (2*(season_stats$total_fga + result*season_stats$total_fta))

t_test <- t.test(season_stats$tsa_0.44, season_stats$tsa_0.425)