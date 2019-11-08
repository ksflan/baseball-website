
# FIXME
# 1. add sink() calls for logging
# 2. add estimates for matchups

# This script runs every night and fits the Bradley-Terry model for baseball teams.
#     It uses updated data from the previous day to estimate the relative skill
#     of each team.


# COMMAND: /usr/local/bin/Rscript "/Users/kevin/Documents/R Projects/mlb-stan/nightly/nightly-bt-model.R"


library(lubridate)
library(rstan)
library(tidyverse)
library(RPostgreSQL)

source("/Users/kevin/Documents/R Projects/nightly-pitchfx/helper-functions.R")
# source("/Volumes/kevin/Documents/R Projects/nightly-pitchfx/helper-functions.R")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

setwd("/Users/kevin/Documents/R Projects/mlb-stan")

####Loading into the database
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname = 'sammi', port = 5432, host = 'localhost', user = 'kevin')
# con <- dbConnect(drv, dbname = 'sammi', port = 5432, host = '192.168.1.71', user="kevin")


# Load the games data
games <- dbGetQuery(con, "select * from pitchfx.schedule where date_part('year', original_date) = 2019")


data_pre <- games %>%
  filter(!is.na(home_score),
         !is.na(away_score),
         home_sport_code == "mlb",
         # away_team_id %in% teamids2,
         # home_team_id %in% teamids2)%>%#,
         game_type == "R") %>%
  mutate(week = (as.numeric(ymd(original_date) - min(ymd(original_date))) %/% 2) + 1)

teams <- levels(factor(c(data_pre$away_team_name, data_pre$home_team_name)))

# Set the prior skill numbers # FIXME
# priors <- seq(-1, 1, length.out = 30)
priors <- rep(0, 30)


# Data
data <- list(
  N = nrow(data_pre),
  T = length(teams),
  W = max(data_pre$week),
  team1 = as.numeric(factor(data_pre$home_team_name, levels = teams)),
  team2 = as.numeric(factor(data_pre$away_team_name, levels = teams)),
  winner = ifelse(data_pre$home_score > data_pre$away_score, 1, 0),
  week = data_pre$week,
  priors = priors
)




model_fit <- stan("mlb-bt-dlm-home-field-advantage.stan",
                  iter = 1000,
                  chains = 2,
                  data = data)




pars <- rstan::extract(model_fit)
all_skill <- NULL
for(i in 1:dim(pars$skill)[3]) {
  skill_temp <- pars$skill[,,i] %>%
    as.data.frame() %>%
    gather(week, value) %>%
    mutate(week = as.integer(gsub("V", "", week)),
           team = teams[i],
           parameter = "skill")
  all_skill <- rbind(all_skill, skill_temp)
}

all_matchup <- NULL
for(i in 1:dim(pars$pred)[1]) {
  matchup_temp <- pars$pred[i,,] %>%
    as.data.frame()
  colnames(matchup_temp) <- teams
  matchup_temp <- matchup_temp %>%
    mutate(team2 = teams) %>%
    gather(team1, value, -team2)
    # mutate(week = as.integer(gsub("V", "", week)),
    #        team = teams[i],
    #        parameter = "skill")
  all_matchup <- rbind(all_matchup, matchup_temp)
}






# Write samples to the database

# dbGetQuery() # delete old rows in some way
drop_table("stan",
           "mlb_skill")
write_table_to_psql(all_skill,
                    "stan",
                    "mlb_skill")

# dbGetQuery() # delete old rows in some way
drop_table("stan",
           "mlb_matchups")
write_table_to_psql(all_matchup,
                    "stan",
                    "mlb_matchups")









