

library(rstan)
library(data.table)
library(tidyverse)
library(lubridate)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

games2017 <- fread("games-2017.csv")

gdat <- games2017 %>%
  mutate(game_date = ymd(game_dt),
         hwins = home_score_ct > away_score_ct,
         week = (as.numeric(ymd(game_date) - min(ymd(game_date))) %/% 2) + 1) %>%
  select(away_team_id,
         home_team_id,
         away_score_ct,
         home_score_ct,
         hwins,
         week,
         game_date)

data <- list(
  N = nrow(gdat),
  T = length(unique(gdat$home_team_id)),
  W = max(gdat$week),
  team1 = as.numeric(factor(gdat$home_team_id)),
  team2 = as.numeric(factor(gdat$away_team_id)),
  winner = as.numeric(gdat$hwins),
  week = gdat$week
)


m1 <- stan("mlb-bt-dlm.stan",
           iter = 1000,
           chains = 2,
           data = data)

m2 <- stan("mlb-bt-dlm-home-field-advantage.stan",
           iter = 1000,
           chains = 2,
           data = data)

team_names <- c(gdat$away_team_id, gdat$home_team_id)
pars <- rstan::extract(m1)
skill_par <- pars$skill
skill50 <- matrix(rep(NA, dim(skill_par)[3]*dim(skill_par)[2]), ncol = dim(skill_par)[2], byrow = FALSE)
for(i in 1:dim(skill_par)[3]) {
  skill50[i,] <- apply(skill_par[,,i], MARGIN = 2, FUN = quantile, probs = 0.5)
}
skill50 <- skill50 %>%
  as.data.frame() %>%
  mutate(team = levels(factor(team_names))) %>%
  gather(dim, q50, -team) %>%
  group_by(team) %>%
  mutate(week = row_number())

skill25 <- matrix(rep(NA, dim(skill_par)[3]*dim(skill_par)[2]), ncol = dim(skill_par)[2], byrow = FALSE)
for(i in 1:dim(skill_par)[3]) {
  skill25[i,] <- apply(skill_par[,,i], MARGIN = 2, FUN = quantile, probs = 0.25)
}
skill25 <- skill25 %>%
  as.data.frame() %>%
  mutate(team = levels(factor(team_names))) %>%
  gather(dim, q25, -team) %>%
  group_by(team) %>%
  mutate(week = row_number())
skill75 <- matrix(rep(NA, dim(skill_par)[3]*dim(skill_par)[2]), ncol = dim(skill_par)[2], byrow = FALSE)
for(i in 1:dim(skill_par)[3]) {
  skill75[i,] <- apply(skill_par[,,i], MARGIN = 2, FUN = quantile, probs = 0.75)
}
skill75 <- skill75 %>%
  as.data.frame() %>%
  mutate(team = levels(factor(team_names))) %>%
  gather(dim, q75, -team) %>%
  group_by(team) %>%
  mutate(week = row_number())

skill <- skill25 %>%
  left_join(skill50) %>%
  left_join(skill75)

png("mlb-teams.png", height = 20, width = 40, res = 400, units = "cm")
skill %>%
  ggplot() +
  geom_line(aes(week,q50,color=team), size = 2) +
  geom_point(aes(week,q50,color=team)) +
  geom_errorbar(aes(week, ymin = q25, ymax = q75, color = team)) +
  # geom_line(aes(week,cwpct,color=team)) +
  theme(legend.position = "none") +
  facet_wrap(~team)#, scales = "free_y")
dev.off()
