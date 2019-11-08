

# This script sends an email with a model standings update and predictions for the 
#     next day's games.



# COMMAND: /usr/local/bin/Rscript "/Users/kevin/Documents/R Projects/mlb-stan/nightly/nightly-stan-email.R"


library(lubridate)
library(rstan)
library(tidyverse)
library(RPostgreSQL)
library(teamcolors)
library(gmailr)
library(data.table)

source("/Users/kevin/Documents/R Projects/nightly-pitchfx/helper-functions.R")
# source("/Volumes/kevin/Documents/R Projects/nightly-pitchfx/helper-functions.R")

setwd("/Users/kevin/Documents/R\ Projects/mlb-stan/nightly")

Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")

####Loading into the database
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname = 'sammi', port = 5432, host = 'localhost', user = 'kevin')
# con <- dbConnect(drv, dbname = 'sammi', port = 5432, host = '192.168.1.71', user="kevin")








name_xrefs <- fread("team-xfres-temp.csv") # fixme here

samples <- dbGetQuery(con, "select * from stan.mlb_skill")


team_names <- c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles",
                "Boston Red Sox",
                "Chicago Cubs", "Chicago White Sox", "Cincinnati Reds", "Cleveland Indians",
                "Colorado Rockies", "Detroit Tigers", "Houston Astros", "Kansas City Royals",
                "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins",
                "Milwaukee Brewers",
                "Minnesota Twins", "New York Mets", "New York Yankees", "Oakland Athletics",
                "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres",
                "San Francisco Giants", "Seattle Mariners", "St. Louis Cardinals",
                "Tampa Bay Rays", "Texas Rangers", "Toronto Blue Jays",
                "Washington Nationals")
teamids2 <- c(109, 144, 110, 111, 112, 145, 113, 114, 115, 116, 117, 118, 108, 119, 146,
              158, 142, 121, 147, 133, 143, 134, 135, 137, 136, 138, 139, 140, 141, 120)
xref <- data.frame(
  team_names = team_names,
  team_ids = teamids2,
  stringsAsFactors = FALSE
) %>%
  left_join(teamcolors, by = c("team_names" = "name")) %>%
  left_join(name_xrefs, by = c("team_ids" = "home_team_id"))

colors <- xref$primary
names(colors) <- xref$home_team_name

plot_dat <- samples %>%
  group_by(Team = team,
           Week = week) %>%
  summarise(q50 = quantile(value, 0.5),
            q25 = quantile(value, 0.25),
            q75 = quantile(value, 0.75)) %>%
  left_join(xref, by = c("Team" = "home_team_name")) %>%
  # mutate(Date = ymd("2018-02-23") + Week)
  mutate(Date = ymd("2018-03-27") + Week)

png("team-rankings.png", height = 10, width = 20, res = 500, units = "cm")
plot_dat %>%  
  ggplot(aes(Date, q50, color=Team)) +
  geom_line() +
  geom_point() +
  ggrepel::geom_text_repel(data = plot_dat %>%
                             filter(Week == max(Week)),
                           aes(label = Team, x = Date, y = q50),
                           xlim = c(max(plot_dat$Date) + 1.5, max(plot_dat$Date) + 2),
                           # segment.colour = "gray",
                           arrow = arrow(length = unit(0.01, 'npc')),
                           point.padding = 0.8) +
  # geom_errorbar(aes(ymin = q25,
                    # ymax = q75)) +
  # scale_fill_manual(values = xref$secondary) + 
  scale_color_manual(values = colors, guide = FALSE) +
  xlim(c(min(plot_dat$Date), max(plot_dat$Date) + 4)) +
  theme(legend.position = "right")
dev.off()


# test <- data_pre %>%
#   mutate(aname = paste0(away_team_city, " ", away_team_name)) %>%
#   left_join(teamcolors, by = c("aname" = "name"))
# team_ids <- data.frame(
#   team_id = unique(c(data_pre$away_team_id, data_pre$home_team_id)),
#   team_name = unique(c(data_pre$away_team_name, data_pre$home_team_name)),
#   stringsAsFactors = FALSE
# ) %>%
#   arrange(team_id)
# 
# 
# team_colors_id <- c(109, )
# 




matchups <- dbGetQuery(con, "select * from stan.mlb_matchups")




games <- dbGetQuery(con, paste0("select *
                                   from pitchfx.schedule g
                                   where (g.away_team_id = 116
                                   or g.home_team_id = 116)
                                   and g.original_date = '", today(), "'"))


tigers_matchup <- matchups %>%
  filter(team1 == games$home_team_name,
         team2 == games$away_team_name)

png("tigers-matchup.png", height = 10, width = 20, res = 500, units = "cm")
tigers_matchup %>%
  ggplot(aes(value)) +
  geom_density(fill = "blue", color = "blue", alpha = 0.5) +
  xlim(c(0,1)) +
  labs(title = paste0("Probability that the ",
                      tigers_matchup$team1[1],
                      " beat the ",
                      tigers_matchup$team2[1]),
       subtitle = paste0("Median prediction: ", round(quantile(tigers_matchup$value, 0.5), 2)),
       x = "Probability of a Victory",
       y = "Density")
dev.off()


# rmarkdown::render("~/Documents/R Projects/baseball-prediction-website/index.Rmd",
#                   output_dir = "/Users/kevin/Documents/R Projects/mlb-stan/nightly")
# rmarkdown::render_site(input = "~/Documents/R Projects/baseball-prediction-website")
# system("kinit --use-keytab --keytab=\"/Users/kevin/Documents/R Projects/baseball-prediction-website/_keytab/ksflan_afs.keytab\" ksflan/afs@UMICH.EDU")
# system("aklog")
# system("cp -R \"/Users/kevin/Documents/R Projects/baseball-prediction-website/_site/\" /afs/umich.edu/user/k/s/ksflan/Public/html/")
# system("kdestroy -a")

# system("rm -r /Library/WebServer/_site/*")
# system("mv -v \"/Users/kevin/Documents/R Projects/baseball-prediction-website/_site/\" /Library/WebServer/")




# Send the email

matchup_email <- mime() %>%
  from("database.updates.goblue@gmail.com") %>%
  to("ksflan@umich.edu") %>%
  subject("Daily Tigers Preview -- testing") %>%
  # attach_file("tigers-matchup.png",
  #             type = "png") %>%
  # attach_file("team-rankings.png",
  #             type = "png")
  attach_file("/Library/WebServer/_site/index_files/figure-html/unnamed-chunk-1-1.png")

send_message(matchup_email)

