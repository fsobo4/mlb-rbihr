required_packages <- c("tidyverse", "devtools", "RSQLite", "DBI", "lubridate", "paws")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
invisible(lapply(required_packages, library, character.only = TRUE))
devtools::install_github("BillPetti/baseballr", ref = "development_branch")
library("baseballr")

##run "season <- mlb_schedule(season = 2026)" to get all game dates from 2026
##NEED:
##gamePK, officialDate,
##teams_home_team_name, teams_away_team_name, 
##note: "official" works fine and only has yyyy-mm-dd date, 
##but "gameDate" has yyyy-mm-dd date as well as game time in UTC (hh-mm-ss)
##subset the subsequent dataframe for just that stuff 
##Some sort of "next_game <- Sys.Date() SELECT MAX gameDate after current time"