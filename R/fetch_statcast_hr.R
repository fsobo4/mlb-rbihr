library(tidyverse)
library(baseballr)
library(lubridate)
library(dplyr)
library(DBI)
library(RSQLite)

am <- get_chadwick_lu()
am$names <- paste(am$name_last, am$name_first, sep = ", ")
am = subset(am, select = c(names, key_mlbam))
am$key_mlbam <- as.integer(am$key_mlbam)
am <- am |> filter(!is.na(key_mlbam))

con <- dbConnect(SQLite(), "hr_data_test.db")

dbExecute(con, " 
CREATE TABLE IF NOT EXISTS homeruns (
    game_date TEXT,
    pitch_name TEXT,
    release_speed REAL,
    batter_name TEXT,
    pitcher_name TEXT,
    des TEXT,
    game_type TEXT,
    stand TEXT,
    p_throws TEXT,
    home_team TEXT,
    away_team TEXT,
    bb_type TEXT,
    balls INTEGER,
    strikes INTEGER,
    game_year INTEGER,
    on_3b TEXT,
    on_2b TEXT,
    on_1b TEXT,
    runners_on_base INTEGER,
    hr_rbi INTEGER,
    outs_when_up INTEGER,
    inning INTEGER,
    inning_topbot TEXT,
    --Next 2 columns are the primary key
    --If database breaks, check here first
    game_pk INTEGER NOT NULL,
    at_bat_number INTEGER NOT NULL,
    pitch_number INTEGER,
    home_score INTEGER,
    away_score INTEGER,
    post_away_score INTEGER,
    post_home_score INTEGER,
    delta_run_exp REAL,
    bat_score_diff INTEGER,
    home_win_exp REAL,
    away_win_exp REAL,
    bat_win_exp REAL,
    wp_delta REAL,
    pit_win_exp REAL,
    age_pit INTEGER,
    age_bat INTEGER,
    n_thruorder_pitcher INTEGER,
    n_priorpa_thisgame_player_at_bat INTEGER,
 PRIMARY KEY (game_pk, at_bat_number)   
);")

clean_hr_data <- function(df,am) {
  df = subset(df, select = c(game_date, 
                             pitch_name,
                             release_speed,
                             des,
                             game_type,
                             stand,
                             player_name,
                             p_throws,
                             pitcher,
                             home_team,
                             away_team,
                             bb_type,
                             delta_home_win_exp,
                             balls,
                             strikes,
                             game_year,
                             on_3b,
                             on_2b,
                             on_1b,
                             outs_when_up,
                             inning,
                             inning_topbot,
                             game_pk,
                             at_bat_number,
                             pitch_number,
                             home_score,
                             away_score,
                             post_away_score,
                             post_home_score,
                             delta_run_exp,
                             bat_score_diff,
                             home_win_exp,
                             bat_win_exp,
                             age_pit,
                             age_bat,
                             n_thruorder_pitcher,
                             n_priorpa_thisgame_player_at_bat))
  df$game_date <- ymd(df$game_date)
  df$wp_delta <-ifelse(
    df$inning_topbot == "Bot",
    df$delta_home_win_exp, 
    -df$delta_home_win_exp
  )
  df = subset(df, select = -c(delta_home_win_exp))
  df <- df |> relocate(pitch_name, .after = game_date)
  df$away_win_exp <- 1 - df$home_win_exp
  df <- df |> relocate(away_win_exp, .after = home_win_exp)
  df$pit_win_exp <- 1 - df$bat_win_exp
  df <- df |> relocate(pit_win_exp, .after = bat_win_exp)
  df <- df |> relocate(wp_delta, .after = bat_win_exp)
  df <- left_join(df, am, by = c("pitcher" = "key_mlbam"))
  
  df <- df |> relocate(names, .after = player_name)
  df <- df |> rename(batter_name = player_name)
  df <- df |> rename(pitcher_name = names)
  df = subset(df, select = -c(pitcher))
  df$runners_on_base = (!is.na(df$on_3b)) + (!is.na(df$on_2b)) + (!is.na(df$on_1b))
  df$hr_rbi = 1 + df$runners_on_base
  df <- df |> relocate(runners_on_base, .before = outs_when_up)
  df <- df |> relocate(hr_rbi, .after = runners_on_base)
  
  df <- df |>
    left_join(am, by = c("on_1b" = "key_mlbam")) |>
    select(-on_1b) |>
    rename(on_1b = names) |>
    relocate(on_1b, .after = game_year) |>
    left_join(am, by = c("on_2b" = "key_mlbam")) |>
    select(-on_2b) |>
    rename(on_2b = names) |>
    relocate(on_2b, .after = game_year) |>
    left_join(am, by = c("on_3b" = "key_mlbam")) |>
    select(-on_3b) |>
    rename(on_3b = names) |>
    relocate(on_3b, .after = game_year)
}

fetch_hr_year <- function(year) {
  start <- if (year == 2008) {
    "2008-03-25"
    } else {
    paste0(year,"-03-01")}

  year_starts <- seq.Date(as.Date(start), as.Date(paste0(year,"-11-30")), by = "7 days")
  
  yearly_data <- list()

  for (i in seq_along(year_starts)) {
    year_start <- year_starts[i]
    year_end <- year_starts[i] + 6
    cat("Year:", year, "| Fetching:", as.character(year_start), "to", as.character(year_end), "\n")
    
    
    year_data <- tryCatch({ 
      statcast_search(
        start_date = as.character(year_start), 
        end_date = as.character(year_end), 
        player_type = "batter"
      )
      }, error = function(e) {
        cat("Failed:", as.character(year_start), "- ", conditionMessage(e), "\n")
        NULL
      })
    
    if(!is.null(year_data) && nrow(year_data) >0) {
        year_data <- filter(year_data, events == "home_run")
        if(nrow(year_data) > 0) {
          yearly_data[[i]] <- clean_hr_data(year_data, am)
          }
      }
    Sys.sleep(3)
  }
  bind_rows(yearly_data)
}

for (year in 2008:2025) {
        cat("Fetching year:", year, "\n")

        year_data <- fetch_hr_year(year)

        if (nrow(year_data) > 0) {
            dbWriteTable(con, "homeruns_staging", year_data, overwrite = TRUE)
            dbExecute(con, "INSERT OR IGNORE INTO homeruns SELECT * FROM homeruns_staging")
            dbExecute(con, "DROP TABLE homeruns_staging")
            cat("Written:", year, "- rows added:", nrow(year_data), "\n")
        } else {
            cat("No data for year:", year, "\n")
        }
    }
dbDisconnect(con)