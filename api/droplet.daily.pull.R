library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(paws.storage)
library(baseballr)

s3 <- paws.storage::s3()

am <- get_chadwick_lu()
am$names <- paste(am$name_last, am$name_first, sep = ", ")
am = subset(am, select = c(names, key_mlbam))
am$key_mlbam <- as.integer(am$key_mlbam)
am <- am |> filter(!is.na(key_mlbam))

con <- dbConnect(SQLite(), "/api/hr_data.db")

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
  df$game_date <- format(ymd(df$game_date), "%Y-%m-%d")
  df$pitch_name[is.na(df$pitch_name)] <- "N/A"
  df$wp_delta <-ifelse(
    df$inning_topbot == "Bot",
    df$delta_home_win_exp, 
    -df$delta_home_win_exp
  )
  df = subset(df, select = -c(delta_home_win_exp))
  df <- df |> relocate(pitch_name, .after = game_date)
  df$away_win_exp <- round(1 - df$home_win_exp, 3)
  df <- df |> relocate(away_win_exp, .after = home_win_exp)
  df$pit_win_exp <- round(1 - df$bat_win_exp, 3)
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

fetch_hrs <- function() {
  cat("Fetching home runs from:", as.character(Sys.Date() - 1), "\n")
  
  day_data <- tryCatch({ 
    statcast_search(
      start_date = as.character(Sys.Date() -1), 
      end_date = as.character(Sys.Date()-1), 
      player_type = "batter"
    )
  }, error = function(e) {
    cat("Failed:", as.character(Sys.Date()), "- ", conditionMessage(e), "\n")
    NULL
  })
  
  if(!is.null(day_data) && nrow(day_data) >0) {
    day_data <- filter(day_data, events == "home_run", game_type != "S")
    if(nrow(day_data) > 0) {
      day_data <- clean_hr_data(day_data, am)
    } else {print(paste("No new home runs as of:", as.character(Sys.Date() - 1)))
    }
    bind_rows(day_data)
  }
}

{
  cat("Fetching:", as.character(Sys.Date() - 1), "\n")
  
  day_data <- fetch_hrs()
  
  if (!is.null(day_data) && nrow(day_data) > 0) {
    day_data <- as.data.frame(day_data)
    dbWriteTable(con, "homeruns_staging", day_data, overwrite = TRUE)
    rows_inserted <- dbExecute(con, "INSERT OR IGNORE INTO homeruns (
              game_date, 
              pitch_name, 
              release_speed, 
              batter_name, 
              pitcher_name, 
              des,
              game_type, 
              stand, 
              p_throws, 
              home_team, 
              away_team, 
              bb_type, 
              balls, 
              strikes,
              game_year, 
              on_3b, 
              on_2b, 
              on_1b, 
              runners_on_base, 
              hr_rbi, 
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
              away_win_exp, 
              bat_win_exp, 
              wp_delta, 
              pit_win_exp, 
              age_pit,
              age_bat, 
              n_thruorder_pitcher, 
              n_priorpa_thisgame_player_at_bat
            )
            SELECT
              game_date, 
              pitch_name, 
              release_speed, 
              batter_name, 
              pitcher_name, 
              des,
              game_type, 
              stand, 
              p_throws, 
              home_team, 
              away_team, 
              bb_type, 
              balls, 
              strikes,
              game_year, 
              on_3b, 
              on_2b, 
              on_1b, 
              runners_on_base, 
              hr_rbi, 
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
              away_win_exp, 
              bat_win_exp, 
              wp_delta, 
              pit_win_exp, 
              age_pit,    
              age_bat, 
              n_thruorder_pitcher, 
              n_priorpa_thisgame_player_at_bat
            FROM homeruns_staging
          ")
    dbExecute(con, "DROP TABLE homeruns_staging")
    
    temp_path <- tempfile(fileext = ".csv")
    write.csv(day_data, temp_path, row.names = FALSE)
    s3$put_object(
      Bucket = "mlb-rbihr",
      Key = paste0(
        "homeruns/year=", as.character(format(Sys.Date() - 1, "%Y")), 
        "/month=", as.character(format(Sys.Date() - 1, "%m")),
        "/day=", as.character(format(Sys.Date() - 1, "%d")), 
        "/homeruns_", as.character(format(Sys.Date() - 1, usetz = FALSE)), 
        ".csv"),
      Body = readBin(temp_path, "raw", file.info(temp_path)$size))
    file.remove(temp_path)
    
    cat("Databases have been updated:", rows_inserted, "rows inserted for", as.character(format(Sys.Date() - 1)), "\n")
  } else {
    cat("No data home runs for:", as.character(format(Sys.Date() - 1)), "\n")
  }
}
dbDisconnect(con)