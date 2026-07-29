library(tidyverse)
library(baseballr)
library(lubridate)
library(dplyr)
hr08 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.08.csv", header = TRUE, sep = ",")
hr09 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.09.csv", header = TRUE, sep = ",")
hr10 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.10.csv", header = TRUE, sep = ",")
hr11 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.11.csv", header = TRUE, sep = ",")
hr12 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.12.csv", header = TRUE, sep = ",")
hr13 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.13.csv", header = TRUE, sep = ",")
hr14 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.14.csv", header = TRUE, sep = ",")
hr15 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.15.csv", header = TRUE, sep = ",")
hr16 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.16.csv", header = TRUE, sep = ",")
hr17 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.17.csv", header = TRUE, sep = ",")
hr18 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.18.csv", header = TRUE, sep = ",")
hr19 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.19.csv", header = TRUE, sep = ",")
hr20 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.20.csv", header = TRUE, sep = ",")
hr21 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.21.csv", header = TRUE, sep = ",")
hr22 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.22.csv", header = TRUE, sep = ",")
hr23 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.23.csv", header = TRUE, sep = ",")
hr24 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.24.csv", header = TRUE, sep = ",")
hr25 <- read.csv("/Users/fsobo15/Library/CloudStorage/GoogleDrive-fsobo15@gmail.com/My Drive/CodeProjs/FullStack Docs/BSB Savant Data/hrdata.25.csv", header = TRUE, sep = ",")
hrs <- bind_rows(hr08, hr09, hr10, hr11, hr12, hr13, hr14, hr15, hr16, hr17, hr18, hr19, hr20, hr21, hr22, hr23, hr24, hr25)
hrs2 = subset(hrs, select = -c(pitch_type, release_pos_x, zone, release_pos_z, batter, events, description, spin_dir, spin_rate_deprecated, break_angle_deprecated, break_length_deprecated, type, hit_location, pfx_x, pfx_z, plate_x, plate_z, hc_x, hc_y, tfs_deprecated, tfs_zulu_deprecated, umpire, sv_id, vx0, vy0, vz0, ax, ay, az, sz_top, intercept_ball_minus_batter_pos_y_inches, intercept_ball_minus_batter_pos_x_inches, swing_path_tilt, attack_direction, attack_angle, arm_angle, api_break_x_batter_in, api_break_x_arm, api_break_z_with_gravity, batter_days_until_next_game, pitcher_days_until_next_game, batter_days_since_prev_game, pitcher_days_since_prev_game, age_bat_legacy, age_pit_legacy, home_score_diff, hyper_speed, delta_pitcher_run_exp, estimated_slg_using_speedangle, swing_length, bat_speed, spin_axis, of_fielding_alignment, if_fielding_alignment, post_fld_score, post_bat_score, fld_score, bat_score, launch_speed_angle, iso_value, babip_value, woba_denom, woba_value, estimated_woba_using_speedangle, estimated_ba_using_speedangle, release_pos_y, fielder_9, fielder_8, fielder_7, fielder_6, fielder_5, fielder_4, fielder_3, fielder_2, release_extension, release_spin_rate, effective_speed, launch_angle, launch_speed, hit_distance_sc, sz_bot))
hrs2$game_date <- ymd(hrs2$game_date)
hrs2$wp_delta <-ifelse(
  hrs2$inning_top == "Bot",
  hrs2$delta_home_win_exp, 
  -hrs2$delta_home_win_exp
)
hrs2 = subset(hrs2, select = -c(delta_home_win_exp))
hrs2 %>% relocate(pitch_name, .after = game_date)
hrs2 <- hrs2 |> relocate(pitch_name, .after = game_date)
hrs2$away_win_exp <- hrs2$bat_win_exp
hrs2$away_win_exp <- 1 - hrs2$home_win_exp
hrs2 <- hrs2 |> relocate(away_win_exp, .after = home_win_exp)
hrs2$pit_win_exp <- 1 - hrs2$bat_win_exp
hrs2 <- hrs2 |> relocate(pit_win_exp, .after = bat_win_exp)
hrs2 <- hrs2 |> relocate(wp_delta, .after = bat_win_exp)
am <- get_chadwick_lu()
am$names <- paste(am$name_last, am$name_first, sep = ", ")
am = subset(am, select = c(names, key_mlbam))
am$key_mlbam <- as.integer(am$key_mlbam)
am <- am |> filter(!is.na(key_mlbam))
hrs2 <- left_join(hrs2, am, by = c("pitcher" = "key_mlbam"))

hrs2 <- hrs2 |> relocate(names, .after = player_name)
hrs2 <- hrs2 |> rename(batter_name = player_name)
hrs2 <- hrs2 |> rename(pitcher_name = names)
hrs2 = subset(hrs2, select = -c(pitcher))
hrs2$runners_on_base = (!is.na(hrs2$on_3b)) + (!is.na(hrs2$on_2b)) + (!is.na(hrs2$on_1b))
hrs2$hr_rbi = 1 + hrs2$runners_on_base
hrs2 <- hrs2 |> relocate(runners_on_base, .before = outs_when_up)
hrs2 <- hrs2 |> relocate(hr_rbi, .after = runners_on_base)

hrs2 <- hrs2 |>
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
