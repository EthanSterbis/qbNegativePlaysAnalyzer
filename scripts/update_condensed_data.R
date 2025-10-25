# scripts/update_condensed_data.R
# =====================================================
# Sterb's NFL QB Negative Plays Analyzer - Data Updater
# Creates condensed play-by-play CSV for app use
# =====================================================

library(dplyr)
library(readr)
library(nflreadr)

# ---- Config ----
seasons <- 2015:2025
out_file <- "data/condensed.csv"

# ---- Load and Condense ----
pbp <- nflreadr::load_pbp(seasons = seasons)

# Keep only columns actually needed in the app:
condensed <- pbp %>%
  select(
    season, season_type, week, game_id, posteam, defteam,
    passer, passer_id, qb_dropback, sack, interception, fumble,
    epa, wpa, xpass, down, qtr, wp, play_id
  )

# ---- Save ----
write_csv(condensed, out_file)

message("Condensed play-by-play saved to: ", out_file)
