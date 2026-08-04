required_packages <- c("plumber", "DBI", "RSQLite")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
invisible(lapply(required_packages, library, character.only = TRUE))

#* @apiTitle mlb-rbihr API
#* @apiDescription API for updating mlb-rbihr database, powered by Baseball Savant

db_path <- "Users/fsobo15/Desktop/Git/data/hr_data.db"
query_db <- function(sql, params = list()) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sql, params = params)
}

#* Get home run records, optionally filtered by year, team, or batter
#* @param year Season year (e.g. 2015)
#* @param team Team abbreviation, matches either home or away team (e.g. TB)
#* @param batter_name Batter name in "Last, First" format
#* @get /homeruns

function(year = NULL, team = NULL, batter_name = NULL) {
  sql <- "SELECT batter_name, COUNT(*) AS hr_count FROM homeruns WHERE 1=1"
  params <- list()
  
  if (!is.null(year)) {
    sql <- paste(sql, "AND game_year = ?")
    params <- c(params, year)
  }
  
    sql <- paste(sql, "GROUP BY batter_name ORDER BY hr_count DESC LIMIT ?")
    params <- c(params, year)
  
    sql <- paste(sql, "GROUP BY batter_name ORDER BY hr_count DESC LIMIT ?")
    params <- c(params, as.integer(limit))

  query_db(sql, params = params)
}