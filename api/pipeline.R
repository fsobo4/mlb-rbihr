required_packages <- c("plumber", "DBI", "RSQLite", "rapidoc")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
invisible(lapply(required_packages, library, character.only = TRUE))

#* @apiTitle mlb-rbihr API
#* @apiDescription API for updating mlb-rbihr database, powered by Baseball Savant

options("plumber.port" = 8000)
#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

db_path <- "/Users/fsobo15/Desktop/Git/data/hr_data.db"
query_db <- function(sql, params = list()) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sql, params = params)
}

#* Health check - confirms the API is running
#* @get / 
function() {
  list(status = "ok", message = "mlb-rbihr API is running")
}

#* Get leaders in total runs batted in from home runs
#* @param limit Number of top players to return (default 10)
#* @get /homeruns/hr-rbi

function(limit = 10) {
  sql <- "
  SELECT batter_name,
    COUNT (*) AS hr_count,
    SUM(hr_rbi) AS total_rbi_from_hr
  FROM homeruns
  GROUP BY batter_name
  ORDER BY total_rbi_from_hr DESC
  LIMIT ?
  "

  query_db(sql, params = list(as.integer(limit)))
}


#* Get home run leaders for a given year
#* @param year Season year (e.g. 2015)
#* @param limit Number of top hitters to return (default 10)
#* @get /homeruns/leaders

function(year = NULL, limit = 10) {
  sql <- "SELECT batter_name, COUNT(*) AS hr_count FROM homeruns WHERE 1=1"
  params <- list()
  
  if (!is.null(year)) {
    sql <- paste(sql, "AND game_year = ?")
    params <- c(params, year)
  }
  
    sql <- paste(sql, "GROUP BY batter_name ORDER BY hr_count DESC LIMIT ?")
    params <- c(params, as.integer(limit))

  query_db(sql, params = params)
  
}

#* Get top batters by average RBI produced per home run
#* @param limit Number of top players to return (default 10)
#* @param min_hr Minimum home runs required to qualify (default 25)
#* @get /homeruns/rbi-leaders

function(limit = 10, min_hr = 25) {
  sql <- "
  SELECT batter_name,
    COUNT (*) AS hr_count,
    SUM(hr_rbi) AS total_rbi,
    ROUND(AVG(hr_rbi), 3) AS avg_rbi_per_hr
  FROM homeruns
  GROUP BY batter_name
  HAVING COUNT(*) >= ?
  ORDER BY avg_rbi_per_hr DESC
  LIMIT ?
  "
  query_db(sql, params = list(as.integer(min_hr), as.integer(limit)))
}