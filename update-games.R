source("R/utils.R")

build_games <- function(query_season = most_recent_season()) {
  query_cbbd("games",list(season = query_season))
}

