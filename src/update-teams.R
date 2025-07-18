source("R/utils.R")

build_teams <- function(query_season = cbbreadr::most_recent_season()) {
  teams <- query_cbbd("teams", list(season = query_season))
  teams$season <- query_season
  cbbd_save(teams, paste0("teams_", query_season), "teams")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:cbbreadr::most_recent_season(), build_teams, .progress = T)
} else {
  build_teams()
}
