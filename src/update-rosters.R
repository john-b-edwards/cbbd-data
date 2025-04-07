source("R/utils.R")

build_rosters <- function(query_season = most_recent_season()) {
  rosters <- query_cbbd("teams/roster", list(season = query_season))
  rosters <- rosters |>
    tidyr::unnest_longer(players) |>
    tidyr::unnest_wider(players, names_sep = "_") |>
    tidyr::unnest_wider(players_hometown, names_sep = "_") |>
    tidyr::unnest_wider(players_hometown_1, names_sep = "_") |>
    janitor::clean_names()
  rosters$season <- query_season
  cbbd_save(rosters, paste0("rosters_", query_season), "rosters")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:most_recent_season(), build_rosters, .progress = T)
} else {
  build_rosters()
}
