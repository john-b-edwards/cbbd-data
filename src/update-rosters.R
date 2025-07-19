source("R/utils.R")

build_rosters <- function(query_season = cbbreadr::most_recent_season()) {
  rosters <- query_cbbd("teams/roster", list(season = query_season))
  rosters <- rosters |>
    tidyr::unnest_longer(players) |>
    tidyr::unnest_wider(players, names_sep = "_") |>
    tidyr::unnest_wider(players_hometown, names_sep = "_") |>
    tidyr::unnest_wider(players_hometown_1, names_sep = "_") |>
    janitor::clean_names()
  rosters$season <- query_season
  data.table::setDT(rosters)
  data.table::setnames(
    rosters,
    old = c(
      "conference",
      "players_id",
      "players_source_id",
      "players_name",
      "players_first_name",
      "players_last_name",
      "players_jersey",
      "players_position",
      "players_height",
      "players_weight",
      "players_hometown_1_city",
      "players_hometown_1_state",
      "players_hometown_1_country",
      "players_hometown_1_latitude",
      "players_hometown_1_longitude",
      "players_hometown_1_county_fips",
      "players_date_of_birth",
      "players_start_season",
      "players_end_season"
    ),
    new = c(
      "conference_short_name",
      "athlete_id",
      "athlete_source_id",
      "athlete_name",
      "athlete_first_name",
      "athlete_last_name",
      "athlete_jersey_number",
      "athlete_position",
      "athlete_height_inches",
      "athlete_weight_pounds",
      "athlete_hometown_city",
      "athlete_hometown_state",
      "athlete_hometown_country",
      "athlete_hometown_latitude",
      "athlete_hometown_longitude",
      "athlete_hometown_county_fips",
      "athlete_date_of_birth",
      "athlete_start_season",
      "athlete_end_season"
    )
  )
  data.table::setDF(rosters)
  cbbd_save(rosters, paste0("rosters_", query_season), "rosters")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:cbbreadr::most_recent_season(), build_rosters, .progress = T)
} else {
  build_rosters()
}
