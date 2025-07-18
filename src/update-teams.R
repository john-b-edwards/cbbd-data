source("R/utils.R")

build_teams <- function(query_season = most_recent_season()) {
  teams <- query_cbbd("teams", list(season = query_season))
  teams$season <- query_season
  data.table::setDT(teams)
  data.table::setnames(
    teams,
    old = c(
      "id",
      "source_id",
      "school",
      "mascot",
      "abbreviation",
      "display_name",
      "short_display_name",
      "primary_color",
      "secondary_color",
      "current_venue",
      "current_city",
      "current_state",
      "conference"
    ),
    new = c(
      "team_id",
      "team_source_id",
      "team_name",
      "team_mascot",
      "team_abbreviation",
      "team_display_name",
      "team_short_name",
      "team_primary_color",
      "team_secondary_color",
      "venue_name",
      "venue_city",
      "venue_state",
      "conference_short_name"
    )
  )
  teams <- as.data.frame(teams)
  cbbd_save(teams, paste0("teams_", query_season), "teams")
}

# build all seasons
if (Sys.getenv("TO_UPDATE") == "ALL") {
  purrr::walk(2003:most_recent_season(), build_teams, .progress = T)
} else {
  build_teams()
}
