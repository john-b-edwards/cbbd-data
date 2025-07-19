source("R/utils.R")

update_recruiting <- function() {
  recruiting <- query_cbbd("recruiting/players", list()) |>
    tidyr::unnest_wider(hometown, names_sep = "_") |>
    tidyr::unnest_wider(committed_to, names_sep = "_")
  data.table::setDT(recruiting)
  data.table::setnames(
    recruiting,
    old = c(
      "id",
      "source_id",
      "position",
      "school_id",
      "school",
      "committed_to_id",
      "committed_to_name",
      "committed_to_conference",
      "name",
      "height_inches",
      "weight_pounds",
      "year"
    ),
    new = c(
      "recruit_id",
      "recruit_source_id",
      "recruit_position",
      "high_school_id",
      "high_school_name",
      "committed_to_team_id",
      "committed_to_team_name",
      "committed_to_conference_short_name",
      "athlete_name",
      "recruit_height_inches",
      "recruit_weight_pounds",
      "recruit_year"
    )
  )
  data.table::setDF(recruiting)
  cbbd_save(recruiting, "recruiting", "recruiting")
}

update_recruiting()
