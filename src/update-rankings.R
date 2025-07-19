source("R/utils.R")

update_rankings <- function() {
  rankings <- query_cbbd("rankings", list())
  data.table::setDT(rankings)
  data.table::setnames(
    rankings,
    old = c("team", "conference"),
    new = c("team_name", "conference_short_name")
  )
  data.table::setDF(rankings)
  cbbd_save(rankings, "rankings", "rankings")
}

update_rankings()
