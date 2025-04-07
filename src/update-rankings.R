source("R/utils.R")

update_rankings <- function() {
  rankings <- query_cbbd("rankings", list())
  cbbd_save(rankings, "rankings", "rankings")
}

update_rankings()
