source("R/utils.R")

build_conferences <- function() {
  conferences <- query_cbbd("conferences", list())
  data.table::setDT(conferences)
  data.table::setnames(
    conferences,
    new = c(
      "conference_id",
      "conference_source_id",
      "conference_name",
      "conference_abbreviation",
      "conference_short_name"
    )
  )
  conferences <- as.data.frame(conferences)
  cbbd_save(conferences, "conferences", "conferences")
}

build_conferences()
