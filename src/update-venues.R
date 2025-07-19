source("R/utils.R")

build_venues <- function() {
  venues <- query_cbbd("venues", list())
  data.table::setDT(venues)
  data.table::setnames(
    venues,
    old = c("id", "source_id", "name", "city", "state", "country"),
    new = c(
      "venue_id",
      "venue_source_id",
      "venue_name",
      "venue_city",
      "venue_state",
      "venue_country"
    )
  )
  data.table::setDF(venues)
  cbbd_save(venues, "venues", "venues")
}

build_venues()
