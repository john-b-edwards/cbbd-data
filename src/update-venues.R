source("R/utils.R")

build_venues <- function() {
  venues <- query_cbbd("venues", list())
  cbbd_save(venues, "venues", "venues")
}

build_venues()
