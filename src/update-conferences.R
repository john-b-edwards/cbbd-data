source("R/utils.R")

build_conferences <- function() {
  conferences <- query_cbbd("conferences", list())
  cbbd_save(conferences, "conferences", "conferences")
}

build_conferences()
