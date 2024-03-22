options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

library(googlesheets4)
library(googledrive)
library(dplyr)

sheet = "1FEthK9U-0wA_ZlGhX7jTtdQFfcWhKz9lAxrAXFucH1Q"

get_last_thursday <- function() {
  today <- Sys.Date()
  # Weekdays in R are ordered such that Sunday is 1
  days_since_thursday <- (as.POSIXlt(today)$wday - 5 + 7) %% 7
  if (days_since_thursday == 0) {
    days_since_thursday <- 7
  }
  last_thursday <- today - days_since_thursday
  return(last_thursday)
}

get_next_thursday <- function() {
  today <- Sys.Date()
  # Weekdays in R are ordered such that Sunday is 1
  daysUntilNextThursday <- (5 - as.POSIXlt(today)$wday + 7) %% 7
  if (daysUntilNextThursday == 0) {
    daysUntilNextThursday <- 7
  }
  nextThursday <- today + daysUntilNextThursday
  return(nextThursday)
}

get_metadata = function(){
  meta = read_sheet(sheet, "meta")
  meta
}

get_attendees = function(){
  attendees = read_sheet(sheet, "attendees")
  attendees[as.Date(attendees$date) > get_last_thursday(), "attendee"]
 }

append_attendee = function(name){
  df = data.frame(date = Sys.Date(), attendee = name)
  googlesheets4::sheet_append(sheet, sheet = "attendees", data = df)
}

last_update = function(){
  drive_info <- drive_get(as_id(sheet))
  drive_info[[3]][[1]][["size"]]
}

last_update()
