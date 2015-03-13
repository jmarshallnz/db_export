# functions for extracting data from various fields etc.

# grep that returns empty if no hit is found, else returns the replace string
mygrep <- function(match, replace, x, ...) {
  ind <- grepl(match, x, ...)
  y   <- gsub(match, replace, x, ...)
  y[!ind] <- ""
  return(y)
}

# extract potential hospital number from comment field of episurv
extract_hospital_from_comment <- function(comment) {

  first_crack <- mygrep("^ *([0-9]{10}[0-9]*).*", "\\1", comment)

  # those that are too long, we check whether they match ##########3800[##], and if so drop the 3800[##]
  hosp_num <- mygrep("^([0-9]{10})3800.*", "\\1", first_crack)
  use_first_crack <- hosp_num == "" & first_crack != ""
  hosp_num[use_first_crack] <- first_crack[use_first_crack]

  hosp_num
}

# compute Age brackets (TODO: ideally this would be done in the database)
extract_age_bracket <- function(age) {
  grp <- (as.numeric(age) %/% 5) * 5
  paste0(grp, " to ", grp+4)
}

# extracts date using lubridate
extract_date <- function(x) {
  after <- dmy(substring(x,1,10), quiet=T)
  # TODO: summarise the warnings from the date conversion
  wrong <- is.na(after) & x != "" & !is.na(x)
  if (any(wrong)) {
    cat("Date conversion failed for: ")
    cat(x[wrong], "\n")
  }
  after
}

# extracts lab id from isolate id.  Assumes isolate id's take the form LABID[a-f]
extract_lab_id_from_isolate_id <- function(id) {
  mygrep("^(.*[0-9]+)[a-f]*$", "\\1", id)
}

