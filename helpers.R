# R helpers

# grep that returns empty if no hit is found, else returns the replace string
mygrep <- function(match, replace, x, ...) {
  ind <- grepl(match, x, ...)
  y   <- gsub(match, replace, x, ...)
  y[!ind] <- ""
  return(y)
}

# find the latest version of a file
find_latest_version <- function(path, prefix = "", full_path = TRUE) {

  files <- list.files(path, paste0("^", prefix, "20[0-9]{2}[0-1][0-9][0-3][0-9]\\.(txt|csv)$"))
  if (length(files) == 0) {
    stop(paste("No suitable files found in", path))
  }

  # choose the latest
  file <- rev(sort(files))[1]
  print(paste0("Using '", file, "' as latest file in '", path, "'"))
  if (full_path) {
    file <- file.path(path, file)
  }
  return(file)
}

# filter those with no ST information at all (in any alleles)
filter_missing_mlst <- function(db) {
  filter(db, ASP != "" |
             GLN != "" |
             GLT != "" |
             GLY != "" |
             PGM != "" |
             TKT != "" |
             UNC != "")
}

# warning-free conversion to numeric
as_numeric <- function(x, with_warnings = FALSE) {
  if (with_warnings) {
    as.numeric(x)
  } else {
    suppressWarnings(as.numeric(x))
  }
}

# function for modifying colours to make them transparent
alpha <- function(col, a)
{
  rgb(t(col2rgb(col)/255), alpha=a)
}

