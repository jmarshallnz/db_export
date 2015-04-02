library(readxl)
library(dplyr)

source("helpers.R")

read_plate <- function(file_path) {

  # extract the plate number
  plate_number <- mygrep("(.*SF)(.*[0-9]{3}.*).xls[x]*$", "\\2", file_path)
  cat("plate number = ", plate_number, ", file_path <- '", file_path, "'\n", sep="")

  db <- data.frame(read_excel(file_path))

  # skip rows until we find ASP in column 2
  which_row <- which(toupper(db[,2]) == "ASP")

  if (length(which_row) == 0 && toupper(names(db)[2]) != "ASP") {
    cat("POTENTIAL BUG HIT! - no ASP found in column 2 in file_path <- '", file_path, "'\n", sep="")
    readline()
  }

  if (length(which_row) > 0) {
    db <- data.frame(read_excel(file_path, skip=which_row[1]))
    if (length(which_row) > 1) {
      cat("WARNING!!! there's multiple ASP in this data sheet, which suggests mucking about...\n")
    }
  }

  # BUG: There's a bug in readxl() where some excel sheets have a blank column first, screwing up the data.frame
  if (length(db[,2]) != nrow(db)) {
    cat("BUG HIT!, nrow(db) =", nrow(db), "!=", length(db[,2]), "= length(db[,2]) on plate", plate_number, "\n")
    readline()
  }

  # BUG: There's a bug in readxl() where some excel sheets have empty columns that are named. These are skipped
  #      for some reason.

  # NOTE: in the case ASP is never found, we're screwed...

  names(db) <- toupper(names(db))
  cols <- c("ASP", "GLN", "GLT", "GLY", "PGM", "TKT", "UNC")
  if (length(setdiff(cols, names(db))) > 0) {
    cat("BUG HIT! Missing columns", setdiff(cols, names(db)), "\n")

    # reassemble with blank columns
    new_db <- db[,1:2]

    for (i in 2:length(cols)) {
      if (cols[i] %in% names(db)) {
        new_db <- cbind(new_db,db[,cols[i]])
      } else {
        new_db <- cbind(new_db, NA)
      }
    }
    names(new_db)[2:8] <- cols

    db <- new_db
  }

  # allowed fields...
  # NEW, X, number, NA, MIX, DYE

  row_valid         <- rep(T, nrow(db))
#  row_empty_profile <- rep(F, nrow_db)
  for (i in seq_len(nrow(db))) {
    profile <- db[i,2:8]
    str_profile <- tolower(profile)
    num_profile <- suppressWarnings(as.numeric(profile));
    profile_valid <- sum(substring(str_profile, 1, 3) == "new" |
                         substring(str_profile, 1, 1) == "x" |
                         is.na(profile) |
                        !is.na(num_profile) |
                         substring(str_profile,1,3) == "mix" |
                         substring(str_profile,1,3) == "dye") == 7
    profile_nonempty <- sum(is.na(profile)) < 7
    row_valid[i] <- profile_valid && profile_nonempty && !is.na(db[i,1])
#    row_empty_profile[i] <- profile_valid && !is.na(db[i,1])
  }

  # take everything up to the last valid row
  # NOTE: There's like blank ones that we'll remove here, but they're useless information as far as putting the
  #       data into the database.
  #       (Not useless necessarily from a QC point of view)
#  # NOTE: We allow invalid rows earlier than this that have a labid, as this may mean a blank (i.e. not done) lab number etc.
#  #       but don't allow completely blank ones
  last_valid <- max(which(row_valid))
  rows <- c(rep(T, last_valid), rep(F, nrow(db)-last_valid)) & row_valid # &row_empty_profile

  db <- db[rows, 1:min(ncol(db),10)]

  # expand to at least 10 columns
  while(ncol(db) < 10) {
    db <- cbind(db, NA)
  }

  # convert names and add plate number
  names(db) <- c("Lab.No", toupper(names(db)[2:8]), "ST_phil", "Notes")
  db$Plate <- plate_number

  # convert all the numbers to numbers
  for (i in 1:10) {
    nums <- suppressWarnings(as.numeric(db[,i]))
    db[,i] <- ifelse(is.na(nums), db[,i], nums)
  }

  db
}

plate_dirs <- list.dirs("../database/mlst", recursive=F)
all_plates <- NULL
for (dir in plate_dirs) {
  files <- list.files(dir) # hmm, how to derive the plate number?
  for (f in files) {
    plate <- read_plate(file.path(dir, f))
    all_plates <- rbind(all_plates, plate)
  }
}

# check data

all_plates[nchar(all_plates$Lab.No) > 15,c(1:8,11)]

# Fixup a couple of silly things in the Lab.No bit...
utf8_rows <- Encoding(all_plates$Lab.No) == "UTF-8"
all_plates$Lab.No[utf8_rows] <- substring(all_plates$Lab.No[utf8_rows],1,nchar(all_plates$Lab.No[utf8_rows])-1)
any(Encoding(all_plates$Lab.No) != "unknown")


# righto, all we do is go and:
# 1. join 'em all up.
# 2. find duplicates by Lab.No (Not sure most efficient route - I guess..

t <- table(all_plates$Lab.No)
dupes   <- names(t)[t > 1]
uniques <- names(t)[t == 1]

unique_labno <- all_plates %>% filter(Lab.No %in% uniques)
duped_labno  <- all_plates %>% filter(!(Lab.No %in% uniques)) %>% arrange(Lab.No)

resolve_duplicate <- function(profiles) {

  # we do this by:
  # 1. Keep NEW as it is, by transforming to a number
  # NEW or a number are unique.  Anything else is NA

  profile <- profiles[1,]
  profile[2:8] <- NA
  for (i in 2:8) {
    numeric <- suppressWarnings(as.numeric(profiles[,i]))
    allele <- ifelse(!is.na(numeric), numeric, ifelse(toupper(profiles[,i]) == "NEW", profiles[,i], ""))
    allele[is.na(allele)] <- ""
    vals <- unique(allele[allele != ""])
    if (length(vals) == 1)
      profile[i] <- vals
    if (length(vals) == 0) # occurs where there's a unique match but it's empty (i.e. X/MIX etc.)
      profile[i] <- "X"
  }
  if (!any(is.na(profile[2:8]))) {
    profile
  } else {
    profiles
  }
}

de_duped <- NULL
still_duped <- NULL
labnumbers <- duped_labno$Lab.No
n_dupes    <- length(labnumbers)

start_row  <- 1
while (start_row < n_dupes) {

  # find the row where the duplicates end
  end_row <- start_row + 1
  while (end_row <= n_dupes &&
         labnumbers[end_row] == labnumbers[start_row])
    end_row <- end_row + 1

  resolved <- resolve_duplicate(duped_labno[start_row:(end_row-1),])

  # add to the appropriate list
  if (nrow(resolved) == 1) {
    de_duped <- rbind(de_duped, resolved)
  } else {
    still_duped <- rbind(still_duped, resolved)
  }

  start_row <- end_row
}

plate_data <- rbind(unique_labno, de_duped)

# read in the actual isolates and see how well we do...
source("read_data.R")

isolates <- read_isolate("../database/isolates") %>% select(Lab.No, ASP, GLN, GLT, GLY, PGM, TKT, UNC, ST)

# righto, join 'em up and see how we do

joined_data <- plate_data %>% left_join(isolates, by="Lab.No")

# find differences:
diff_data <- joined_data %>% filter(ASP.x != ASP.y |
                                   GLN.x != GLN.y |
                                   GLT.x != GLT.y |
                                   GLY.x != GLY.y |
                                   PGM.x != PGM.y |
                                   TKT.x != TKT.y |
                                   UNC.x != UNC.y)

# some 301 rows different.
diff_data[,c(1:8,12:18)]

# which ones are in isolates table but not read in the above code
labnos <- unique(all_plates$Lab.No)

not_found <- isolates %>% filter(!(Lab.No %in% labnos)) %>% filter(ASP != "" |
                   GLN != "" |
                   GLT != "" |
                   GLY != "" |
                   PGM != "" |
                   TKT != "" |
                   UNC != "" | 
                   ST != "")

# some 2196 rows (not bad out of 12k I guess???)

# a bunch of these are labelled UK - one presumes these are sourced from elsewhere...











# FIXME:

# CORRECTION FOR DATABASE...

# SF376 has H2008 but should be H157 (which is now a 61)
# SF368 has H2008 and is correct.

# interestingly, this seems to not be the case anymore?????


