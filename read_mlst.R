library(readxl)
library(dplyr)

source("helpers.R")

read_plate <- function(file_path) {

  # extract the plate number
  plate_number <- mygrep("(.*SF)([0-9]{3}.*).xls[x]*$", "\\2", file_path)
  cat("plate number = ", plate_number, "\n")

  db <- data.frame(read_excel(file_path))

  # skip rows until we find ASP in column 2
  which_row <- which(toupper(db[,2]) == "ASP")
  if (length(which_row) > 0) {
    db <- data.frame(read_excel(file_path, skip=which_row))
  }

  # find last row in dataset
  last_row <- min(which(is.na(db[,2])))
  db <- db[1:(last_row-1),1:min(ncol(db),10)]
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


# righto, all we do is go and:
# 1. join 'em all up.
# 2. find duplicates by Lab.No (Not sure most efficient route - I guess..

t <- table(all_plates$Lab.No)
dupes   <- names(t)[t > 1]
uniques <- names(t)[t == 1]

unique_labno <- all_plates %>% filter(Lab.No %in% uniques)
duped_labno  <- all_plates %>% filter(!(Lab.No %in% uniques)) %>% arrange(Lab.No)

de_duped <- NULL
still_duped <- NULL
for (labno in dupes) {
  # TODO: This is somewhat inefficient for large number of duplicates.
  #       It would be much nicer if we could extract the row numbers
  #       of the duplicates instead.  Hmm, order by Lab.No then iterate?
  profiles <- duped_labno %>% filter(Lab.No == labno)

  # Resolve these profiles as best we can...

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
    # found complete
    de_duped <- rbind(de_duped, profile)
  } else {
    still_duped <- rbind(still_duped, profiles)
  }
}
still_duped[,1:8]


de_duped <- NULL
still_duped <- NULL
start_row <- 1

#TODO: can probably be made more efficient again...
while (start_row != nrow(duped_labno)) {
  labno <- duped_labno$Lab.No[start_row]
  end_row <- start_row + 1
  while (end_row != nrow(duped_labno)) {
    if (duped_labno$Lab.No[end_row] != labno)
      break;
    end_row <- end_row + 1
  }
  profiles <- duped_labno[start_row:(end_row-1),]

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
    # found complete
    de_duped <- rbind(de_duped, profile)
  } else {
    still_duped <- rbind(still_duped, profiles)
  }

  start_row <- end_row
}

for (row in 1labno in dupes) {
  
  # TODO: This is somewhat inefficient for large number of duplicates.
  #       It would be much nicer if we could extract the row numbers
  #       of the duplicates instead.  Hmm, order by Lab.No then iterate?
  profiles <- duped_labno %>% filter(Lab.No == labno)

  # Resolve these profiles as best we can...

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
    # found complete
    de_duped <- rbind(de_duped, profile)
  } else {
    still_duped <- rbind(still_duped, profiles)
  }
}
still_duped[,1:8]



isolated_data <- rbind(unique_labno, de_duped)

# 3. process each duplicate one by one and resolve as needed

# FIXME:

# CORRECTION FOR DATABASE...

# SF376 has H2008 but should be H157 (which is now a 61)
# SF368 has H2008 and is correct.

# interestingly, this seems to not be the case anymore?????


