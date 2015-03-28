# functions for reading data in order to merge datasets

source("extractors.R")
source("merge_db.R")
source("helpers.R")

library(dplyr)

# read in the massey questions files and merge them together
read_massey <- function(episurv_path) {

  massey_files <- list.files(episurv_path, pattern="Massey.*.csv")

  massey <- read.csv(file.path(episurv_path, massey_files[1]), header=T, stringsAsFactors=F, colClasses="character")
  for (i in massey_files[-1]) {
    next_massey <- read.csv(file.path(episurv_path, i), header=T, stringsAsFactors=F, colClasses="character")
    if (any(names(next_massey) != names(massey))) {
      stop("columns don't match in Massey question files - need to use intersection of available columns or some such")
    }
    massey <- merge(massey, next_massey, all.x=T, all.y=T)
  }

  # filter out non-campy cases
  allowed_disease <- c("CAMPLYLOBACTER",
                       "Campy",
                       "campylobacter",
                       "Campylobacter",
                       "CAMPYLOBACTER",
                       "Camyplobacter",
                       "CAPYLOBACTER")

  # spit out the diseases filtered out
  filtered <- massey %>% filter(!disease %in% allowed_disease)
  cat("Diseases filtered out are:\n")
  print(table(filtered$disease))

  massey <- massey %>% filter(disease %in% allowed_disease)

  # read in the fixes file, and fixup episurv and hospital number where known fixed exist
  massey_fixes <- read.csv(file.path(episurv_path, "fixes_massey.csv"), stringsAsFactors=F)

  fixed_rows <- massey_fixes$action_epi == "fixed"
  replace_rows <- which(massey$id %in% massey_fixes$id[fixed_rows])
  cat("Replacing entries with fixed entries:\n")
  print(cbind(massey$episurvnum[replace_rows], massey_fixes$episurv[fixed_rows]))
  massey$episurvnum[replace_rows] <- massey_fixes$episurv[fixed_rows]

  fixed_rows <- massey_fixes$action_hosp == "fixed"
  replace_rows <- which(massey$id %in% massey_fixes$id[fixed_rows])
  cat("Replacing entries with fixed entries:\n")
  print(cbind(massey$hospitalnu[replace_rows], massey_fixes$hospital[fixed_rows]))
  massey$hospitalnu[replace_rows] <- massey_fixes$hospital[fixed_rows]

  massey
}


# read in the episurv information
read_edr <- function(episurv_path) {

  # function to read a single file
  read_single_edr <- function(file) {
    db <- read.csv(file, header=F, stringsAsFactors=F)
    # figure out how many times to skip
    for (i in 1:ncol(db)) {
      skip = which(db[,i] == "EpiSurvNumber")
      if (length(skip) == 1) {
        db <- read.csv(file, header=T, stringsAsFactors=F, colClasses="character", skip=skip-1)
        if (i != 1) {
          db <- db[-(1:(i-1))]
        }
        return(db)
      }
    }
    return(NA)
  }

  edr_files <- list.files(episurv_path, pattern="EntericDiseaseReport.*.csv")

  edr <- read_single_edr(file.path(episurv_path, edr_files[1]))
  for (i in edr_files[-1]) {
    next_edr <- read_single_edr(file.path(episurv_path, i))
    if (any(names(next_edr) != names(edr))) {
      stop("columns don't match in EDR files - need to use intersection of available columns or some such")
    }
    edr <- merge(edr, next_edr, all.x=T, all.y=T)
  }

  edr
}

# read isolate information, and merge in extras as needed
read_isolate <- function(isolate_path, read_extra = T) {

  # read the base isolate table
  isolate_file <- find_latest_version(isolate_path, full_path=FALSE) # false to get filename without path

  isolate <- read.csv(file.path(isolate_path, isolate_file), stringsAsFactors=F, colClasses="character")

  # read in any additional isolates
  if (read_extra) {
    isolate_file_regex <- gsub("^(.*)\\.csv$", "\\1_extras(.*).csv", isolate_file)
    extra_files <- sort(list.files(isolate_path, isolate_file_regex))
    extra_isolate_ids <- NULL
    for (extra_file in extra_files) {
      cat("Adding extra isolates found in '", extra_file, "'\n", sep="")
      extras <- read.csv(file.path(isolate_path, extra_file), stringsAsFactors=F, colClasses="character")

      col_extras <- c("Lab.ID", "Lab.No", "ST", "ASP", "GLN", "GLT", "GLY", "PGM", "TKT", "UNC")

      if (any(!(col_extras %in% names(extras)))) {
        stop(paste0("Missing columns ", col_extras[!(col_extras %in% names(extras))], " in extras file '", extra_file, "'"))
      }

      # filter incompletes out
      extras <- extras %>% filter_missing_mlst

      if (length(intersect(extras$Lab.No, extra_isolate_ids)) > 0) {
        cat("WARNING: Multiple Lab.No's in the extra isolate files:\n")
        print(intersect(extras$Lab.No, extra_isolate_ids))
      }

      extra_isolate_ids <- union(extra_isolate_ids, extras$Lab.No)

      # select only columns we want to merge
      extras <- extras %>% select(one_of(col_extras))

      # merge the databases
      isolate <- merge_db(isolate, extras, "Lab.No", add_new = TRUE)
      #readline()
    }
  }

  isolate
}

# reads the urban/rural concordance file (use Meshblock06 for joining)
read_urban_rural <- function(ur_concordance_file) {

  # load concordance file to map urban/rural status
  ur <- read.csv(ur_concordance_file, stringsAsFactors=F)
  names(ur) <- c("Meshblock06", "UR_cat")

  # convert UR_cat to a number, and U/R binary
  ur_tab <- read.table(header=T, sep=",", stringsAsFactors=F, strip.white=T, text = "
            UR_cat,                               UR_num, UR_bool      
            Area outside urban/rural profile,         NA, NA 
            Highly rural/remote area,                 -3, Rural
            Rural area with low urban influence,      -2, Rural
            Rural area with moderate urban influence, -1, Rural
            Rural area with high urban influence,      0, Urban
            Independent Urban Area,                    1, Urban
            Satellite Urban Area,                      2, Urban
            Main urban area,                           3, Urban"
  )

  ur %>% left_join(ur_tab, by="UR_cat")
}
