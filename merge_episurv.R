# This script is to to validate the episurv information

source("read_data.R")
source("extractors.R")
source("validators.R")
source("merge_db.R")

library(dplyr)
library(lubridate)

episurv_path <- "../EpisurvData"

# read in MasseyQuestion and EntericDiseaseReport files
massey <- read_massey(episurv_path)
edr    <- read_edr(episurv_path)

# don't add known shit to the database
massey <- massey[!validate_episurv(massey$episurvnum),]
edr    <- edr[!validate_episurv(edr$EpiSurvNumber),]

# TODO: Run a validation report on the data we're merging
# TODO: validate against sample table??

# read in existing master sheet
master <- read.csv(file.path(episurv_path, "master_20150115.csv"), stringsAsFactors=F, colClasses="character")

# remove 'tfhj' row, and any missing both EpiSurvNumber and HospitalNo
master <- master %>% filter(EpiSurvNumber != "tfhj")
master <- master %>% filter(!(EpiSurvNumber == "" & Hospital.Numbers == ""))

# remap massey column names
massey <- massey %>% transmute(EpiSurvNumber=episurvnum,
                               Hospital.Numbers=hospitalnu,
                               other.hos.nos=hospitaln1,
                               other.hos.nos.2=hospitaln2,
                               PhoneId="",
                               Date.notified="",
                               DateCall="",
                               Chicken=chicken,
                               Beef=beef,
                               Pork=pork,
                               Lamb=lamb,
                               DeliHam=delimeats,
                               Bacon="",
                               Venison=huntedmeat,
                               MeatMemo="",
                               UnpasturisedMilk=rawmilk)

# TODO: ideally this would be done in the database
edr <- edr %>% mutate(Age5yrbrac = extract_age_bracket(edr$AgeInYears))

# converts dates
convert_dates <- function(db, date_col) {
  for (d in intersect(date_col, names(db))) {
    db[,d] <- extract_date(db[,d])
  }
  db
}

# convert date columns
date_col <- c("ReportDate",
           "OnsetDt",
           paste0("DateConsumed",1:8),
           "OthRecDate",
           paste0("PoolDate", 1:3),
           paste0("RiverSeaDate", 1:3),
           "DtArrived",
           "DtLastDeparted",
           "DtLastEntered",
           "DtSecDeparted",
           "DtSecEntered",
           "DtThirdDeparted",      
           "DtThirdEntered",
           "Date.notified",
           "DateCall")

master <- convert_dates(master, date_col)
edr    <- convert_dates(edr, date_col)
massey <- convert_dates(massey, date_col)

# OK, now go through and merge the data sets



# check column names

cols_common_master_edr <- intersect(names(master), names(edr))
cols_master_not_edr <- setdiff(names(master), names(edr))
cols_edr_not_master <- setdiff(names(edr), names(master))

cols_common_massey_master <- intersect(names(master), names(massey))
cols_master_not_massey <- setdiff(names(master), names(massey))
cols_massey_not_master <- setdiff(names(massey), names(master))

cat("Columns in the master set that aren't covered:\n")
print(setdiff(cols_master_not_massey, names(edr)))

cat("Columns in EDR that aren't covered in master sheet:\n")
print(setdiff(names(edr), c(cols_master_not_massey, "EpiSurvNumber")))

# check matching to EpiSurvNumber
master_ids <- master %>% select(EpiSurvNumber)
edr_ids <- edr %>% select(EpiSurvNumber)

rows <- nrow(edr_ids %>% anti_join(master_ids))
if (rows > 0) {
  cat("WARNING:", rows, "rows in Enteric Disease Report", attr(edr, "db_name"), "that aren't in master sheet\n")
}

# check matching to hospital number
master_hos <- master %>% select(Hospital.Numbers)
massey_hos <- massey %>% select(Hospital.Numbers)

rows <- nrow(massey_hos %>% anti_join(master_hos))
if (rows > 0) {
  cat("WARNING:", rows, "rows in Massey questions", attr(edr, "db_name"), "that aren't in master sheet\n")
}

# check matching to EpiSurvNumber
master_en <- master %>% select(EpiSurvNumber)
massey_en <- massey %>% select(EpiSurvNumber)

rows <- nrow(massey_en %>% anti_join(master_en))
if (rows > 0) {
  cat("WARNING:", rows, "rows in Massey questions", attr(edr, "db_name"), "that aren't in master sheet\n")
}

# check matching to EpiSurvNumber
edr_en <- edr %>% select(EpiSurvNumber)
massey_en <- massey %>% select(EpiSurvNumber)

rows <- nrow(massey_en %>% anti_join(edr_en))
if (rows > 0) {
  cat("WARNING:", rows, "rows in Massey questions", attr(edr, "db_name"), "that aren't in EDR sheet\n")
}

rows <- nrow(edr_en %>% anti_join(massey_en))
if (rows > 0) {
  cat("WARNING:", rows, "rows in EDR", attr(edr, "db_name"), "that aren't in Massey Questions sheet\n")
}

# check matching EpiSurv+Hospital
master_en <- master %>% select(EpiSurvNumber, Hospital.Numbers)
massey_en <- massey %>% select(EpiSurvNumber, Hospital.Numbers)

# TODO: seems to be a bunch of hospital number disagreement :(
nomatch <- massey %>% inner_join(master, by="EpiSurvNumber") %>% filter(Hospital.Numbers.x != Hospital.Numbers.y)
nomatch <- nomatch %>% mutate(Hospital.Numbers.y = substring(Hospital.Numbers.y,1,15))
nomatch %>% select(EpiSurvNumber, Hospital.Numbers.x, Hospital.Numbers.y, other.hos.nos.x, other.hos.nos.y)

# match these against hospital numbers extracted from episurv data
hosp_num <- extract_hospital_from_comment(edr$Comments)

edr_hn <- data.frame(EpiSurvNumber = edr$EpiSurvNumber, hosp_num)

#nomatch %>% left_join(edr_hn, by="EpiSurvNumber") %>% select(EpiSurvNumber, Hospital.Numbers.x, hosp_num, Hospital.Numbers.y, other.hos.nos.x, other.hos.nos.y)
#nomatch <- joint %>% filter(Hospital.Numbers.x != Hospital.Numbers.y)

#nomatch %>% select(EpiSurvNumber, Hospital.Numbers.x, Hospital.Numbers.y)


#master %>% filter(Hospital.Numbers %in% nomatch$Hospital.Numbers.y)

# Now, do the merging
# for now we assume the episurv info is GOLD STANDARD and copy across all of that (even if it overrides what
# is already in place

master_new <- master

master_new <- merge_db(master_new, edr, "EpiSurvNumber")
master_new <- merge_db(master_new, massey, "EpiSurvNumber", add_new=FALSE)
master_new <- merge_db(master_new, massey, "Hospital.Numbers", add_new=FALSE)

# write the new master sheet out
write.csv(master_new, file.path(episurv_path, "master_new.csv"), row.names=F, na="")

