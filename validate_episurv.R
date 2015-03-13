# This script is to to validate the episurv information

source("read_data.R")
source("extractors.R")
source("validators.R")

library(dplyr)
library(lubridate)

episurv_path <- "../EpisurvData"

massey <- read_massey(episurv_path)
edr    <- read_edr(episurv_path)

hosp_num <- extract_hospital_from_comment(edr$Comment)

episurv_hn <- data.frame(EpiSurvNumber = edr$EpiSurvNumber, HospitalNumComment = hosp_num, stringsAsFactors=F)

dat <- validate_hosp_epi(episurv_hn$HospitalNumComment, episurv_hn$EpiSurvNumber)

results_epi <- cbind(episurv_hn, dat)

compare <- results_epi %>% outer_join(results_massey, by=c("EpiSurvNumber" = "episurvnum"))

compare <- compare %>% mutate(hosp_same = HospitalNumComment == hospitalnu) %>% arrange(as.numeric(id))

# find ones where hosp num disagrees
diff_hosp <- compare %>% filter(hosp_same == F)

# TODO:
# righto, now join the episurv_hn data frame to our massey_hn data frame and see where things stand...

compare_hn <- episurv_hn %>% outer_join(massey_hn, by=c("EpiSurvNumber" = "episurvnum"))

compare_hn <- compare_hn %>% mutate(hosp_same = HospitalNumComment == hospitalnu) %>% arrange(as.numeric(id))

write.csv(compare, "comparison.csv", row.names=F)


# create 'fixup' file for massey
fixup_massey <- results_massey %>% filter(bad == TRUE) %>% mutate(action="") %>% select(id, action, episurv=episurvnum, hospital=hospitalnu, reason=bad_reason, resid=resid_col, student=resid_col2)
write.csv(fixup_massey, "fixup_massey.csv", row.names=F)

# create 'fixup' file for episurv
fixup_epi <- results_epi %>% filter(bad == TRUE) %>% mutate(action="") %>% select(id, action, episurv=EpiSurvNumber, hospital=HospitalNumComment, reason=bad_reason, resid=resid_col, student=resid_col2)
write.csv(fixup_epi, "fixup_epi.csv", row.names=F)

results_epi <- cbind(episurv_hn, dat)







# read in the sample level data from the database
sample <- read.csv("../Sample_20150128.csv", stringsAsFactors=F, colClasses="character")

# filter sample table
sample <- sample %>% filter(SA_model_source == "Human")
sample <- sample %>% filter(Hospital.No != "" | LinkNo != "")

# compare the hospital numbers
table(nchar(sample$Hospital.No))

sample$Hospital.No[nchar(sample$Hospital.No) > 10]
sample$LinkNo[nchar(sample$LinkNo) > 10]

massey$hospitalnu[nchar(massey$hospitalnu) > 10]

# find which hospitalnu's we are missing in the sample table
massey_hn <- massey %>% select(id, episurvnum, hospitalnu, hospitaln1, hospitaln2, disease)

sample_hn <- sample %>% select(Lab.ID, Hospital.No, LinkNo)

# find which ones we don't have in the sample table...
massey_hn %>% filter(!(hospitalnu %in% sample_hn$Hospital.No))

# find which ones we do have in the sample table
nrow(massey_hn %>% filter(hospitalnu %in% sample_hn$Hospital.No))

nrow(massey_hn %>% filter(substring(hospitalnu,1,10) %in% sample_hn$Hospital.No))

missing_samples <- massey_hn %>% filter(!(substring(hospitalnu,1,10) %in% sample_hn$LinkNo))
#missing_samples %>% mutate(sh 

massey[,c("id","episurvnum", "hospitalnu")] %>% arrange(episurvnum) %>% mutate(dupe_epi=duplicated(episurvnum), dupe_hosp=duplicated(hospitalnu))

massey_hn


dat <- validate_hosp_epi(massey_hn$hospitalnu, massey_hn$episurvnum)

results_massey <- cbind(massey_hn, dat) %>% mutate(id = as.numeric(id)) %>% arrange(id)

write.csv(results, "MasseyQuestions_validation.csv", row.names=F)

mod.1 <- lm(epi ~ hosp + year, data=dat[!dat$bad,])

library(nlme)

mod.2 <- gls(epi ~ hosp + year, correlation=corAR1(), data=dat[!dat$bad,])

# some validation on hospital numbers vs episurv numbers

massey_hn <- massey_hn %>% mutate(EpiOrder=paste0(substring(episurvnum,1,2),substring(episurvnum,4,9))) %>% arrange(EpiOrder)

massey_hn %>% mutate(HospInSample=hospitalnu %in% sample_hn$Hospital.No)



read_edr <- function(date) {
  db <- read.csv(sprintf("EntericDiseaseReport_%s.csv", date), header=F, stringsAsFactors=F)
  # figure out how many times to skip
  for (i in 1:ncol(db)) {
    skip = which(db[,i] == "EpiSurvNumber")
    if (length(skip) == 1) {
      db <- read.csv(sprintf("EntericDiseaseReport_%s.csv", date), header=T, stringsAsFactors=F, colClasses="character", skip=skip-1)
      attr(db, "db_name") <- date
      return(db)
    }
  }
  return(NA)
}

read_edr2 <- function(file) {
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

read_massey <- function(file) {
  
}

# read in master sheet
master <- read.csv("master_20150115.csv", stringsAsFactors=F, colClasses="character")

# remove 'tfhj' row, and any missing both EpiSurvNumber and HospitalNo
master <- master %>% filter(EpiSurvNumber != "tfhj")
master <- master %>% filter(!(EpiSurvNumber == "" & Hospital.Numbers == ""))


# read in the EntericDiseaseReport files and merge them together
edr_files <- list.files(pattern="EntericDiseaseReport.*.csv")

edr <- read_edr2(edr_files[1])
for (i in edr_files[-1]) {
  next_edr <- read_edr2(i)
  if (any(names(next_edr) != names(edr))) {
    stop("columns don't match in EDR files - need to use intersection of available columns or some such")
  }
  edr <- merge(edr, next_edr, all.x=T, all.y=T)
}

# read in the Massey question files and merge them together
massey_files <- list.files(pattern="Massey.*.csv")

massey <- read.csv(massey_files[1], header=T, stringsAsFactors=F, colClasses="character")
for (i in massey_files[-1]) {
  next_massey <- read.csv(i, header=T, stringsAsFactors=F, colClasses="character")
  if (any(names(next_massey) != names(massey))) {
    stop("columns don't match in Massey question files - need to use intersection of available columns or some such")
  }
  massey <- merge(massey, next_massey, all.x=T, all.y=T)
}

# remap column names
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

# compute Age brackets (TODO: ideally this would be done in the database)
age_bracket <- function(age) {
  grp <- (as.numeric(age) %/% 5) * 5
  paste0(grp, " to ", grp+4)
}

edr <- edr %>% mutate(Age5yrbrac = age_bracket(AgeInYears))

# converts dates using lubridate
convert_dates <- function(db, date_col) {
  for (d in intersect(date_col, names(db))) {
    before <- db[,d]
    after <- dmy(substring(db[,d],1,10), quiet=T)
    # TODO: summarise the warnings from the date conversion
    wrong <- is.na(after) & before != "" & !is.na(before)
    if (any(wrong)) {
      cat("Date conversion failed for: ")
      cat(before[wrong], "\n")
    }
    db[,d] <- after
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

setdiff(cols_master_not_massey, names(edr))
setdiff(names(edr), cols_master_not_massey)

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

nomatch <- massey %>% inner_join(master, by="EpiSurvNumber") %>% filter(Hospital.Numbers.x != Hospital.Numbers.y)

nomatch <- nomatch %>% select(EpiSurvNumber, Hospital.Numbers.x, Hospital.Numbers.y)

nomatch <- joint %>% filter(Hospital.Numbers.x != Hospital.Numbers.y)

nomatch %>% select(EpiSurvNumber, Hospital.Numbers.x, Hospital.Numbers.y)




master %>% filter(Hospital.Numbers %in% nomatch$Hospital.Numbers.y)



rows <- nrow(massey_en %>% anti_join(master_en))
if (rows > 0) {
  cat("WARNING:", rows, "rows in Massey questions", attr(edr, "db_name"), "that aren't in master sheet\n")
}



# Now, do the merging
# for now we assume the episurv info is GOLD STANDARD and copy across all of that (even if it overrides what
# is already in place


compare <- function(v1,v2) {
    # This function returns TRUE wherever elements are the same, including NA's,
    # and false everywhere else.
    same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}

# merges the 'new' database into the existing database 'db', using 'id_field' for linkage
merge_db <- function(db, new, id_field) {
  cols_to_merge <- intersect(names(new), names(db))
  rows_to_merge <- intersect(new[,id_field], db[,id_field])
  rows_to_add   <- setdiff(new[,id_field], db[,id_field])

  cat("Merging", length(rows_to_merge), "rows across", length(cols_to_merge), "columns matching on the", id_field, "field\n")
  cat("and adding", length(rows_to_add), "new rows\n")

  col_new <- match(cols_to_merge, names(new))
  col_db  <- match(cols_to_merge, names(db))

  row_new <- match(rows_to_merge, new[,id_field])
  row_db  <- match(rows_to_merge, db[,id_field])

  ds_db  <- db[row_db, col_db]
  ds_new <- new[row_new, col_new]

  # TODO: Produce a report showing these differences.
  #       A latex table with separate rows might do the trick if kept small enough?
  diff_rows <- NULL
  for (i in 1:nrow(ds_db))
  {
    diff = !compare(ds_db[i,], ds_new[i,])
    if (any(diff)) {
      diff_rows <- rbind(diff_rows, c(i, sum(diff)))
      cat("row", i, "is different\n")
      print(rbind(ds_db[i,which(diff)], ds_new[i,which(diff)]))
#      readline()
    }
  }
  db[row_db, col_db] <- new[row_new, col_new]

  if (length(rows_to_add) > 0) {
    row_new <- match(rows_to_add, new[,id_field])
    row_db  <- nrow(db) + 1:length(row_new)
    db[row_db, col_db] <- new[row_new, col_new]
  }
  return(db)
}

db <- master
new <- edr
id_field <- "EpiSurvNumber"

master <- merge_db(master, edr, "EpiSurvNumber")
master <- merge_db(master, massey, "EpiSurvNumber")
master <- merge_db(master, massey, "Hospital.Numbers")

# 4. add new rows not previously found

# 1. find the columns we wish to replace into
cols_from_massey <- intersect(names(massey), names(master))

col_massey <- match(cols_from_massey, names(massey))
col_master <- match(cols_from_massey, names(master))

# 2. find the rows we wish to replace into (including new rows?)
rows_from_massey <- intersect(massey$EpiSurvNumber, master$EpiSurvNumber)
row_massey <- match(rows_from_massey, massey$EpiSurvNumber)
row_master <- match(rows_from_massey, master$EpiSurvNumber)

# 3. do the replacement
ds1 <- master[row_master, col_master]
ds2 <- edr[row_edr, col_edr]

compare <- function(v1,v2) {
    # This function returns TRUE wherever elements are the same, including NA's,
    # and false everywhere else.
    same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}

# TODO: Produce a report showing these differences.
#       A latex table with separate rows might do the trick if kept small enough?
diff_rows <- NULL
for (i in 1:nrow(ds1))
{
  diff = !compare(ds1[i,], ds2[i,])
  if (any(diff)) {
    diff_rows <- rbind(diff_rows, c(i, sum(diff)))
    cat("row", i, "is different\n")
    print(rbind(ds1[i,which(diff)], ds2[i,which(diff)]))
    readline()
  }
}
master[row_master, col_master] <- edr[row_edr, col_edr]

# 4. add new rows not previously found
new_episurv <- setdiff(edr$EpiSurvNumber, master$EpiSurvNumber)
if (length(new_episurv) > 0) {
  cat("Adding", length(new_episurv), "rows of information from Episurv Disease Report to master sheet\n")
  row_edr <- match(new_episurv, edr$EpiSurvNumber)
  row_master <- nrow(master) + 1:length(row_edr)
  master[row_master, col_master] <- edr[row_edr, col_edr]
}








anti_join(edr %>% select(EpiSurvNumber), master %>% select(EpiSurvNumber))

test <- merge(master, edr, all=T, by="EpiSurvNumber")










# check each has the same columns
if (any(names(db1) != names(db2))) {
  stop("Database column names disagree. TODO: Allow reordering them\n")
}

# find a unique column to order by to ensure things are comparable
unique_columns <- character(0)
for (i in names(db1)) {
  if (!anyDuplicated(db1[,i]) && !anyDuplicated(db2[,i])) {
    cat("Column", i, "appears unique\n")
    unique_columns <- c(unique_columns, i)
  }
}

if (length(unique_columns) == 0)
  stop("No unique column available to compare databases on :(")

unique_column <- unique_columns[1]
cat("Using the", unique_column, "column to compare databases\n")

# find the common rows of the two databases
common <- intersect(db1[,unique_column], db2[,unique_column])

if (nrow(db1) != nrow(db2)) {
  cat("WARNING: Databases have differing number of rows. Using", length(common), "entries to compare\n")
}
# reorder the databases to match these
ind1 <- match(common, db1[,unique_column])
ind2 <- match(common, db2[,unique_column])

db1 <- db1[ind1,]
db2 <- db2[ind2,]

# compare columns across the data sets.  Trick is that read.csv() reads
# blanks as NA, so replace the NA first

db1[is.na(db1)] <- ""
db2[is.na(db2)] <- ""

for (i in names(db1)) {
  num_diff = sum(db1[,i] != db2[,i])
  if (num_diff != 0) {
    cat("column", i, "has", num_diff, "differences:\n")
  }
}

for (i in names(db1)) {
  num_diff = sum(db1[,i] != db2[,i])
  if (num_diff != 0) {
    cat("column", i, "has", num_diff, "differences:\n")
    diff <- db1[,i] != db2[,i]
    m <- cbind(db1[diff,i], db2[diff,i])
    rownames(m) <- which(diff)
    colnames(m) <- c(attr(db1,"db_name"), attr(db2,"db_name"))
    print(m)
    readline()
#    write.csv(m, sprintf("diff_%s.csv", i))
  }



















library(reshape2)

dates <- melt(master[,c("EpiSurvNumber", date_col)], id.vars="EpiSurvNumber")

unique_dates <- unique(dates$value)

parsed_dates <- dmy(unique_dates)

u <- unique_dates[is.na(parsed_dates)]
u <- u[u != ""]
u <- u[u != "tfhj"]

for (j in date_col) {
  found <- match(master[,j], u)
  if (sum(!is.na(found)) > 0) {
    cat(j,":", sum(!is.na(found)),"are ")
    cat(u[found[!is.na(found)]],"\n")
  }
}



OnsetDt
DateConsumed1

edr %>% mutate(

}

