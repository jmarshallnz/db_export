# This script is to convert an export from the campylobacter access database
# i.e. that produced from the Export for BIONUMERICS query and resulting in an
# excel spreadsheet
#
# We convert the spreadsheet into a usable form for source attribution,
# picking out just the rows we need (e.g. MLST profile(s), source, covariates)
# and tidying the data up as needed.
#
# The current procedure is as follows:
#
# 1. Specify which rows we want to eliminate.
#    a. Things without MLST information.
#    b. Stuff from inappropriate projects?
#    c. Non-primary human cases?
#
# 2. Specify the columns we want.
#
# 3. Join to any other databases (e.g. for urban/rural status from Meshblock06)
#
# 4. Validate the MLST data:
#    a. From the allelic profile, generate the ST column.
#    b. Check this against the ST column.
#    c. Fix accordingly.
#    d. optionally impute missing information, remove those with little.
#    e. remove duplicate sample information (same ST, same LabID)
#
# Goal from 1..4 is getting good information for ALL valid isolates.  We then
# subfilter for a particular purpose (e.g. grab only 2005 isolates) below here.
#
# 5. Format up for island model
#    a. Make all STs and NEWs numeric.
#    b. Replace source names with numbers.
#    c. Save as .TXT.

# Ideally we'd read the .xls sheet in directly.  Unfortunately, access produces .xls sheets
# that aren't nicely processable by any of the xls reading packages in R, as the Episurv Comment
# field contains newlines, quotes, double quotes, commas and tabs...
#
# Thus, we need to first load the sheet in Excel, and save it as a .csv.
#
# This isn't too much more work than running the Excel export in the first place, but is frustrating
# nonetheless.  For more info, the openxlsx reader doesn't handle t="inlineStr" style tags.

source("pubmlst_data.R")

library(dplyr)

db <- read.csv("../Export_Bionumerics_20150128.csv", stringsAsFactors=F)

# 1. Process MLST information.

cols_mlst  <- c("ASP", "GLN", "GLT", "GLY", "PGM", "TKT", "UNC")

#   a. convert "NEW" to a special value?

# TODO: What does NEW mean?  New at the time of sampling?  Why not in PubMLST?

# Talked with Anne. NEW is new at the time of sampling.  i.e. it could be known now
# and possibly is now in PubMLST.  Will see if I can get the sequence information that
# corresponds to these NEW isolates.

for (i in cols_mlst) {
  newbies <- db[,i] == "NEW"
  if (sum(newbies) > 0)
    db[newbies,i] <- 1000 - 1:sum(newbies)
}

#   a. convert MLST info to numeric

for (i in cols_mlst)
  db[,i] <- suppressWarnings(as.numeric(db[,i]))

#   b. eliminate rows without MLST information.

db = db %>% filter(!is.na(ASP) |
                   !is.na(GLN) |
                   !is.na(GLT) |
                   !is.na(GLY) |
                   !is.na(PGM) |
                   !is.na(TKT) |
                   !is.na(UNC))

#   c. ST from PubMLST, optionally imputing missing alleles

pubmlst <- get_allelic_profiles(pubmlst_sts_url="http://pubmlst.org/data/profiles/campylobacter.txt",
                                pubmlst_isolates_path="../pubmlst_isolates_20150219.txt")

results <- get_sequence_type(mlst=db[,cols_mlst], pubmlst=pubmlst, impute_alleles=0)

compST <- data.frame(db = db$ST, pubmlst = result$ST)

compST %>% filter(db != "" & is.na(pubmlst))
compST %>% filter(db == "" & !is.na(pubmlst))
compST %>% filter(db != pubmlst & !is.na(pubmlst))

db[,cols_mlst] <- results[,cols_mlst]
db$ST <- results$ST
db$CC <- results$CC
db$coli <- results$coli

# 2. Eliminate the rows we don't want.
#   a. rows in the wrong project

db = db %>% filter(Project != "Els Acke Pet Food" &
                   Project != "UKVTRI" &
                   Project != "Abattoir Cases" &
                   Project != "Barbara Binney" &
                   Project != "Caecal Project" &
                   Project != "Occupational" &
                   Project != "UKVTRI" &
                   Project != "Poultry Industry" &
                   Project != "Timaru" &
                   Project != "Tui" &
                   Project != "Steve Trewick" &
                   Project != "Random" &
                   Project != "TeckLok" &
                   Project != "Whitford Wild Bird Trust" &
                   Project != "total_genome_sequencing" &
                   Project != "")

#   b. rows without source information

# TODO: Should we rely on the SA_model_source column, or should this be filled in automatically based
#       on the Source.Type and Sample.Type columns?

db = db %>% filter(SA_model_source != "")

#   c. human isolates outside of the Manawatu

# TODO: Is the Manawatu column derivable from other columns?  Can we check this?

db = db %>% filter(Manawatu != "No" | SA_model_source != "Human")

#   d. human isolates that aren't primaries and don't have unique STs

# TODO: It's not clear what to do if Primary is not specified as Yes or No.  Some are blanks.
#       Even if it is No, we should still include if there is another isolate ID and the ST was unique?

non_primary = db %>% filter(Source.Type == "Human" & Primary.sample == "No")

# find corresponding primaries if they're a duplicate, and compare STs (Note: Need ST information here)
primary = db %>% filter(Source.Type == "Human" & Primary.sample != "No")

join_cols = as.vector("LabID")
names(join_cols)

include_np <- non_primary %>% inner_join(primary %>% select(-Duplicate.to), by=c("Duplicate.to" = "Lab.ID"))

include_np = include_np %>% filter(ASP.x != ASP.y |
                                   GLN.x != GLN.y |
                                   GLT.x != GLT.y |
                                   GLY.x != GLY.y |
                                   PGM.x != PGM.y |
                                   TKT.x != TKT.y |
                                   UNC.x != UNC.y)

# add these rows in
db = db %>% filter(Source.Type != "Human" | Primary.sample != "No" | Lab.ID %in% include_np$Lab.ID)

#   e. Sources we wish to eliminate

db = db %>% filter(SA_model_source != "Pig")

#   f. Incomplete SA profiles
db = db %>% filter(!is.na(ASP) &
                   !is.na(GLN) &
                   !is.na(GLT) &
                   !is.na(GLY) &
                   !is.na(PGM) &
                   !is.na(TKT) &
                   !is.na(UNC) & 
                   !is.na(ST))

# fixup date information
db <- db %>% mutate(ReportDate = as.Date(ReportDate, format="%d/%m/%Y"))

# eliminate those humans without a date
db <- db %>% filter(!is.na(ReportDate) | Source.Type != "Human")

# add a quarter and intervention column
quarters_since_2005 <- function(date) {
  dt <- as.POSIXlt(date)
  floor(dt$mon/3) + (dt$year - 105)*4 + 1
}

db <- db %>% mutate(Quarter = quarters_since_2005(ReportDate))
db <- db %>% mutate(Intervention = ifelse(Quarter <= 12, "before", "after"))

# eliminate things outside our range
db <- db %>% filter(Quarter > 0 | is.na(Quarter))

# TODO: Get prevalence information directly from the number of isolates?


# load concordance file to map urban/rural status
ur <- read.csv("../concordance-2006.csv", stringsAsFactors=F)
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

ur = ur %>% left_join(ur_tab, by="UR_cat")

table(ur$UR_cat)
table(ur$UR_num)
table(ur$UR_bool)

# join our database
db <- db %>% left_join(ur, by="Meshblock06")

# Eliminate columns we don't want
sub <- db %>% select(ST, ASP, GLN, GLT, GLY, PGM, TKT, UNC, SA_model_source, UR_num, UR_bool, ReportDate, Quarter, Intervention)

# write to .csv file
write.csv(sub, "output.csv", row.names=F)

# TODO: Write R Markdown document to encapsulate this information.


# The 2013 procedure was as follows:
#
# 1. Nigel dumped data from the Access database.
# 2. Nigel imputed the missing fields, and filled in ST and imputed column.
# 3. JM fixed various fields (incorrect STs, missing imputed column values, in RED in the excel sheet) and dumped to .csv.

# column names for data set
col_names_alleles        <- c("ASP", "GLN", "GLT", "GLY", "PGM", "TKT", "UNC")
col_names_clonal_complex <- c("CC")
col_names_sequence_type  <- c("ST")
col_names_source         <- c("SA_model_source")
col_names_month          <- c("Month")
col_names_sample_id      <- c("Lab.ID")

# known c.coli clonal complex to remove
remove_c_coli_cc <- c("828")

# date range
date_min <- "2005-07-01"
date_max <- "2006-06-31"

# source groups
#sources <- list("Supplier A", "Supplier B", "Supplier_other", "Duck_poultry", "Turkey", "Spent_hen", "Cattle", "Sheep", "Cat_dog_pet", "Water_bird_wild", "Wild_bird_other", "Environmental water")
#sources <- list(c("Supplier A", "Supplier B", "Supplier_other"), c("Cattle", "Sheep"), c("Duck_poultry", "Turkey", "Spent_hen", "Cat_dog_pet", "Water_bird_wild", "Wild_bird_other", "Environmental water"))
#sources <- list(c("Supplier A", "Supplier B", "Supplier_other"), c("Cattle", "Sheep"), "Environmental water")
#sources <- list(c("Supplier A", "Supplier B", "Supplier_other"), c("Cattle", "Sheep"), "Environmental water", c("Duck_poultry", "Turkey", "Spent_hen", "Cat_dog_pet", "Water_bird_wild", "Wild_bird_other"))

sources <- list(c("Supplier A", "Supplier B", "Supplier_other"), "Duck_poultry", "Turkey", "Spent_hen", "Cattle", "Sheep", "Cat_dog_pet", "Water_bird_wild", "Wild_bird_other", "Environmental water")

# human group
human_group <- "Human"

# read in 2013 data from Nigel.
d2013 <- read.csv("2013_all_dump2.csv")
#d2013 <- read.csv("2013_all_dump.csv")

# throw away anything that doesn't match our sources
source_match <- d2013[,col_names_source] == human_group
for (i in 1:length(sources))
{
  for (j in 1:length(sources[[i]]))
    source_match <- source_match | d2013[,col_names_source] == sources[[i]][j]
}
cat("Eliminating", sum(!source_match), "entries not matching our source list or humans\n")
d2013 <- d2013[source_match,]
d2013[,col_names_sample_id] <- factor(d2013[,col_names_sample_id])

# !!!YET ANOTHER FUCKUP!!!
d2013$ST <- as.character(d2013$ST)
d2013$ST[d2013$ST == "u53"][2] <- "u53b"
d2013$ST <- factor(d2013$ST)
# !!!YET ANOTHER FUCKUP!!!

# throw away all imputed
imputed <- d2013$imputed != 0 & !is.na(d2013$imputed)
d2013 <- d2013[!imputed,]

# check the dataset contains the columns we expect
sum(is.na(d2013[,col_names_sample_id]))  # great - none missing

# switch months to a Date object
d2013[, col_names_month] <- as.Date(d2013[,col_names_month], format="%d-%b-%y")

# identify the allele columns
allele_cols  <- sapply(col_names_alleles, function(x) { wch <- which(names(d2013)==x); if (length(wch) == 0) { NA } else { wch } })
if (any(is.na(allele_cols)))
  stop("Missing allele column(s) ", allele_names[is.na(allele_cols)], " in data set!")

# eliminate humans without date
humans_no_date <- is.na(d2013[, col_names_month]) & d2013[, col_names_source] == human_group
cat("Eliminating", sum(humans_no_date), "human entries with no date\n")
d2013 <- d2013[!humans_no_date,]

# replace the month column
min_year  <- as.numeric(substr(date_min, 1, 4))
min_month <- as.numeric(substr(date_min, 6, 7))
max_year  <- as.numeric(substr(date_max, 1, 4))
max_month <- as.numeric(substr(date_max, 6, 7))
ym_min <- min_year * 4 + (min_month-1) %/% 3
ym_max <- max_year * 4 + (max_month-1) %/% 3

year  <- as.numeric(substr(d2013[,col_names_month], 1, 4))
month <- as.numeric(substr(d2013[,col_names_month], 6, 7))
ym <- year * 4 + (month-1) %/% 3

out_of_range <- ym < ym_min | ym > ym_max
out_of_range <- out_of_range & !is.na(out_of_range)
d2013$Month <- ym - ym_min + 1

cat("Eliminating", sum(out_of_range), "entries that are out of the data range", date_min, "to", date_max, "\n")
d2013 <- d2013[!out_of_range,]

# throw away known c.coli
ccoli <- rep(FALSE, nrow(d2013))
for (cc in remove_c_coli_cc)
  ccoli <- ccoli | d2013[col_names_clonal_complex] == cc
cat("Eliminating", sum(ccoli), "entries matching c.coli clonal complexes\n")
d2013 <- d2013[!ccoli,]

# throw away 2013
#d2013 <- d2013[!(d2013$Month >= "2013-01-01" & !is.na(d2013$Month)),]

# throw away those where we have non-numeric entries (i.e. NEW) across more than 4...
s <- rep(0, nrow(d2013))
for (j in allele_cols)
  s <- s + is.na(as.numeric(as.character(d2013[,j])))

cat("Eliminating", sum(s >= 4), "entries due to 4 or more new alleles\n")
d2013 <- d2013[s < 4,]


STs <- unique(d2013$ST)
for (i in 1:length(STs))
{
  if (nrow(unique(d2013[d2013$ST == STs[i], col_names_alleles])) > 1)
  {
    cat("Error ST", STs[i], "is not unique\n")
    dupes <- d2013[d2013$ST == STs[i],]
    uniq <- dupes[!duplicated(dupes[,col_names_alleles]),]
    for (i in 1:nrow(uniq))
      cat(uniq[i,]$Lab.ID, uniq[i,]$ST, as.character(uniq[i,col_names_alleles]), uniq[i,]$imputed, "\n")
  }
}

# Remove duplicate STs from the same sample

uniqueSamples <- levels(d2013[,col_names_sample_id])
d2013_no_dupes <- d2013[0,]
for (i in 1:length(uniqueSamples))
{
  wch <- which(d2013[,col_names_sample_id] == uniqueSamples[i])
  # pull out the ST profile for these...
  if (length(wch) > 1) {
    u <- duplicated(d2013[wch,allele_cols])
    d2013_no_dupes <- rbind(d2013_no_dupes, d2013[wch[!u],])
  } else if (length(wch) == 1) {
    d2013_no_dupes <- rbind(d2013_no_dupes, d2013[wch,])
  }
}

cat("Eliminated", nrow(d2013) - nrow(d2013_no_dupes), "rows due to being duplicates\n")

# replace the u* with 99*
# replace ###[a-z] with ###[000]
STs <- levels(d2013_no_dupes[,col_names_sequence_type])
for (i in 1:length(STs))
{
  if (substr(STs[i], 1, 1) == "u")
  {
    newST <- paste("9", substr(STs[i],2,10), sep="")
    # check if this type is previously found...
    while (sum(STs == newST) > 0)
      newST <- paste("9", newST, sep="")
    cat("Replacing ST", STs[i], "with numeric value", newST, "\n")
    STs[i] <- newST;
  }
  if (is.na(as.numeric(substr(STs[i], nchar(STs[i]), nchar(STs[i])))))
  {
    newST <- paste(substr(STs[i],1,nchar(STs[i])-1), "0", sep="")
    # check if this type is previously found...
    while (sum(STs == newST) > 0)
      newST <- paste(newST, "0", sep="")
    cat("Replacing ST", STs[i], "with numeric value", newST, "\n")
    STs[i] <- newST;
  }
}
levels(d2013_no_dupes[,col_names_sequence_type]) <- STs
d2013_no_dupes[,col_names_sequence_type] <- as.numeric(as.character(d2013_no_dupes[,col_names_sequence_type]))

# replace non-numeric alleles with 999... making sure we don't create duplicate STs...
for (j in allele_cols)
{
  d2013_no_dupes[,j] <- as.numeric(as.character(d2013_no_dupes[,j]))
  non_num <- is.na(d2013_no_dupes[,j])
  nums <- as.numeric(as.factor(d2013_no_dupes[non_num,col_names_sequence_type]))
  d2013_no_dupes[non_num,j] <- 1000-nums
}

# replace the source column
source <- rep(0, nrow(d2013_no_dupes))
for (i in 1:length(sources))
{
  for (j in 1:length(sources[[i]]))
    source[d2013_no_dupes[,col_names_source] == sources[[i]][j]] <- i
}
d2013_no_dupes[,col_names_source] <- source

# write out for model running...
animals <- subset(d2013_no_dupes, source>0)[,c(col_names_sequence_type, col_names_alleles, col_names_source)]
names(animals) <- c("ST", col_names_alleles, "Source")
humans  <- subset(d2013_no_dupes, source==0)[,c(col_names_sequence_type, col_names_alleles, col_names_month)]
names(humans) <- c("ST", col_names_alleles, "YM")

write.table(animals, "animals_2013_9.txt", quote=F, sep="\t", row.names=F)
write.table(humans, "humans_2005_only.txt", quote=F, sep="\t", row.names=F)

