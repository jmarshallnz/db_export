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

source("helpers.R")
source("pubmlst_data.R")

library(dplyr)

db_file <- find_latest_version("../final_data/")
db <- read.csv(db_file, stringsAsFactors=F)

# infer MLST from pubMLST
db <- db %>% fill_mlst_from_pubmlst(pubmlst_isolates_path="../pubmlst_isolates")

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

#   e. Sources we wish to eliminate (TODO: This filtering should be done elsewhere)

db = db %>% filter(SA_model_source != "Pig")

#   f. c.coli isolates

db = db %>% filter(Coli != "Coli")

#   g. Incomplete SA profiles

# first remove those with more than X news
new_allele_max <- 3

cols_mlst  <- c("ASP", "GLN", "GLT", "GLY", "PGM", "TKT", "UNC")
incomplete <- which(rowSums(db[,cols_mlst] > 900) > new_allele_max)
db = db[-incomplete,]

db = db %>% filter(!is.na(ASP) &
                   !is.na(GLN) &
                   !is.na(GLT) &
                   !is.na(GLY) &
                   !is.na(PGM) &
                   !is.na(TKT) &
                   !is.na(UNC) & 
                   !is.na(ST))

#   h. Duplicate isolates from the same sample with the same ST

unique_rows <- as.numeric(rownames(unique(db %>% select(Lab.ID, ST))))
db <- db[unique_rows,]

# fixup date information.  Use Sampled.Date as that doesn't depend on episurv information,
# thus allowing unlinked samples to be included in the attribution
db <- db %>% mutate(Sampled.Date = as.Date(Sampled.Date, "%d/%m/%Y"))

# eliminate those humans without a date
db <- db %>% filter(!is.na(Sampled.Date) | Source.Type != "Human")

# add a quarter and intervention column
quarters_since_2005 <- function(date) {
  dt <- as.POSIXlt(date)
  floor(dt$mon/3) + (dt$year - 105)*4 + 1
}

db <- db %>% mutate(Quarter = quarters_since_2005(Sampled.Date))
db <- db %>% mutate(Year = as.POSIXlt(Sampled.Date)[["year"]] + 1900)
db <- db %>% mutate(Intervention = ifelse(Quarter <= 12, "before", "after"))

# eliminate things outside our range
db <- db %>% filter(Quarter > 0 | is.na(Quarter))

# TODO: Get prevalence information directly from the number of isolates?

# add in the urban/rural status
db <- db %>% left_join(read_urban_rural("../concordance-2006.csv"), by="Meshblock06")

# Eliminate columns we don't want
sub <- db %>% select(ST, ASP, GLN, GLT, GLY, PGM, TKT, UNC, Source=SA_model_source, Imputed, UR_num, UR_bool, Sampled.Date, Quarter, Year, Intervention)

# write to .csv file
write.csv(sub, "output.csv", row.names=F)

# TODO: Write R Markdown document to encapsulate this information.

