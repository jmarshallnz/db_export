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

db <- read.csv("../Export_Bionumerics_20150317.csv", stringsAsFactors=F)

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

results <- get_sequence_type(mlst=db[,cols_mlst], pubmlst=pubmlst, impute_alleles=T)

compST <- data.frame(db = db$ST, pubmlst = results$ST, stringsAsFactors=F)

compST %>% filter(db != "" & is.na(pubmlst))
compST %>% filter(db == "" & !is.na(pubmlst))
compST %>% filter(db != pubmlst & !is.na(pubmlst))

db[,cols_mlst] <- results[,cols_mlst]
db$ST <- results$ST
db$CC <- results$CC
db$Coli <- results$coli
db$Imputed <- results$imputed

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

# fixup date information
db <- db %>% mutate(ReportDate = as.Date(ReportDate))

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
sub <- db %>% select(ST, ASP, GLN, GLT, GLY, PGM, TKT, UNC, Source=SA_model_source, Imputed, UR_num, UR_bool, ReportDate, Quarter, Intervention)

# write to .csv file
write.csv(sub, "output.csv", row.names=F)

# TODO: Write R Markdown document to encapsulate this information.

# source groups
#sources <- list(c("Supplier A", "Supplier B", "Supplier_other"), "Duck_poultry", "Turkey", "Spent_hen", "Cattle", "Sheep", "Cat_dog_pet", "Water_bird_wild", "Wild_bird_other", "Environmental water")

# replace the source column
#source <- rep(0, nrow(d2013_no_dupes))
#for (i in 1:length(sources))
#{
#  for (j in 1:length(sources[[i]]))
#    source[d2013_no_dupes[,col_names_source] == sources[[i]][j]] <- i
#}
#d2013_no_dupes[,col_names_source] <- source

# write out for model running...
#animals <- subset(d2013_no_dupes, source>0)[,c(col_names_sequence_type, col_names_alleles, col_names_source)]
#names(animals) <- c("ST", col_names_alleles, "Source")
#humans  <- subset(d2013_no_dupes, source==0)[,c(col_names_sequence_type, col_names_alleles, col_names_month)]
#names(humans) <- c("ST", col_names_alleles, "YM")

#write.table(animals, "animals_2013_9.txt", quote=F, sep="\t", row.names=F)
#write.table(humans, "humans_2005_only.txt", quote=F, sep="\t", row.names=F)

