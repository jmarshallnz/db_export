# 2014 analysis of rawmilk status by age etc.

# start by extracting the necessary data (only human isolates)

source("helpers.R")
source("pubmlst_data.R")
source("read_data.R")

library(dplyr)
library(lubridate)

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

# grab human isolates that aren't outside the Manawatu, and are primary cases (we don't care about
# multiple STs from the same isolate for the epi analysis).
db = db %>% filter(SA_model_source == "Human" & Manawatu != "No" & Primary.sample != "No")

# fixup date information.  Use Sampled.Date as that doesn't depend on episurv information,
# thus allowing unlinked samples to be included in the attribution
db <- db %>% mutate(Sampled.Date = as.Date(Sampled.Date, "%d/%m/%Y"))
db <- db %>% mutate(Year = as.POSIXlt(Sampled.Date)[["year"]] + 1900)
db <- db %>% mutate(Month = month(Sampled.Date, label=TRUE, abbr=TRUE))
db <- db %>% mutate(Intervention = ifelse(Year <= 2007, "before", "after"))

# tidy up information on epi covariates
db <- db %>% mutate(Sex = ifelse(Sex == "Unknown" | Sex == "", NA, Sex))
db <- db %>% mutate(Farm = ifelse(Farm == "Unknown", NA, Farm))
db <- db %>% mutate(Farm = ifelse(Farm == "Unknown", NA, Farm))

# raw milk
rawmilk_tab <- read.table(header=T, sep=",", row.names=1, stringsAsFactors=F, strip.white=T, text = "
              ,     Value 
            '',      NA 
            FALSE,   No 
            na,      NA
            nc,      NA
            no,      No
            No,      No
            TRUE,   Yes
            uk,      NA
            Unknown, NA
            yes,    Yes
            Yes,    Yes"
)

db <- db %>% mutate(Rawmilk = rawmilk_tab[UnpasturisedMilk,])

# and ethnicity
db <- db %>% mutate(Ethnicity = ifelse(grepl("Maori", EthnicityGrouping), "Maori",
                                ifelse(grepl("Asian", EthnicityGrouping), "Asian",
                                ifelse(grepl("Pacific", EthnicityGrouping), "Pacific",
                                ifelse(grepl("European", EthnicityGrouping), "European", NA)))))

# add in the urban/rural status
db <- db %>% left_join(read_urban_rural("../concordance-2006.csv"), by="Meshblock06")

# eliminate those where we have no link to urban/rural status (i.e. episurv)
# or where we don't have date information

start_year   <- 2005
current_year <- 2014

db <- db %>% filter(!is.na(UR_bool) & !is.na(Sampled.Date) & Year >= start_year & Year <= current_year)

# Eliminate columns we don't want, and unique-ise
sub <- db %>% select(Lab.ID, UR_num, UR_bool, Sampled.Date, Year, Month, Intervention, Sex, Age5yrbrac, Farm, Outbrk, Rawmilk, Ethnicity)

sub <- sub %>% unique

# write to .csv file (problem: month is then ill-defined)
write.csv(sub, "../reports/epidemiology_data/20150330.csv", row.names=F)

