# This script is to perform the join of episurv with sample and isolate information from the
# database

source("read_data.R")
source("extractors.R")
source("validators.R")
source("merge_db.R")

library(dplyr)

fill_manawatu <- function(manawatu, hospital_number) {

  # outside hospital numbers take the form [#]#(A-Z)...
  outside_manawatu <- grepl("^[0-9][0-9]*[a-zA-Z]", hospital_number)
  cat("Outside hospital numbers:\n")
  print(hospital_number[outside_manawatu])

  ifelse(outside_manawatu, "No", manawatu)
}

join_data <- function(episurv_path, sample_path, isolate_path, cols_to_extract = NULL) {

  # read data in
  episurv <- read.csv(episurv_path, stringsAsFactors=F, colClasses="character")
  sample  <- read.csv(sample_path, stringsAsFactors=F, colClasses="character")
  isolate <- read_isolate(isolate_path, TRUE)

  # we first need to make sure that the Hospital.No field of the episurv master sheet is full where needed.
  # in the case of no Massey Questions data for a row, we'll have this missing, but it may be in the comments field

  # TODO: Ideally this would be done in merge_episurv.R
  hosp_num_from_comment <- extract_hospital_from_comment(episurv$Comments)
  episurv <- episurv %>% mutate(Hospital.Numbers = ifelse(Hospital.Numbers == "" & hosp_num_from_comment != "", hosp_num_from_comment, Hospital.Numbers))
  episurv <- episurv %>% mutate(ID = rownames(episurv))

  # the join USED to be done based on the "New.LinkNo" column, which was filled with a bunch of manual queries in
  # the access database. Then the "Hospital.No" column was cleared.  Said queries just joined on "Hospital.No", however.

  # thus, we restore the Hospital.No column first, then use that for the join.

  # find the rows where we have New.LinkNo specified, but don't have a hospital number
  sample_unlinked <- sample %>% filter(New.LinkNo == "")
  sample_linked   <- sample %>% filter(New.LinkNo != "")

  # link to episurv table and add hospital number
  sample_linked <- sample_linked %>% left_join(episurv %>% select(Link.Number, Hospital.Numbers), by=c("New.LinkNo" = "Link.Number"))

  # replace the Hospital.No column with hospital numbers from episurv table, so we can use that to link on.
  sample_linked <- sample_linked %>% mutate(Hospital.No = ifelse(Hospital.Numbers != "" & !is.na(Hospital.Numbers), Hospital.Numbers, Hospital.No), Hospital.Numbers = NULL)

  # and now NA out the ones we don't have matches for
  sample_linked$Hospital.No[sample_linked$Hospital.No == ""] <- NA
  sample_unlinked$Hospital.No[sample_unlinked$Hospital.No == ""] <- NA

  # NOTE: At least one of the samples is linked to the 'wrong' row in episurv here, as for one of them we have duplicate Hospital.Numbers.  This isn't really a problem, as it
  #       appears to be the same person in both cases.
  hosp_num <- sample_linked %>% left_join(episurv %>% select(Hospital.Numbers, Link.Number), by=c("Hospital.No" = "Hospital.Numbers")) %>% select(Hospital.No, LinkNo, Link.Number, New.LinkNo)
  hosp_num <- hosp_num %>% filter(Link.Number != New.LinkNo)
  if (nrow(hosp_num)) {
    cat("Some hospital numbers are linking to differing rows than expected (due to duplicates in episurv table)\n")
    print(hosp_num)
  }

  # rbind the databases back together
  sample <- rbind(sample_linked, sample_unlinked)

  # Now that we have the hospital number, we can fill in some missing Manawatu
  humans <- sample %>% filter(SA_model_source == "Human")
  humans <- humans %>% mutate(Manawatu = fill_manawatu(Manawatu, Hospital.No))
  sample <- rbind(sample %>% filter(SA_model_source != "Human"), humans)

  # OK, now join up the tables, adding extra stuff where needed
  isolate <- isolate %>% mutate(Isolate.Level.Lab.ID = Lab.ID)
  episurv <- episurv %>% mutate(EpiHospNumbers = Hospital.Numbers)

  final <- sample %>% left_join(isolate, by = c("Lab.ID" = "Lab.ID")) %>% left_join(episurv, by = c("Hospital.No" = "Hospital.Numbers"))

  # add and rename columns as needed for consistency with previous Access database
  final <- final %>% mutate(Expr1000 = Lab.No) %>% rename(Sample.level_Comments = Comments.x,
                                                        Isolate.level_Comments = Comments.y,
                                                        Episurv_Comments = Comments,
                                                        Hospital.Numbers = EpiHospNumbers)

  # extract only the columns asked for
  if (is.null(cols_to_extract)) {
    # default cols to drop
    cols_to_drop <- c("New.LinkNo", "OtherPos_1", "OtherPos_2", "OtherPos_3",
                      "Isolate.Location", "EpiSurve.Case.No", "Age", "Gender",
                      "Urban.Rural", "In.Outpatient", "Other.Pos", "DateCreated.x",
                      "SampleStatus", "SampleStatusDescription", "ManualOverride",
                      "EpiSurvNumber", "Finished.QC", "Partial.QC", "Date.PCR",
                      "Date.Gel", "Single.Genus.PCR", "Single.Species.PCR", "CAT",
                      "Boltons.mCCDA", "mCCDA", "Sma1", "Kpn.1",
                      "TKT.SNP", "PFGE.Date", "Peer.Checked", "MLST.Date",
                      "ASP..lari.", "ASP..fetus.", "ASP..coli.",
                      "GLN..lari.", "GLN..uknown.", "GLN..upsaliensis.", "GLN..coli.",
                      "GLT..lari.", "GLT..unknown.", "GLT..upsaliensis.", "GLT..coli.",
                      "GLY..lari.", "GLY..unknown.", "GLY..upsaliensis", "GLY..coli.",
                      "PGM..lari.", "PGM..unknown.", "PGM..coli.",
                      "TKT..lari.", "TKT..unknown.", "TKT..upsaliensis", "TKT..coli.",
                      "atpA..UNC..lari.", "UNC..unknown.", "UNC..upsaliensis.", "UNC..coli.", "UNC..fetus.",
                      "Other.than.jejuni.ST", "Other.than.jejuni.CC", "DateCreated.y")
    cols_to_extract <- setdiff(names(final), cols_to_drop)
  }

  final[, cols_to_extract]
}

final <- join_data("../EpisurvData/master_20150317.csv", "../Sample_20150128.csv", "../Isolate_20150128.csv")

# and write out to csv
write.csv(final, "Export_Bionumerics_20150317.csv", row.names=F, na="")


