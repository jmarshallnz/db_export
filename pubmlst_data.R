# This script is to use PubMLST data to match MLST sequence type numbers to
# putative species.

# we simply download the PubMLST profile list, and match this against the PubMLST
# isolate list and count the species designations.

source("extractors.R")

library(dplyr)
library(reshape2)

# derive MLST allelic profiles from pubmlst information
get_allelic_profiles <- function(pubmlst_sts_url, pubmlst_isolates_path) {

  # download pubmlst data
  mlst <- read.table(pubmlst_sts_url, header=T, sep="\t")

  # read in previously downloaded pubmlst isolate data
  isolates <- read.table(pubmlst_isolates_path, header=T, sep="\t", comment.char="")
  cols_iso <- c("aspA", "glnA", "gltA", "glyA", "pgm", "tkt", "uncA")

  for (i in cols_iso)
    isolates[,i] <- as.numeric(as.character(isolates[,i]))

  # cleanup the species name column
  levels(isolates$species) <- gsub("Campylobacter (.*)", "\\1", levels(isolates$species))
  levels(isolates$species) <- gsub(" ", ".", levels(isolates$species))

  # join to the isolates table
  joined <- mlst %>% left_join(isolates)

  # and aggregate up by reshaping
  types <- joined %>% mutate(value=1) %>% dcast(ST ~ species, sum)

  # drop the useless "NA" type
  types <- types %>% mutate("NA"=NULL)

  # and join back to the original database
  mlst <- mlst %>% inner_join(types)
  mlst <- mlst %>% mutate(prob_coli = ifelse(coli > jejuni, "coli", ""))

  # cleanup the clonal complex
  mlst <- mlst %>% mutate(CC = mygrep("^ST-([0-9]+) complex", "\\1", clonal_complex))

  # finally, dump only the columns we're after
  mlst %>% select(ST, ASP=aspA, GLN=glnA, GLT=gltA, GLY=gltA, PGM=pgm, TKT=tkt, UNC=uncA, CC, coli = prob_coli)
}

