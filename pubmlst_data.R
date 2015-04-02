# This script is to use PubMLST data to match MLST sequence type numbers to
# putative species.

# we simply download the PubMLST profile list, and match this against the PubMLST
# isolate list and count the species designations.

source("helpers.R")

library(dplyr)
library(reshape2)
library(pubmlst)

# derive MLST allelic profiles from pubmlst information
get_allelic_profiles <- function(pubmlst_sts_url, pubmlst_isolates_path) {

  # download pubmlst data
  mlst <- read.table(pubmlst_sts_url, header=T, sep="\t")

  # read in previously downloaded pubmlst isolate data
  isolates_file <- find_latest_version(pubmlst_isolates_path)
  isolates <- read.table(isolates_file, header=T, sep="\t", comment.char="")

  cols_iso <- c("aspA", "glnA", "gltA", "glyA", "pgm", "tkt", "uncA")

  for (i in cols_iso)
    isolates[,i] <- as_numeric(as.character(isolates[,i]))

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
  mlst %>% select(ST, ASP=aspA, GLN=glnA, GLT=gltA, GLY=glyA, PGM=pgm, TKT=tkt, UNC=uncA, CC, coli = prob_coli)
}

get_sequence_type <- function(mlst, pubmlst) {

  cols_mlst  <- c("ASP", "GLN", "GLT", "GLY", "PGM", "TKT", "UNC")

  # speed up by processing the unique allele combinations
  seqs <- factor(apply(mlst, 1, paste, collapse="_"))
  levs <- levels(seqs)

  sequences <- t(sapply(levels(seqs), function(x) { as_numeric(unlist(strsplit(x, split="_"))) }))
  colnames(sequences) <- cols_mlst

  # output
  output  <- pubmlst[0,]
  imputed <- rep(0, nrow(sequences)) # easier to store separately

  # storage for new MLST sequences we find
  newmlst <- pubmlst[0,]

  for (i in 1:nrow(sequences)) {
    st <- sequences[i,cols_mlst]

    # find hits in pubmlst
    possibles <- find_matching_profiles(st, pubmlst)

    if (sum(possibles) == 0 && !any(is.na(st))) {
      # we may have found this new profile before
      possibles <- find_matching_profiles(st, newmlst)
      if (sum(possibles) == 0) {
        # new ST
        newtype <- 10000 + nrow(newmlst)
        newmlst[nrow(newmlst)+1,] <- c(newtype, st, NA, "")
        output[i,] <- c(newtype, st, NA, "")
      } else if (sum(possibles) == 1) {
        output[i,] <- newmlst[possibles,]
      } else {
        stop("Bug - can't possibly have duplicate new_mlst matches")
      }
    } else if (sum(possibles) == 1) {
      # found - fill in the gaps from PubMLST
      output[i,] <- pubmlst[possibles,]
      imputed[i] <- sum(is.na(st))
    } else {
      # multiple hits -> leave as unimputed
      output[i,cols_mlst] <- st
    }
    if (i %% 100 == 0)
      cat("done", i, "of", nrow(sequences), "STs\n")
  }
  output$Imputed <- imputed

  result <- output[as_numeric(seqs),]
  rownames(result) <- 1:nrow(result)

  result
}

fill_mlst_from_pubmlst <- function(db, pubmlst_isolates_path="../pubmlst_isolates") {

  cols_mlst  <- c("ASP", "GLN", "GLT", "GLY", "PGM", "TKT", "UNC")

  #   a. convert "NEW" to a special value

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
    db[,i] <- as_numeric(db[,i])

  #   c. ST from PubMLST, optionally imputing missing alleles
  pubmlst <- get_allelic_profiles(pubmlst_sts_url="http://pubmlst.org/data/profiles/campylobacter.txt",
                                  pubmlst_isolates_path=pubmlst_isolates_path)

  results <- get_sequence_type(mlst=db[,cols_mlst], pubmlst=pubmlst)

  # store results
  db[,cols_mlst] <- results[,cols_mlst]
  db$ST <- results$ST
  db$CC <- results$CC
  db$Coli <- results$coli
  db$Imputed <- results$Imputed
  db
}

