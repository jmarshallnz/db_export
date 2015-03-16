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
  mlst %>% select(ST, ASP=aspA, GLN=glnA, GLT=gltA, GLY=glyA, PGM=pgm, TKT=tkt, UNC=uncA, CC, coli = prob_coli)
}

get_sequence_type <- function(mlst, pubmlst, impute_alleles = F) {

  cols_mlst  <- c("ASP", "GLN", "GLT", "GLY", "PGM", "TKT", "UNC")

  # speed up by processing the unique allele combinations
  seqs <- factor(apply(mlst, 1, paste, collapse="_"))
  levs <- levels(seqs)

  sequences <- data.frame(t(sapply(levels(seqs), function(x) { suppressWarnings(as.numeric(unlist(strsplit(x, split="_")))) })))
  names(sequences) <- cols_mlst
  sequences$ST <- ""
  sequences$CC <- ""
  sequences$coli <- ""
  imputed <- rep(0, nrow(sequences)) # don't store in sequences, as it makes assignment from pubmlst trickier

  new_sts <- 0
  for (i in 1:nrow(sequences)) {
    st <- sequences[i,cols_mlst]

    # find hits in pubmlst
    possibles <- rep(T, nrow(pubmlst))
    na_count <- 0
    for (j in 1:length(st)) {
      if (!is.na(st[,j])) {
        possibles <- possibles & pubmlst[,cols_mlst[j]] == st[,j]
      } else {
        na_count <- na_count + 1;
      }
    }
    if (sum(possibles) == 0 && na_count == 0) {
      # new ST?
      new_sts <- new_sts+1
      pubmlst[nrow(pubmlst)+1,c("ST", cols_mlst, "CC", "coli")] <- c(10000 + new_sts, st, "", "")
      sequences[i,"ST"] <- 10000 + new_sts
    } else if (sum(possibles) == 0) {
      # partial with no match in pubmlst -> leave blank
    }
    else if (sum(possibles) == 1) {
      # found - fill in the gaps from PubMLST
      sequences[i,] <- pubmlst[possibles,names(sequences)]
      imputed[i] <- na_count
    }
    if (i %% 100 == 0)
      cat("done", i, "of", nrow(sequences), "STs\n")
  }
  sequences$imputed <- imputed

  result <- sequences[as.numeric(seqs),]
  rownames(result) <- 1:nrow(result)

  result
}
