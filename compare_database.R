# This script is to to compare two databases, ignoring one or more columns

library(dplyr)

read_db <- function(date) {
  db <- read.csv(file.path("..", sprintf("Sample_%s.csv", date)), stringsAsFactors=F)
  attr(db, "db_name") <- date
  db
}

db2 <- read_db("20150128")
db1 <- read_db("20150142")

#db1 <- read.csv("../Sample_20150116.csv", stringsAsFactors=F)
#db2 <- read.csv("../Sample_20150114.csv", stringsAsFactors=F)
#db1 <- read.csv("../Sample_20141207.csv", stringsAsFactors=F)
#db1 <- read.csv("../Sample_20140818.csv", stringsAsFactors=F)

# trim off whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
for (i in 1:ncol(db1))
  db1[,i] <- trim(db1[,i])
for (i in 1:ncol(db2))
  db2[,i] <- trim(db2[,i])

# order the columns
db1 <- db1[,sort(names(db1))]
db2 <- db2[,sort(names(db2))]

# check each has the same columns
if (any(names(db1) != names(db2))) {
  stop("Database column names disagree.")
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
}

