# merge databases

library(dplyr)

compare <- function(v1,v2) {
    # This function returns TRUE wherever elements are the same, including NA's,
    # and false everywhere else.
    same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}

# merges the 'new' database into the existing database 'db', using 'id_field' for linkage
merge_db <- function(db, new, id_field, add_new = TRUE) {
  cols_to_merge <- intersect(names(new), names(db))
  rows_to_merge <- intersect(new[,id_field], db[,id_field])
  rows_to_add   <- setdiff(new[,id_field], db[,id_field])

  cat("Merging", length(rows_to_merge), "rows across", length(cols_to_merge), "columns matching on the", id_field, "field\n")
  if (add_new) {
    cat("and adding", length(rows_to_add), "new rows\n")
  } else {
    cat("WARNING: Not adding", length(rows_to_add), "new rows\n")
  }

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

  if (add_new && length(rows_to_add) > 0) {
    row_new <- match(rows_to_add, new[,id_field])
    row_db  <- nrow(db) + 1:length(row_new)
    db[row_db, col_db] <- new[row_new, col_new]
  }
  return(db)
}

