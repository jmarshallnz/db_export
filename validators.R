# functions for validation of fields

# validator for hospital numbers
validate_hospital <- function(hosp) {

  # hospital numbers take the form:
  # YY0#######, where YY may be a single year, or (for 4/5 are Y#######)

  bad_hosp <- !grepl("^1[0-9]0[0-9]{7}$", hosp) & !grepl("^[6-9]0[0-9]{7}$", hosp) & !grepl("^[4-5][0-1][0-9]{6}$", hosp)
  if (any(bad_hosp)) {
    cat("Invalid hospital numbers:\n")
    print(hosp[bad_hosp])
  }
  bad_hosp
}

# validator for episurv numbers
validate_episurv <- function(epi) {

  # episurv numbers take the form: YY-######-PN

  # or 200YMW#####

  bad <- !grepl("^[0-9]{2}-[0-9]{6}-PN$", epi) & !grepl("^200[4-7]MW[0-9]{5}$", epi)
  if (any(bad)) {
    cat("Invalid episurv numbers:\n")
    print(epi[bad])
  }
  bad
}

# validates hospital numbers against episurv numbers
validate_hosp_epi <- function(hosp, epi) {

  append <- function(v, a) {
    ifelse(nchar(v) > 0, paste0(v, ",", a), a)
  }

  stopifnot(length(epi) == length(hosp))

  n <- length(epi)

  bad <- validate_episurv(epi)
  bad_reason <- character(n)
  bad_reason[bad] <- "InvalidEpisurv"

  bad_hosp <- validate_hospital(hosp)
  bad <- bad | bad_hosp
  bad_reason[bad_hosp] <- append(bad_reason[bad_hosp], "InvalidHospital")

  # epi/hosp are the correct format, so split into dataframes of year/num
  dat  <- data.frame(year=rep(NA,n),epi=NA,hosp=NA)

  dat$year[!bad] <- as.numeric(gsub("^([1]*[0-9])(-[0-9]{6}-PN)$", "\\1", epi[!bad]))
  dat$epi[!bad]  <- as.numeric(gsub("^([1]*[0-9]-)([0-9]{6})(-PN)$", "\\2", epi[!bad]))
  dat$hosp[!bad] <- as.numeric(gsub("^([1]*[0-9]0)([0-9]{7})$", "\\2", hosp[!bad]))
  hosp_year       <- rep(NA,n)
  hosp_year[!bad] <- as.numeric(gsub("^([1]*[0-9])(0[0-9]{7})$", "\\1", hosp[!bad]))

  bad_match <- dat$year != hosp_year & !bad
  if (any(bad_match)) {
    cat("Invalid matching between episurv and hospital number years:\n")
    print(cbind(hosp[bad_match], epi[bad_match]))
  }
  bad <- bad | bad_match
  bad_reason[bad_match] <- append(bad_reason[bad_match], "YearMismatch")

  dat$year <- factor(dat$year)

  # IDEA: correlate the (converted) episurv number
  #       and work out where we have outliers
  par(mfrow=c(2,3))
  plot(epi ~ hosp, col=year, data=dat[!bad,])

  # fit a linear model
  mod.2 <- lm(epi ~ hosp * year, data=dat[!bad,])
  mod.1 <- lm(epi ~ hosp + year, data=dat[!bad,])
  mod.0 <- lm(epi ~ hosp, data=dat[!bad,])

  # find most suitable model
  # NOTE: episurv numbers past the year just keep incrementing, so assuming
  #       the rate at which disease increases is approximately constant, we'd
  #       expect a common-slope model to be most suitable
  mod <- mod.1
#  if (anova(mod.2, mod.1)[2,6] < 0.05) {
#    cat("Separate slopes for year\n")
#    mod <- mod.2
#  } else if (anova(mod.1, mod.0)[2,6] < 0.05) {
#    cat("Separate intercepts for year\n")
#    mod <- mod.1
#  } else {
#    cat("Common model across years\n")
#    mod <- mod.0
#  }

  plot(mod)

  # TODO: using a GLS fit is probably the better way to go here, as there's substantial
  #       auto-correlation (as would make sense!)
  #
  #       Further, we may be able to utilise the fact that episurv number keeps increasing
  #
  #       At the moment, the detection here sucks

  # find gross outliers with large residuals
  resid_cluster <- kmeans(abs(resid(mod)), 2)
  large_resid   <- rep(FALSE, n)
  large_resid[!bad] <- resid_cluster$cluster == which.max(resid_cluster$centers)

  bad <- bad | large_resid
  bad_reason[large_resid] <- append(bad_reason[large_resid], "OutlierMismatch")
  resid_col <- rep(NA, n)
  resid_col2 <- rep(NA, n)
  resid_col[large_resid] <- abs(rstandard(mod))[large_resid]
  resid_col2[large_resid] <- abs(rstudent(mod))[large_resid]

  cbind(dat, bad, bad_reason, resid_col, resid_col2)
}

