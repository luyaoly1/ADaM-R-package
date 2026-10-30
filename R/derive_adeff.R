#' Derive Efficacy Analysis Dataset (ADEFF)
#'
#' Merges SDTM RS (Disease Response) with ADSL to derive core ADaM Efficacy (ADEFF)
#' variables using high-performance data.table syntax.
#'
#' @param rs SDTM Disease Response (RS) domain data frame or data.table containing
#'   STUDYID, USUBJID, RSTESTCD, RSTEST, RSORRES, and RSDTC.
#' @param adsl ADaM Subject-Level Analysis Dataset containing STUDYID, USUBJID, and TRTSDT.
#'
#' @return A data.table containing derived ADEFF variables.
#' @export
#' @import data.table
derive_adeff <- function(rs, adsl) {

  # Defensive copy to data.table
  dt_rs <- as.data.table(rs)
  dt_adsl <- as.data.table(adsl)

  # 1. Merge RS with ADSL on STUDYID and USUBJID
  adeff <- merge(
    dt_rs[!is.na(RSORRES) & RSORRES != ""],
    dt_adsl[, .(STUDYID, USUBJID, TRTSDT)],
    by = c("STUDYID", "USUBJID"),
    all.x = FALSE
  )

  # 2. Derive Analysis Date, Relative Day, Parameter, and Values
  adeff[, `:=`(
    ADT = as.Date(substr(RSDTC, 1, 10)),
    PARAM = paste0(RSTEST, " Assessment"),
    PARAMCD = RSTESTCD,
    AVALC = RSORRES,
    AVAL = suppressWarnings(as.numeric(RSORRES))
  )]

  # CDISC Relative Day ADY (No Day 0)
  adeff[, ADY := ifelse(ADT >= TRTSDT,
                        as.numeric(ADT - TRTSDT) + 1,
                        as.numeric(ADT - TRTSDT))]

  # 3. Derive Baseline Values (last non-missing record on or before TRTSDT)
  setkey(adeff, STUDYID, USUBJID, PARAMCD, ADT)

  base_dt <- adeff[!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT,
                   .(BASE = AVAL[.N], BASEC = AVALC[.N]),
                   by = .(STUDYID, USUBJID, PARAMCD)]

  # Merge baseline back
  adeff <- merge(adeff, base_dt, by = c("STUDYID", "USUBJID", "PARAMCD"), all.x = TRUE)

  # 4. Derive Baseline Flag (ABLFL), Change (CHG), and Percent Change (PCHG)
  adeff[, `:=`(
    ABLFL = ifelse(!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT &
                     ADT == max(ADT[ADT <= TRTSDT], na.rm = TRUE), "Y", NA_character_),
    CHG = ifelse(!is.na(BASE) & !is.na(AVAL), AVAL - BASE, NA_real_),
    PCHG = ifelse(!is.na(BASE) & BASE != 0 & !is.na(AVAL), ((AVAL - BASE) / BASE) * 100, NA_real_)
  ), by = .(STUDYID, USUBJID, PARAMCD)]

  # 5. Reorder and return selected CDISC ADaM columns
  cols <- c("STUDYID", "USUBJID", "TRTSDT", "PARAM", "PARAMCD",
            "RSDTC", "ADT", "ADY", "AVAL", "AVALC", "ABLFL", "BASE", "BASEC", "CHG", "PCHG")

  return(adeff[, .SD, .SDcols = intersect(cols, names(adeff))])
}
