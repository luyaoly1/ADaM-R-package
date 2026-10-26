#' Derive ADAE Dataset from SDTM AE and ADaM ADSL (Tidyverse Implementation)
#'
#' @param ae An SDTM AE dataframe.
#' @param adsl An ADaM ADSL dataframe.
#'
#' @return An ADAE dataframe compliant with CDISC conventions.
#' @export
#' @import dplyr
#' @importFrom lubridate ymd days_in_month
derive_adae <- function(ae, adsl) {

  # Helper: Impute missing start date components to first day ("YYYY-01-01" or "YYYY-MM-01")
  impute_astdt <- function(dtc) {
    case_when(
      is.na(dtc) | dtc == "" ~ as.Date(NA),
      nchar(dtc) == 10 ~ suppressWarnings(lubridate::ymd(dtc)),
      nchar(dtc) == 7  ~ suppressWarnings(lubridate::ymd(paste0(dtc, "-01"))),
      nchar(dtc) == 4  ~ suppressWarnings(lubridate::ymd(paste0(dtc, "-01-01"))),
      TRUE ~ as.Date(NA)
    )
  }

  # Helper: Impute missing end date components to last day of month/year
  impute_aendt <- function(dtc) {
    case_when(
      is.na(dtc) | dtc == "" ~ as.Date(NA),
      nchar(dtc) == 10 ~ suppressWarnings(lubridate::ymd(dtc)),
      nchar(dtc) == 7  ~ {
        d <- suppressWarnings(lubridate::ymd(paste0(dtc, "-01")))
        d + lubridate::days_in_month(d) - 1
      },
      nchar(dtc) == 4  ~ suppressWarnings(lubridate::ymd(paste0(dtc, "-12-31"))),
      TRUE ~ as.Date(NA)
    )
  }

  ae %>%
    # 1. Merge subject demographics and treatment variables from ADSL
    left_join(
      adsl %>% select(STUDYID, USUBJID, TRTSDT, TRTEDT, TRT01A, TRT01P, AGE, SEX, RACE),
      by = c("STUDYID", "USUBJID")
    ) %>%

    # 2. Convert character ISO dates to R Date objects and impute partial dates
    mutate(
      ASTDT = impute_astdt(AESTDTC),
      AENDT = impute_aendt(AEENDTC),

      # Record Imputation Flags ('D' = Day imputed, 'M' = Month imputed)
      ASTDTF = case_when(
        nchar(AESTDTC) == 7 ~ "D",
        nchar(AESTDTC) == 4 ~ "M",
        TRUE ~ NA_character_
      ),
      AENDTF = case_when(
        nchar(AEENDTC) == 7 ~ "D",
        nchar(AEENDTC) == 4 ~ "M",
        TRUE ~ NA_character_
      ),

      # 3. Calculate Relative Study Days (CDISC Rule: No Day 0)
      ASTDY = if_else(ASTDT >= TRTSDT, as.numeric(ASTDT - TRTSDT) + 1, as.numeric(ASTDT - TRTSDT)),
      AENDY = if_else(AENDT >= TRTSDT, as.numeric(AENDT - TRTSDT) + 1, as.numeric(AENDT - TRTSDT)),

      # 4. Calculate AE Duration in Days
      ADURN = if_else(!is.na(ASTDT) & !is.na(AENDT), as.numeric(AENDT - ASTDT) + 1, NA_real_),
      ADURU = if_else(!is.na(ADURN), "DAYS", NA_character_),

      # 5. Set Treatment-Emergent Flag ('Y' if event start date >= treatment start date)
      TRTEMFL = if_else(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT, "Y", NA_character_)
    ) %>%

    # 6. Sort and assign Analysis Sequence Number (ASEQ) per subject
    group_by(STUDYID, USUBJID) %>%
    arrange(ASTDT, AESTDTC, AESEQ, .by_group = TRUE) %>%
    mutate(ASEQ = row_number()) %>%
    ungroup()
}
