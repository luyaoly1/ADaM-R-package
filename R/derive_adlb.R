#' Derive Laboratory Analysis Dataset (ADLB) Core Variables
#'
#' Merges SDTM LB with ADSL to derive standard ADaM Laboratory (ADLB) variables
#' including Analysis Date (ADT), Relative Day (ADY), Baseline Flag (ABLFL),
#' Baseline Value (BASE), Change from Baseline (CHG), and Percent Change (PCHG).
#'
#' @param lb SDTM Laboratory domain data frame containing USUBJID, LBTEST,
#'   LBTESTCD, LBSTRESN, LBSTRESU, and LBDTC.
#' @param adsl ADaM Subject-Level Analysis Dataset containing STUDYID, USUBJID, and TRTSDT.
#'
#' @return A data frame with derived ADLB variables.
#' @export
#' @importFrom dplyr %>% filter mutate inner_join left_join arrange group_by ungroup select if_else row_number n
derive_adlb <- function(lb, adsl) {

  # Step 1: Merge LB with ADSL and derive Analysis Date/Day
  adlb_raw <- lb %>%
    filter(!is.na(LBSTRESN)) %>%
    inner_join(
      adsl %>% select(STUDYID, USUBJID, TRTSDT),
      by = c("STUDYID", "USUBJID")
    ) %>%
    mutate(
      # Extract YYYY-MM-DD from ISO LBDTC string
      ADT = as.Date(substr(LBDTC, 1, 10)),

      # CDISC Relative Day ADY (No Day 0)
      ADY = if_else(ADT >= TRTSDT,
                    as.numeric(ADT - TRTSDT) + 1,
                    as.numeric(ADT - TRTSDT)),

      # Standard Analysis Value & Parameter definitions
      AVAL = LBSTRESN,
      AVALU = LBSTRESU,
      PARAM = paste0(LBTEST, " (", LBSTRESU, ")"),
      PARAMCD = LBTESTCD
    )

  # Step 2: Identify Baseline Records (Last non-missing record on or before TRTSDT)
  baseline_vals <- adlb_raw %>%
    filter(!is.na(ADT), !is.na(TRTSDT), ADT <= TRTSDT) %>%
    group_by(STUDYID, USUBJID, PARAMCD) %>%
    arrange(ADT, .by_group = TRUE) %>%
    filter(row_number() == n()) %>%
    select(STUDYID, USUBJID, PARAMCD, BASE = AVAL) %>%
    ungroup()

  # Step 3: Join BASE back and derive CHG and PCHG
  adlb_derived <- adlb_raw %>%
    left_join(baseline_vals, by = c("STUDYID", "USUBJID", "PARAMCD")) %>%
    mutate(
      # Baseline flag for records on/before treatment start
      ABLFL = if_else(!is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT, "Y", NA_character_),

      # Change from baseline
      CHG = if_else(!is.na(BASE), AVAL - BASE, NA_real_),

      # Percent change from baseline
      PCHG = if_else(!is.na(BASE) & BASE != 0, ((AVAL - BASE) / BASE) * 100, NA_real_)
    ) %>%
    select(
      STUDYID, USUBJID, TRTSDT, PARAM, PARAMCD,
      LBDTC, ADT, ADY, AVAL, AVALU, ABLFL, BASE, CHG, PCHG
    )

  return(adlb_derived)
}
