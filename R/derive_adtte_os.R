#' By Yao Lu. Derive Overall Survival (OS) for ADTTE Dataset
#'
#' @param adsl An ADaM ADSL dataframe containing TRTSDT, DTHDT, and LSTALVDT.
#'
#' @return An ADTTE dataframe with PARAMCD = "OS".
#' @export
#' @import dplyr
derive_adtte_os <- function(adsl) {

  adsl %>%
    filter(!is.na(TRTSDT)) %>%
    mutate(
      PARAM = "Overall Survival",
      PARAMCD = "OS",

      # Event Date (Death) or Censoring Date (Last Known Alive)
      ADT = if_else(!is.na(DTHDT), DTHDT, LSTALVDT),

      # Censor flag: 0 = Event (Death), 1 = Censored (Alive)
      CNSR = if_else(!is.na(DTHDT), 0, 1),
      EVNTDESC = if_else(CNSR == 0, "Death", "Alive"),

      # Time to Event in Months (Standard CDISC: 1 month = 30.4375 days)
      AVAL = (as.numeric(ADT - TRTSDT) + 1) / 30.4375,
      AVALU = "MONTHS"
    ) %>%
    select(STUDYID, USUBJID, TRTSDT, PARAM, PARAMCD, ADT, CNSR, EVNTDESC, AVAL, AVALU)
}
