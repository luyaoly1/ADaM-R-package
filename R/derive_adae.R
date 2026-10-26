#' Derive ADAE Dataset from SDTM AE and ADaM ADSL
#'
#' @param ae An SDTM AE dataframe.
#' @param adsl An ADaM ADSL dataframe.
#'
#' @return An ADAE dataframe compliant with CDISC standards.
#' @export
#' @import dplyr
#' @importFrom admiral derive_vars_merged derive_vars_dt derive_vars_dy derive_vars_duration derive_var_trtemfl derive_var_obs_number exprs
derive_adae <- function(ae, adsl) {
  ae %>%
    derive_vars_merged(
      dataset_add = adsl,
      new_vars = exprs(TRTSDT, TRTEDT, TRT01A, TRT01P, AGE, SEX, RACE),
      by_vars = exprs(STUDYID, USUBJID)
    ) %>%
    derive_vars_dt(
      new_vars_prefix = "AST",
      dtc = AESTDTC,
      date_imputation = "first",
      highest_imputation = "M",
      flag_imputation = "auto"
    ) %>%
    derive_vars_dt(
      new_vars_prefix = "AEN",
      dtc = AEENDTC,
      date_imputation = "last",
      highest_imputation = "M",
      flag_imputation = "auto"
    ) %>%
    derive_vars_dy(
      reference_date = TRTSDT,
      source_vars = exprs(ASTDT, AENDT)
    ) %>%
    derive_vars_duration(
      new_var = ADURN,
      new_var_unit = ADURU,
      start_date = ASTDT,
      end_date = AENDT
    ) %>%
    derive_var_trtemfl(
      start_date = ASTDT,
      end_date = AENDT,
      trt_start_date = TRTSDT,
      trt_end_date = TRTEDT
    ) %>%
    derive_var_obs_number(
      new_var = ASEQ,
      by_vars = exprs(STUDYID, USUBJID),
      order = exprs(ASTDT, AESTDTC, AESEQ)
    )
}
