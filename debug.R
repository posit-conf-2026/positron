# =============================================================================
# debug.R

library(tibble)
library(dplyr)

# ---- Sample data -----------------------------------------------------------
loan_applicants <- tibble(
  applicant_id  = c("A001", "A002", "A003", "A004"),
  annual_income = c(65000, 48000, 52000, 91000),
  monthly_debt  = c(1200, 900, 1100, 1500),
  credit_score  = c(710, 640, NA, 780),
  employment_years = c(4, 2, 6, 9)
)

# ---- Try it out -------------------------------------------------------------
# process_loan_application("A001", loan_applicants)
# process_loan_application("A003", loan_applicants)

process_loan_application <- function(applicant_id, applicants_df) {
  applicant_label <- paste("Processing application for", applicant_id)
  message(applicant_label)

  identity_result <- verify_identity(applicant_id, applicants_df)
  risk_result <- assess_credit_risk(applicant_id, applicants_df)

  decision <- list(
    applicant_id = applicant_id,
    identity_verified = identity_result,
    risk_score = risk_result
  )
  decision
}

verify_identity <- function(applicant_id, applicants_df) {
  applicant <- applicants_df |> filter(applicant_id == !!applicant_id)
  years_employed <- applicant$employment_years

  doc_check <- check_identity_documents(applicant)
  doc_check
}

check_identity_documents <- function(applicant) {
  has_ssn_on_file <- TRUE
  id_valid <- has_ssn_on_file && !is.na(applicant$applicant_id)
  id_valid
}

assess_credit_risk <- function(applicant_id, applicants_df) {
  applicant <- applicants_df |> filter(applicant_id == !!applicant_id)
  income_bracket <- if (applicant$annual_income > 60000) "high" else "standard"

  risk_score <- calculate_risk_score(applicant)
  risk_score
}

calculate_risk_score <- function(applicant) {
  monthly_income <- applicant$annual_income / 12
  dti <- applicant$monthly_debt / monthly_income

  tier <- classify_credit_tier(applicant$credit_score)

  score <- list(dti = dti, tier = tier)
  score
}

classify_credit_tier <- function(credit_score) {
  subprime_cutoff <- 620
  is_subprime <- credit_score < subprime_cutoff

  if (is_subprime) {
    "subprime"
  } else {
    "prime"
  }
}

