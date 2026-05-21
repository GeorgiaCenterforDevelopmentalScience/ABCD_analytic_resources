#Library

library(dplyr)
library(psych)
library(stringr)
library(purrr)
library(MplusAutomation)

########## SET WORKING DIRECTORY, OUTPUT DIRECTORY, AND FILE NAME ########## 

setwd("/home/cjh37695/ABCD_PROJECTS/")

OUT_DIR <- file.path(getwd(), "SUICIDALITY_SOCIAL_FACTORS")

# create name with analytic step 
FILENAME <- "ABCD_SSP_Vprep"

# get today's date appended
DATE_STAMP <- format(Sys.Date(), "%m.%d.%y")

########## LOAD IN BASE_DATA df ########## 

df <- read.csv(
  file.path(OUT_DIR, "ABCD_SSP_BASE_DATA_05.21.26.csv")
)

codebook <- read.csv(
  file.path(OUT_DIR, "ABCD_SSP_CODEBOOK_05.21.26.csv")
)

################################################################################
##################### COMPUTE WAVE-SPECIFIC PAST/PRESENT SUICIDE INDICATORS

# This "total" approach is used because we are interested broadly in if these symptoms are 
# present for a person by that wave or not. This is adopted because there are 
# idiosyncrasies for people between waves that make us weary to adopt dynamic 
# wave-specific scores as accurate indices of waxing and waning suicide risk. 

# helper function: Create a broad dummy from two component variables.
# Each broad dummy = 1 if either present-year or past-year item is endorsed.
# If both component items are missing, broad dummy = NA; otherwise = 0.

past_pres_SuRrk_dummy <- function(data, new_var, var1, var2) {
  
  requested_vars <- c(var1, var2)
  available_vars <- requested_vars[requested_vars %in% names(data)]
  missing_vars   <- setdiff(requested_vars, available_vars)
  
  if (length(available_vars) == 0) {
    warning("Neither ", var1, " nor ", var2, " found. ", new_var, " not created.")
    return(data)
  }
  
  vals <- data[available_vars]
  
  n_full_na <- sum(rowSums(!is.na(vals)) == 0)
  n_usable  <- sum(rowSums(!is.na(vals)) > 0)
  
  data[[new_var]] <- ifelse(
    rowSums(vals == 1, na.rm = TRUE) > 0,
    1,
    ifelse(
      rowSums(!is.na(vals)) == 0,
      NA,
      0
    )
  )
  
  cat("\nCreated:", new_var, "\n")
  cat("  Found variables:", paste(available_vars, collapse = ", "), "\n")
  
  if (length(missing_vars) > 0) {
    cat("  Missing variables:", paste(missing_vars, collapse = ", "), "\n")
  } else {
    cat("  Missing variables: none\n")
  }
  
  cat("  Rows fully NA across available input variable(s):", n_full_na, "\n")
  cat("  Rows with at least one non-missing input value:", n_usable, "\n")
  
  return(data)
}

###### applied to passive suicidal idealizations 
#GCDS version: SuIdPas[pr|pt]_[W], ABCD version: mh_y_ksads__suic__pass__[pres|past]_dx

df <- past_pres_SuRrk_dummy(df, "SuIdPasT_1", "SuIdPaspr_1", "SuIdPaspt_1")
df <- past_pres_SuRrk_dummy(df, "SuIdPasT_3", "SuIdPaspr_3", "SuIdPaspt_3")
df <- past_pres_SuRrk_dummy(df, "SuIdPasT_5", "SuIdPaspr_5", "SuIdPaspt_5")
df <- past_pres_SuRrk_dummy(df, "SuIdPasT_7", "SuIdPaspr_7", "SuIdPaspt_7")
df <- past_pres_SuRrk_dummy(df, "SuIdPasT_9", "SuIdPaspr_9", "SuIdPaspt_9")
df <- past_pres_SuRrk_dummy(df, "SuIdPasT_11", "SuIdPaspr_11", "SuIdPaspt_11")
df <- past_pres_SuRrk_dummy(df, "SuIdPasT_13", "SuIdPaspr_13", "SuIdPaspt_13")

###### applied to suicide attempts 
#GCDS version: SuAtt[pr|pt]_[W], ABCD version:  mh_y_ksads__suic__atmpt__[pres|past]_dx 

df <- past_pres_SuRrk_dummy(df, "SuAttT_1", "SuAttpr_1", "SuAttpt_1")
df <- past_pres_SuRrk_dummy(df, "SuAttT_3", "SuAttpr_3", "SuAttpt_3")
df <- past_pres_SuRrk_dummy(df, "SuAttT_5", "SuAttpr_5", "SuAttpt_5")
df <- past_pres_SuRrk_dummy(df, "SuAttT_7", "SuAttpr_7", "SuAttpt_7")
df <- past_pres_SuRrk_dummy(df, "SuAttT_9", "SuAttpr_9", "SuAttpt_9")
df <- past_pres_SuRrk_dummy(df, "SuAttT_11", "SuAttpr_11", "SuAttpt_11")
df <- past_pres_SuRrk_dummy(df, "SuAttT_13", "SuAttpr_13", "SuAttpt_13")

###### applied to suicidal ideations: active methods
#GCDS version: SuIdMTD[pr|pt]_[W], ABCD version:  mh_y_ksads__suic__actv__mthd__[pres|past]_dx 

df <- past_pres_SuRrk_dummy(df, "SuIdMtdT_1", "SuIdMTDpr_1", "SuIdMTDpt_1")
df <- past_pres_SuRrk_dummy(df, "SuIdMtdT_3", "SuIdMTDpr_3", "SuIdMTDpt_3")
df <- past_pres_SuRrk_dummy(df, "SuIdMtdT_5", "SuIdMTDpr_5", "SuIdMTDpt_5")
df <- past_pres_SuRrk_dummy(df, "SuIdMtdT_7", "SuIdMTDpr_7", "SuIdMTDpt_7")
df <- past_pres_SuRrk_dummy(df, "SuIdMtdT_9", "SuIdMTDpr_9", "SuIdMTDpt_9")
df <- past_pres_SuRrk_dummy(df, "SuIdMtdT_11", "SuIdMTDpr_11", "SuIdMTDpt_11")
df <- past_pres_SuRrk_dummy(df, "SuIdMtdT_13", "SuIdMTDpr_13", "SuIdMTDpt_13")


###### applied to suicidal ideations: active nonspecific
#GCDS version: SuIdNonS[pr|pt]_[W], ABCD version: mh_y_ksads__suic__actv__[pres|past]_dx 

df <- past_pres_SuRrk_dummy(df, "SuIdNonST_1", "SuIdNonSpr_1", "SuIdNonSpt_1")
df <- past_pres_SuRrk_dummy(df, "SuIdNonST_3", "SuIdNonSpr_3", "SuIdNonSpt_3")
df <- past_pres_SuRrk_dummy(df, "SuIdNonST_5", "SuIdNonSpr_5", "SuIdNonSpt_5")
df <- past_pres_SuRrk_dummy(df, "SuIdNonST_7", "SuIdNonSpr_7", "SuIdNonSpt_7")
df <- past_pres_SuRrk_dummy(df, "SuIdNonST_9", "SuIdNonSpr_9", "SuIdNonSpt_9")
df <- past_pres_SuRrk_dummy(df, "SuIdNonST_11", "SuIdNonSpr_11", "SuIdNonSpt_11")
df <- past_pres_SuRrk_dummy(df, "SuIdNonST_13", "SuIdNonSpr_13", "SuIdNonSpt_13")


##----------- remove the wave-specific past- and present-dignosis variables 

df <- df %>%
  dplyr::select(
    -dplyr::matches(
      "^(SuAttpt|SuAttpr|SuIdNonSpt|SuIdNonSpr|SuIdActpt|SuIdActpr|SuIdPaspt|SuIdPaspr)_\\d+$"
    )
  )

#check it 
names(df)

################################################################################
####### COMPUTE EVENT-HISTORY SUICIDE-RISK VARIABLES FOR SURVIVAL ANALYSIS

# Recode wave-specific suicide outcomes into first-onset event-history indicators.
# Participants remain in the risk set until their first observed event. 
# later waves are set to NA after onset.
# This format is required for discrete-time survival models estimating 
# wave-specific first-event hazard.
## People who experienced the event "drop out"

################################################################################
##################### COMPUTE EVENT-HISTORY VARIABLES FOR SURVIVAL ANALYSIS

# Recode wave-specific event indicators into first-onset event-history indicators.
# Participants remain in the risk set until first observed onset; later waves are set to NA.
# This supports discrete-time survival models estimating wave-specific first-event hazard.

survival_event_coding <- function(data, base_var, waves, prefix = "SV_") {
  
  # Construct input and output variable names
  input_vars  <- paste0(base_var, "_", waves)
  output_vars <- paste0(prefix, base_var, "_", waves)
  
  # Keep only waves that are actually present in the data
  available <- input_vars %in% names(data)
  
  if (!any(available)) {
    warning("No variables found for ", base_var, ". No survival variables created.")
    return(data)
  }
  
  input_vars  <- input_vars[available]
  output_vars <- output_vars[available]
  waves_found <- waves[available]
  waves_miss  <- waves[!available]
  
  # Copy original event indicators into survival-coded variables
  data[output_vars] <- data[input_vars]
  
  # Set later waves to NA after first observed event
  for (i in seq_along(output_vars)) {
    
    if (i == 1) next
    
    earlier_vars <- output_vars[1:(i - 1)]
    
    prior_event <- rowSums(data[earlier_vars] == 1, na.rm = TRUE) > 0
    
    data[[output_vars[i]]][prior_event] <- NA
  }
  
  # Print diagnostic summary
  cat("\nCreated survival-coded variables for:", base_var, "\n")
  cat("  Waves found:", paste0("_", waves_found, collapse = ", "), "\n")
  
  if (length(waves_miss) > 0) {
    cat("  Waves missing:", paste0("_", waves_miss, collapse = ", "), "\n")
  } else {
    cat("  Waves missing: none\n")
  }
  
  cat("  New variables:", paste(output_vars, collapse = ", "), "\n")
  
  cat("  Post-coding event counts:\n")
  print(lapply(data[output_vars], table, useNA = "ifany"))
  
  return(data)
}

######  APPLIED  

# indicate the wave suffixes to be included 
surv_waves <- c(1, 3, 5, 7, 9, 11, 13)

# Passive suicidal ideation
df <- survival_event_coding(
  data = df,
  base_var = "SuIdPasT",
  waves = surv_waves
)

# Suicide attempts
df <- survival_event_coding(
  data = df,
  base_var = "SuAttT",
  waves = surv_waves
)

# Active suicidal ideation with method
df <- survival_event_coding(
  data = df,
  base_var = "SuIdMtdT",
  waves = surv_waves
)

# Active nonspecific suicidal ideation
df <- survival_event_coding(
  data = df,
  base_var = "SuIdNonST",
  waves = surv_waves
)


################################################################################
##################### SUMMARIZE SUICIDE-RISK PREVALENCE AND FIRST-ONSET EVENTS

# This table summarizes each suicide-risk indicator by wave:
# 1) Original wave-specific prevalence: number and percent endorsing the original variable.
# 2) First-onset prevalence: number and percent endorsing the SV_ event-history variable.
# 3) Wave-specific chi-square tests by Y_SEX for both original and first-onset indicators.

summarize_survival_indicator <- function(data, base_var, waves, sex_var = "Y_SEX") {
  
  out <- lapply(waves, function(w) {
    
    orig_var <- paste0(base_var, "_", w)
    sv_var   <- paste0("SV_", base_var, "_", w)
    
    # Skip wave if neither original nor SV variable exists
    if (!orig_var %in% names(data) & !sv_var %in% names(data)) {
      warning("Neither ", orig_var, " nor ", sv_var, " found. Skipping wave ", w, ".")
      return(NULL)
    }
    
    #### Original wave-specific prevalence
    
    if (orig_var %in% names(data)) {
      
      orig_n_obs <- sum(!is.na(data[[orig_var]]))
      orig_n_1   <- sum(data[[orig_var]] == 1, na.rm = TRUE)
      orig_pct   <- ifelse(orig_n_obs > 0, 100 * orig_n_1 / orig_n_obs, NA_real_)
      
      orig_chisq_p <- tryCatch({
        tab <- table(data[[orig_var]], data[[sex_var]], useNA = "no")
        if (all(dim(tab) >= c(2, 2))) {
          suppressWarnings(chisq.test(tab)$p.value)
        } else {
          NA_real_
        }
      }, error = function(e) NA_real_)
      
    } else {
      
      orig_n_obs <- NA_integer_
      orig_n_1   <- NA_integer_
      orig_pct   <- NA_real_
      orig_chisq_p <- NA_real_
    }
    
    
    #### First-onset / event-history prevalence
    
    if (sv_var %in% names(data)) {
      
      sv_n_obs <- sum(!is.na(data[[sv_var]]))
      sv_n_1   <- sum(data[[sv_var]] == 1, na.rm = TRUE)
      sv_pct   <- ifelse(sv_n_obs > 0, 100 * sv_n_1 / sv_n_obs, NA_real_)
      
      sv_chisq_p <- tryCatch({
        tab <- table(data[[sv_var]], data[[sex_var]], useNA = "no")
        if (all(dim(tab) >= c(2, 2))) {
          suppressWarnings(chisq.test(tab)$p.value)
        } else {
          NA_real_
        }
      }, error = function(e) NA_real_)
      
    } else {
      
      sv_n_obs <- NA_integer_
      sv_n_1   <- NA_integer_
      sv_pct   <- NA_real_
      sv_chisq_p <- NA_real_
    }
    
    
    #### Return one row
    
    data.frame(
      INDICATOR = base_var,
      WAVE = paste0("_", w),
      
      ORIGINAL_VAR = ifelse(orig_var %in% names(data), orig_var, NA),
      ORIGINAL_N_OBSERVED = orig_n_obs,
      ORIGINAL_N_EVENT = orig_n_1,
      ORIGINAL_PCT = round(orig_pct, 2),
      ORIGINAL_CHISQ_BY_SEX_P = round(orig_chisq_p, 4),
      
      SV_VAR = ifelse(sv_var %in% names(data), sv_var, NA),
      SV_RISKSET_N_OBSERVED = sv_n_obs,
      SV_NEW_ONSET_N = sv_n_1,
      SV_NEW_ONSET_PCT = round(sv_pct, 2),
      SV_CHISQ_BY_SEX_P = round(sv_chisq_p, 4),
      
      stringsAsFactors = FALSE
    )
  })
  
  dplyr::bind_rows(out)
}


################################################################################
##################### APPLY SUMMARY FUNCTION

surv_waves <- c(1, 3, 5, 7, 9, 11, 13) ## the final waves have a suscpicious missing pattern - don't use

SUICIDE_RISK_SUMMARY <- dplyr::bind_rows(
  
  summarize_survival_indicator(
    data = df,
    base_var = "SuIdPasT",
    waves = surv_waves
  ),
  
  summarize_survival_indicator(
    data = df,
    base_var = "SuAttT",
    waves = surv_waves
  ),
  
  summarize_survival_indicator(
    data = df,
    base_var = "SuIdMtdT",
    waves = surv_waves
  ),
  
  summarize_survival_indicator(
    data = df,
    base_var = "SuIdNonST",
    waves = surv_waves
  )
)

SUICIDE_RISK_SUMMARY


##################### CUMULATIVE FIRST-ONSET SUMMARY

CUMULATIVE_ONSET_SUMMARY <- SUICIDE_RISK_SUMMARY %>%
  dplyr::group_by(INDICATOR) %>%
  dplyr::summarise(
    BASELINE_N = dplyr::first(ORIGINAL_N_OBSERVED),
    CUMULATIVE_NEW_ONSET_N = sum(SV_NEW_ONSET_N, na.rm = TRUE),
    CUMULATIVE_NEW_ONSET_PCT_BASELINE = round(
      100 * CUMULATIVE_NEW_ONSET_N / 11775, 2
    ),
    .groups = "drop"
  )

CUMULATIVE_ONSET_SUMMARY


################################################################################
##################### REMOVE ORIGINAL WAVE-SPECIFIC SUICIDE-RISK VARIABLES

# After creating first-onset event-history variables and verifying summary output,
# remove the original repeated wave-specific suicide-risk indicators.
# The SV_ variables are retained as the analysis-ready DTS/event-history outcomes.

df_RED <- df %>%
  dplyr::select(
    -dplyr::matches("^(SuIdPasT|SuAttT|SuIdMtdT|SuIdNonST)_\\d+$")
  )

################## SAVE THE DATA

# save dataframe
write.csv(
  df_RED, 
  file.path(OUT_DIR, paste0(FILENAME, "_BASE_DATA_SUID_", DATE_STAMP, ".csv")), 
  row.names=FALSE, na="")
