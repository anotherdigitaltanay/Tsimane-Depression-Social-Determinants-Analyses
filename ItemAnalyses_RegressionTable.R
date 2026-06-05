############### This script summarises the regression co-efficients from the 16 item level models      #################

############### Loading the relevant packages #################
###############################################################
library(brms)
library(dplyr)
library(purrr)
library(flextable)
library(posterior)


############### Loading the Models from RDS files ################################
##################################################################################

# Loading all 16 item models fitted and saved in the previous script steps
item1  <- readRDS("item1.RDS")
item2  <- readRDS("item2.RDS")
item3  <- readRDS("item3.RDS")
item4  <- readRDS("item4.RDS")
item5  <- readRDS("item5.RDS")
item6  <- readRDS("item6.RDS")
item7  <- readRDS("item7.RDS")
item8  <- readRDS("item8.RDS")
item9  <- readRDS("item9.RDS")
item10 <- readRDS("item10.RDS")
item11 <- readRDS("item11.RDS")
item12 <- readRDS("item12.RDS")
item13 <- readRDS("item13.RDS")
item14 <- readRDS("item14.RDS")
item16 <- readRDS("item16.RDS")
item18 <- readRDS("item18.RDS")



############### Creating a function to extract estimates for a single model #################
##################################################################################

extract_model_estimates <- function(model, item_name, outcome_prefix) {
  
  # Extracting posterior draws
  draws <- as_draws_df(model)
  
  # Initializing an empty dataframe to store the results
  results <- data.frame(Predictor = character(), Estimate = character(), stringsAsFactors = FALSE)
  
  # -------------------------------------------------------------------------
  # 1. Processing Fixed Effects, Imputed Effects, Intercepts, and Spline terms
  # -------------------------------------------------------------------------
  pop_cols <- grep(paste0("^(b_|bsp_|bs_|sds_)", outcome_prefix), names(draws), value = TRUE)
  
  for (col in pop_cols) {
    posterior <- draws[[col]]
    
    mean_est <- mean(posterior)
    ci_lower <- quantile(posterior, 0.025)
    ci_upper <- quantile(posterior, 0.975)
    p_pos <- mean(posterior > 0)
    
    # Cleaning the predictor name for the table 
    if (grepl("^sds_", col)) {
      clean_name <- gsub(paste0("^sds_", outcome_prefix, "_?"), "Spline SD: ", col)
    } else if (grepl("^bs_", col)) {
      clean_name <- gsub(paste0("^bs_", outcome_prefix, "_?"), "Spline Base: ", col)
    } else {
      clean_name <- gsub(paste0("^(b_|bsp_)", outcome_prefix, "_?"), "", col)
      clean_name <- gsub("^mi", "", clean_name) # Remove 'mi' prefix from imputed vars
    }
    
    # Formatting string as: Mean [95% CI Lower, 95% CI Upper], P(>0) = X.XX
    est_str <- sprintf("%.2f [%.2f, %.2f], P(>0) = %.2f", mean_est, ci_lower, ci_upper, p_pos)
    results <- rbind(results, data.frame(Predictor = clean_name, Estimate = est_str))
  }
  
  # -------------------------------------------------------------------------
  # 2. Calculating Intraclass Correlation Coefficients (ICCs)
  # -------------------------------------------------------------------------
  
  # First identifying the standard deviation columns for the random effects
  sd_com_col <- grep(paste0("^sd_ComID__", outcome_prefix, "_Intercept"), names(draws), value = TRUE)
  sd_hh_col  <- grep(paste0("^sd_ComID:UniqueFamID__", outcome_prefix, "_Intercept"), names(draws), value = TRUE)
  sd_int_col <- grep(paste0("^sd_Interviewer__", outcome_prefix, "_Intercept"), names(draws), value = TRUE)
  
  # Calculating variance for each posterior draw
  var_com <- draws[[sd_com_col]]^2
  var_hh  <- draws[[sd_hh_col]]^2
  var_int <- draws[[sd_int_col]]^2
  
  # Residual variance for a cumulative logit model is pi^2 / 3
  var_res <- (pi^2) / 3 
  
  # Calculating Total variance
  total_var <- var_com + var_hh + var_int + var_res
  
  # Calculating ICCs for different levels
  icc_com <- var_com / total_var
  icc_hh  <- var_hh / total_var
  icc_int <- var_int / total_var
  icc_res <- var_res / total_var
  
  # Creating a function to represent the ICCs
  format_icc <- function(posterior) {
    mean_est <- mean(posterior)
    ci_lower <- quantile(posterior, 0.025)
    ci_upper <- quantile(posterior, 0.975)
    p_pos <- mean(posterior > 0) # Will naturally be 1.00 since variance > 0
    sprintf("%.2f [%.2f, %.2f], P(>0) = %.2f", mean_est, ci_lower, ci_upper, p_pos)
  }
  
  # Appending the calculated ICCs to results
  icc_df <- data.frame(
    Predictor = c("ICC (Community)", "ICC (Household)", "ICC (Interviewer)", "ICC (Residual)"),
    Estimate = c(format_icc(icc_com), format_icc(icc_hh), format_icc(icc_int), format_icc(icc_res)),
    stringsAsFactors = FALSE
  )
  results <- rbind(results, icc_df)
  
  # Renaming the Estimate column to the respective Item name
  colnames(results)[2] <- item_name
  
  return(results)
}

############### Creating a function to generate a formatted flextable #######################
##################################################################################

format_item_flextable <- function(df) {

  ft <- flextable(df)
  
  # Adding the explanatory line to the top of the table
  ft <- add_header_lines(ft, values = "Estimates are presented as: Mean Estimate [95% Credible Interval], P(>0)")
  
  # Making the column headers bold, applying a grey backdrop to the first column and autofitting the table
  ft <- bold(ft, part = "header")
  ft <- bg(ft, j = 1, bg = "#D3D3D3", part = "body")
  ft <- autofit(ft)
  
  return(ft)
}


############### Generating Table 1 (Items 1 to 4) ################################
##################################################################################

t1_item1 <- extract_model_estimates(item1, "Sadness", "D1Sad")
t1_item2 <- extract_model_estimates(item2, "Crying", "D2Cry")
t1_item3 <- extract_model_estimates(item3, "Self-Critical", "D3SelfCritical")
t1_item4 <- extract_model_estimates(item4, "Suicidal Ideation", "D4LifeNotValuable")

# Joining results efficiently by Predictor using purrr::reduce
table1_df <- list(t1_item1, t1_item2, t1_item3, t1_item4) %>%
  reduce(full_join, by = "Predictor")

# Generating Table 1
table1_ft <- format_item_flextable(table1_df)
table1_ft


############### Generating Table 2 (Items 5 to 8) ################################
##################################################################################

t2_item5 <- extract_model_estimates(item5, "Uselessness", "D5Useless")
t2_item6 <- extract_model_estimates(item6, "Apathy", "D6NoInterest")
t2_item7 <- extract_model_estimates(item7, "Fatigue", "D7Tired")
t2_item8 <- extract_model_estimates(item8, "Concentration", "D8CantConcentrate")

table2_df <- list(t2_item5, t2_item6, t2_item7, t2_item8) %>%
  reduce(full_join, by = "Predictor")

# Generating Table 2
table2_ft <- format_item_flextable(table2_df)
table2_ft


############### Generating Table 3 (Items 9 to 12) ###############################
##################################################################################

t3_item9  <- extract_model_estimates(item9, "Nervousness", "D9Nervous")
t3_item10 <- extract_model_estimates(item10, "Mistrust", "D10Paranoid")
t3_item11 <- extract_model_estimates(item11, "Sleep", "D11CantSleep")
t3_item12 <- extract_model_estimates(item12, "Eating", "D12CantEat")

table3_df <- list(t3_item9, t3_item10, t3_item11, t3_item12) %>%
  reduce(full_join, by = "Predictor")

# Generating Table 3
table3_ft <- format_item_flextable(table3_df)
table3_ft


############### Generating Table 4 (Items 13, 14, 16, 18) ########################
##################################################################################

t4_item13 <- extract_model_estimates(item13, "Libido", "D13CantFuck")
t4_item14 <- extract_model_estimates(item14, "Pessimism", "D14Pessimistic")
t4_item16 <- extract_model_estimates(item16, "Guilt", "D16PunishMe")
t4_item18 <- extract_model_estimates(item18, "Rumination", "D18HeavyThoughts")

table4_df <- list(t4_item13, t4_item14, t4_item16, t4_item18) %>%
  reduce(full_join, by = "Predictor")

# Generating Table 4
table4_ft <- format_item_flextable(table4_df)
table4_ft