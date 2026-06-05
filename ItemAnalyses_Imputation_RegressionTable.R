############### This script generates the summary tables for the Imputation Sub-Models            #################
############### The tables include Mean, 95% CI, and P(>0) for all parameters      #################

############### Loading the relevant packages #################
###############################################################
library(brms)
library(dplyr)
library(purrr)
library(flextable)
library(posterior)

############### Loading the Models from RDS files ################################
##################################################################################

# Loading all 16 item models
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


############### Creating a Function to extract imputation estimates for a single model ######
##################################################################################

extract_imputation_estimates <- function(model, item_name) {
  
  # Extracting posterior draws
  draws <- as_draws_df(model)
  
  # Grabbing the fixed effects (b_) and residual standard deviations (sigma_) of interest
  target_cols <- grep("^(b_|sigma_)(SCI|SFluency|HHSize)", names(draws), value = TRUE)
  
  # Creating an empty dataframe to store the results
  results <- data.frame(Missing_Predictor = character(), Predictor = character(), Estimate = character(), stringsAsFactors = FALSE)
  
  # Looping through the target columns to calculate mean, CI, and P(>0)
  for (col in target_cols) {
    posterior <- draws[[col]]
    
    mean_est <- mean(posterior)
    ci_lower <- quantile(posterior, 0.025)
    ci_upper <- quantile(posterior, 0.975)
    p_pos <- mean(posterior > 0)
    
    # Identifying the Missing Predictor category
    if (grepl("SCI", col)) {
      missing_pred <- "Social Conflict Index"
    } else if (grepl("SFluency", col)) {
      missing_pred <- "Spanish Fluency"
    } else if (grepl("HHSize", col)) {
      missing_pred <- "Household Size"
    } else {
      missing_pred <- "Other"
    }
    
    # Making the predictor names readable for the table
    if (grepl("^b_", col)) {
      # E.g., extracts just "Age" from "b_SCI_Age"
      clean_name <- sub("^b_[A-Za-z]+_(.*)", "\\1", col)
    } else if (grepl("^sigma_", col)) {
      # Relabel standard deviations
      clean_name <- "Residual SD (sigma)"
    } else {
      clean_name <- col
    }
    
    # Cleaning up some final predictor names
    clean_name <- gsub("SexM", "Sex (Male)", clean_name)
    clean_name <- gsub("Sex1", "Sex (1)", clean_name)
    
    # Formatting string as: Mean [95% CI Lower, 95% CI Upper], P(>0) = X.XX %
    est_str <- sprintf("%.2f [%.2f, %.2f], P(>0) = %.2f", mean_est, ci_lower, ci_upper, p_pos)
    
    # Creating a dataframe now with the results
    results <- rbind(results, data.frame(
      Missing_Predictor = missing_pred, 
      Predictor = clean_name, 
      Estimate = est_str,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sorting the dataframe by grouping SCI, SFluency, and HHSize parameters together 
  results <- results %>% arrange(Missing_Predictor, Predictor)
  
  # Renaming columns to match final table output
  colnames(results) <- c("Missing Predictor", "Predictor", item_name)
  
  return(results)
}


############### Creating a Function to generate a formatted flextable #######################
##################################################################################

format_item_flextable <- function(df) {

  ft <- flextable(df)
  
  # Adding the explanatory line to the top of the table
  ft <- add_header_lines(ft, values = "Imputation Estimates are presented as: Mean Estimate [95% Credible Interval], P(>0)")
  
  # Making the column headers bold
  ft <- bold(ft, part = "header")
  
  # Vertically merging identical values in the "Missing Predictor" column for a clean, grouped look
  ft <- merge_v(ft, j = "Missing Predictor")
  
  # Apply a grey backdrop to the first TWO columns (the identifier columns)
  ft <- bg(ft, j = 1:2, bg = "#D3D3D3", part = "body")
  ft <- autofit(ft)
  
  # Adding vertical alignment to the top so merged cells look neat
  ft <- valign(ft, j = "Missing Predictor", valign = "top", part = "body")
  
  return(ft)
}


############### Generating Table 1 (Items 1 to 4) ################################
##################################################################################

t1_item1 <- extract_imputation_estimates(item1, "Sadness")
t1_item2 <- extract_imputation_estimates(item2, "Crying")
t1_item3 <- extract_imputation_estimates(item3, "Self-Critical")
t1_item4 <- extract_imputation_estimates(item4, "Suicidal Ideation")

# Joining them efficiently by both predictor columns using purrr::reduce
table1_df <- list(t1_item1, t1_item2, t1_item3, t1_item4) %>%
  reduce(full_join, by = c("Missing Predictor", "Predictor")) %>%
  arrange(`Missing Predictor`, Predictor)

# Generating table 1
table1_ft <- format_item_flextable(table1_df)
table1_ft


############### Generating Table 2 (Items 5 to 8) ################################
##################################################################################

t2_item5 <- extract_imputation_estimates(item5, "Uselessness")
t2_item6 <- extract_imputation_estimates(item6, "Apathy")
t2_item7 <- extract_imputation_estimates(item7, "Fatigue")
t2_item8 <- extract_imputation_estimates(item8, "Concentration")

table2_df <- list(t2_item5, t2_item6, t2_item7, t2_item8) %>%
  reduce(full_join, by = c("Missing Predictor", "Predictor")) %>%
  arrange(`Missing Predictor`, Predictor)

# Generating table 1
table2_ft <- format_item_flextable(table2_df)
table2_ft


############### Generating Table 3 (Items 9 to 12) ###############################
##################################################################################

t3_item9  <- extract_imputation_estimates(item9, "Nervousness")
t3_item10 <- extract_imputation_estimates(item10, "Mistrust")
t3_item11 <- extract_imputation_estimates(item11, "Sleep")
t3_item12 <- extract_imputation_estimates(item12, "Eating")

table3_df <- list(t3_item9, t3_item10, t3_item11, t3_item12) %>%
  reduce(full_join, by = c("Missing Predictor", "Predictor")) %>%
  arrange(`Missing Predictor`, Predictor)

# Generating table 3
table3_ft <- format_item_flextable(table3_df)
table3_ft


############### Generating Table 4 (Items 13, 14, 16, 18) ########################
##################################################################################

t4_item13 <- extract_imputation_estimates(item13, "Libido")
t4_item14 <- extract_imputation_estimates(item14, "Pessimism")
t4_item16 <- extract_imputation_estimates(item16, "Guilt")
t4_item18 <- extract_imputation_estimates(item18, "Rumination")

table4_df <- list(t4_item13, t4_item14, t4_item16, t4_item18) %>%
  reduce(full_join, by = c("Missing Predictor", "Predictor")) %>%
  arrange(`Missing Predictor`, Predictor)

# Generating table 4
table4_ft <- format_item_flextable(table4_df)
table4_ft