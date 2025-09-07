############### This script summarises the multilevel structure of the Tsimane depression dataset                                                #################
############### Importantly, it generates the pertinent summaries and plots only on the complete cases dataset (i.e. excluding missing data)     #################


############### Loading the relevant packages #################
###############################################################

########### If you haven't installed some of the packages below previously, type install.packages("the package name you want to install") ############
library(rstan)
library(cmdstanr)
library(rethinking)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(brms)
library(ggplot2)
library(patchwork)
library(knitr)
library(tidybayes)
library(broom.mixed)
library(bayesplot)
library(flextable) 
library(sjPlot)

## Setting seed for reproducibility
set.seed(9000)

# Running this so that one can see upto 10000 rows of printed results
options(max.print = 10000)

## Loading the cleaned dataset generated from "Tsimane Depression Analyses_Imputation.R"
depression_data <- read.csv("Cleaned_Depression_Dataset_for_Complete_Case_Analysis.csv")
str(depression_data)

## Removing observations having missing data
dep_data <- na.omit(depression_data)

## Renaming for making model formulas more readable
dep_data <- dep_data %>%
  rename("D" = "DepressionScoreM2", "Region" = "region", "Interview_Date" = "InterviewDate", "Sex" = "Male", "SCI" = "SocialConflictIndex", "SFluency" = "SpanishFluency", "Town_Distance" = "route_distance_town", "Com_Size" = "max_community_size", "HH_Size" = "household_size")

## Setting appropriate variable types
dep_data$Interview_Date <- as.POSIXct(dep_data$Interview_Date, tz = "UTC")

## Making transformations and/or setting appropriate variable types
dep_data$D <- standardize(dep_data$D)
dep_data$PID <- as.factor(dep_data$PID)
dep_data$UniqueFamID <- as.factor(dep_data$UniqueFamID)
dep_data$ComID <- as.factor(dep_data$ComID)
dep_data$Region <- as.factor(dep_data$Region)
dep_data$Interviewer <- as.factor(dep_data$Interviewer)
dep_data$Interview_Date <- standardize(as.numeric(dep_data$Interview_Date))
dep_data$Age <- standardize(dep_data$Age)
dep_data$Sex <- as.factor(dep_data$Sex)
dep_data$SCI <- standardize(as.numeric(dep_data$SCI))
dep_data$SFluency <- standardize(as.numeric(dep_data$SFluency))
dep_data$Town_Distance <- standardize(dep_data$Town_Distance)
dep_data$Com_Size <- standardize(dep_data$Com_Size)
dep_data$HH_Size <- standardize(dep_data$HH_Size)





################### Summary Tables and Plots

#### Summarizing the multilevel structure of the dataset in a summary stats table
#################################################################################

# Make summary dataframe for the multilevel structure of our dataset
multilevel_summary_stats <- dep_data %>%
  summarise(
    Variable = c("Regions", "Communities", "Households", "Individuals", "Total Observations contributed by all these levels"),
    N = c(n_distinct(Region), n_distinct(ComID), n_distinct(UniqueFamID), n_distinct(PID_FAMID), n()))


################### Now the summary table of the multilevel structure of our data table
multilevel_summary_stats_ft <- flextable(multilevel_summary_stats)

# Bold the column headers, apply a grey backdrop to the first column and autofit the table
multilevel_summary_stats_ft <- bold(multilevel_summary_stats_ft, part = "header")
multilevel_summary_stats_ft <- bg(multilevel_summary_stats_ft, j = 1, bg = "#D3D3D3", part = "body")
multilevel_summary_stats_ft <- autofit(multilevel_summary_stats_ft)

## Can copy the table directly from here on to word
multilevel_summary_stats_ft




#### Visualising the multilevel structure of the dataset via plots
##################################################################

## First we count how many communities are present in each region
com_per_region <- dep_data %>%
  distinct(Region, ComID) %>%                           # Outlines the communities present in each region
  count(Region) %>%                                     # Now precisely counting how many communities present in each region (in a summary estimate)
  count(n, name = "num_regions")                        # Counting now how many regions have the same count of communities

## Plot
plot1 <- ggplot(com_per_region, aes(x = factor(n), y = num_regions)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(
    title = "(a) Distribution of Communities per Region",
    x = "Number of Communities in Region",
    y = "Number of Regions"
  ) +
  scale_y_continuous(breaks = 0:max(com_per_region$num_regions)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )

##################################################################
## Now we count how many households are present in each communities
hh_per_com <- dep_data %>%
  distinct(ComID, UniqueFamID) %>%                           
  count(ComID) %>%                                     
  count(n, name = "num_coms") 

## Plot
plot2 <- ggplot(hh_per_com, aes(x = factor(n), y = num_coms)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(
    title = "(b) Distribution of Households per Community",
    x = "Number of Households in Community",
    y = "Number of Communities"
  ) +
  scale_y_continuous(breaks = 0:max(hh_per_com$num_coms)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )
  

##################################################################
## Now we count how many individuals are present in each household
id_per_hh <- dep_data %>%
  distinct(UniqueFamID, PID) %>%                           
  count(UniqueFamID) %>%                                     
  count(n, name = "num_hh") 

## Plot
plot3 <- ggplot(id_per_hh, aes(x = factor(n), y = num_hh)) +
  geom_bar(stat = "identity", fill = "Blue") +
  labs(
    title = " (c) Distribution of Individuals per Household",
    x = "Number of Individuals in Household",
    y = "Number of Households"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )



##################################################################
## Now we count how many observations are present for each individual
obs_per_id <- dep_data %>%
  count(PID_FAMID) %>%                                     
  count(n, name = "num_id") 

## Plot
plot4 <- ggplot(obs_per_id, aes(x = factor(n), y = num_id)) +
  geom_bar(stat = "identity", fill = "Green") +
  labs(
    title = "(d) Distribution of Observations per Individual",
    x = "Number of observations per Individual",
    y = "Number of Individuals"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )

################ Combined Plot of Multilevel structure

## Combine plots from all four models
combined_hierarchy_plot <- (plot1 | plot2) / (plot3 | plot4)


#### Visualising Distribution of Datapoints across interviewers
##################################################################

# First, create anonmyized interviewer names
interviewer_key <- dep_data %>%
  distinct(Interviewer) %>%
  mutate(interviewer_anon = paste0("Interviewer_", row_number()))

# Next, let's create anonmyized region names
region_key <- dep_data %>%
  distinct(Region) %>%
  mutate(region_anon = paste0("Region_", row_number()))

## Sync these anonymous names to the dataset
dep_data <- dep_data %>%
  left_join(interviewer_key, by = "Interviewer") %>%
  left_join(region_key, by = "Region")

## Count the number of observations per interviewer now
interviewer_count <- dep_data %>%
  count(interviewer_anon, region_anon)

## Now lets plot
interviewer_plot <- ggplot(interviewer_count, aes(x = interviewer_anon, y = n, fill = region_anon)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") + 
  labs(
    title = "Observations Recorded Per Interviewer Across Different Regions",
    x = "Interviewer",
    y = "Number of Observations",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )










# Create anonymized interviewer names
interviewer_key <- depression_data_v2 %>%
  distinct(Interviewer) %>%
  mutate(interviewer_anon = paste0("Interviewer_", row_number()))

# Create anonymized region names
region_key <- depression_data_v2 %>%
  distinct(region) %>%
  mutate(region_anon = paste0("Region_", row_number()))

depression_data_v2 <- depression_data_v2 %>%
  left_join(interviewer_key, by = "Interviewer") %>%
  left_join(region_key, by = "region")




# First, build a simplified crosswalk table
interviewer_links <- depression_data_v2 %>%
  distinct(interviewer_anon, region_anon)  # or use community/household if more granular


ggplot(interviewer_links, aes(x = interviewer_anon, y = region_anon)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.6, color = "steelblue") +
  labs(
    title = "Interviewers Work Across Multiple Regions",
    x = "Interviewer",
    y = "Region"
  ) +
  theme_minimal()







# Make summary dataframe for the multilevel structure of our dataset
multilevel_summary_stats <- depression_data_v2 %>%
  summarise(
    Variable = c("Individuals", "Households", "Communities", "Regions", "Interviewers", "Total Observations contributed by all these levels"),
    N = c(n_distinct(PID_FAMID), n_distinct(UniqueFamID), n_distinct(ComID), n_distinct(region), n_distinct(Interviewer), n()))


######## Summarizing all the predictors used in our analyses
#########################################################

######## Calculating number of males in our dataset
## First, we retain only one observation per individual (since there are repeated observations from certain individuals which might inflate the prevalence of males/females)
unique_individuals <- depression_data_v2 %>% 
  distinct(PID_FAMID, .keep_all = TRUE)

## Now calculate the percentage
percentage_males <- round((sum(unique_individuals$Male)/nrow(unique_individuals))*100, 2)


######## Calculating mean, sd, median, lower and upper quintile of town distance and community size in our dataset
## Retaining one observation per community
unique_communities <- depression_data_v2 %>% 
  distinct(ComID, .keep_all = TRUE)

## Calculating summary stats for town distance
mean_distance <- round(mean(unique_communities$route_distance_town), 2)
sd_distance <- round(sd(unique_communities$route_distance_town), 2)
median_distance <- median(unique_communities$route_distance_town)
lower_quin_distance <- quantile(unique_communities$route_distance_town, 0.25)
upper_quin_distance <- quantile(unique_communities$route_distance_town, 0.75)

## Calculating summary stats for community size
mean_csize <- round(mean(unique_communities$max_community_size), 2)
sd_csize <- round(sd(unique_communities$max_community_size), 2)
median_csize <- median(unique_communities$max_community_size)
lower_quin_csize <- quantile(unique_communities$max_community_size, 0.25)
upper_quin_csize <- quantile(unique_communities$max_community_size, 0.75)



######## Calculating Interview Date Data separately as well
## Doing it in the lines below as the summarise function later on leads to strange numeric outputs, presumably because dplyr & summarise are making some internal transformations on the date format)
mean_date <- as.character(mean(depression_data_v2$InterviewDate))

## We only need the date (not the time for the summary stats, so removing time in the line below)
mean_date_only <- substr(mean_date, 1, 10)
median_date <- as.character(median(depression_data_v2$InterviewDate))
min_date <- as.character(min(depression_data_v2$InterviewDate))
max_date <- as.character(max(depression_data_v2$InterviewDate))
lower_quin_date <- as.character(quantile(depression_data_v2$InterviewDate, 0.25))
upper_quin_date <- as.character(quantile(depression_data_v2$InterviewDate, 0.75))


## Now summarising all predictors in a summary table
summary_stats <- depression_data_v2 %>%
  summarise(
    Variable = c("Depression Sum Score (possible range: 16 - 64)", "Age (in years)", "Sex (% of Males in the sample)", 
                 "Social Conflict Index (possible range: 0 - 6)", "Spanish Fluency (possible range: 0 - 2)",
                 "Route Distance of Community to Town (in kms)", "Maximum Community Size recorded during fieldwork",
                 "Household Size", "Depression Interview Date"),
    Summary_Statistics_Calculated_Over = c("Entire Sample: 2665", "Entire Sample: 2665", "Unique Individuals: 1876", "Entire Sample: 2665", "Entire Sample: 2665",
                                           "Unique Communities: 68", "Unique Communities: 68", "Entire Sample: 2665", "Entire Sample: 2665"),
    Mean = c(round(mean(DepressionScoreM2), 2), round(mean(Age), 2), paste(percentage_males, "% of Males"), round(mean(SocialConflictIndex, na.rm = TRUE), 2), 
             round(mean(SpanishFluency, na.rm = TRUE), 2), mean_distance, mean_csize, round(mean(household_size, na.rm = TRUE), 2), mean_date_only),
    SD = c(round(sd(DepressionScoreM2), 2), round(sd(Age), 2), "-", round(sd(SocialConflictIndex, na.rm = TRUE), 2), round(sd(SpanishFluency, na.rm = TRUE), 2),
           sd_distance, sd_csize, round(sd(household_size, na.rm = TRUE), 2), "-"),
    Median = c(median(DepressionScoreM2), round(median(Age), 2), "-", median(SocialConflictIndex, na.rm = TRUE), median(SpanishFluency, na.rm = TRUE),
               median_distance, median_csize, median(household_size, na.rm = TRUE), median_date),
    Min = c(min(DepressionScoreM2), round(min(Age), 2), "-", min(SocialConflictIndex, na.rm = TRUE), min(SpanishFluency, na.rm = TRUE),
            min(route_distance_town), min(max_community_size), min(household_size, na.rm = TRUE), min_date),
    Max = c(max(DepressionScoreM2), round(max(Age), 2), "-", max(SocialConflictIndex, na.rm = TRUE), max(SpanishFluency, na.rm = TRUE),
            max(route_distance_town), max(max_community_size), max(household_size, na.rm = TRUE), max_date), 
    Q1 = c(quantile(DepressionScoreM2, 0.25), round(quantile(Age, 0.25), 2), "-", quantile(SocialConflictIndex, 0.25, na.rm = TRUE), 
           quantile(SpanishFluency, 0.25, na.rm = TRUE), round(lower_quin_distance, 2), round(lower_quin_csize, 2),
           quantile(household_size, 0.25, na.rm = TRUE), lower_quin_date),
    Q3 = c(quantile(DepressionScoreM2, 0.75), round(quantile(Age, 0.75), 2), "-", quantile(SocialConflictIndex, 0.75, na.rm = TRUE),
           quantile(SpanishFluency, 0.75, na.rm = TRUE), round(upper_quin_distance, 2), round(upper_quin_csize, 2),
           quantile(household_size, 0.75, na.rm = TRUE), upper_quin_date),
    Missingness = c("-", "-", "-", sum(is.na(SocialConflictIndex)), sum(is.na(SpanishFluency)), "-", "-", sum(is.na(household_size)), "-"))



######## Summarizing all the model outputs in one table
#########################################################

## We use the draws from each model (T1, T2, T3, T4) to summarise estimates of each model in a dataframe
## The dataframe is then converted to a flextable

# Conditional R2 for all models which are manually added to the dataframe
mod1_R2 <- bayes_R2(mod1)
mod2_R2 <- bayes_R2(mod2)
mod3_R3 <- bayes_R2(mod3)
mod4_R4 <- bayes_R2(mod4)


## Generating the data frame now
models_summary <- data.frame(Predictors = c("Intercept", "Age", "Sex", "Social Conflict Index (SCI)", 
                                            "Spanish Fluency", "Interview Date (Spline Fixed Effect)", "Interview Date (Spline Smooth Term)",
                                            "Distance to Town", "Maximum Community Size", "Household Size", 
                                            "Residual Variance", 
                                            "Between-Cluster Variance", " ", " ", " ", " ", "ICC", " ", " ", " ", " ", " ", "Conditional R2"), 
                             Estimates_Model_1 = c( round(mean(T1$Intercept),2), round(mean(T1$Age),2), round(mean(T1$Male),2), 
                                                    round(mean(T1$SCI),2), round(mean(T1$SFluency),2), "-", "-", 
                                                    "-", "-", "-", round(mean((T1$sigma_D)^2), 2), 
                                                    paste(round(mean((T1$sd_PID__D_Intercept)^2), 2), " (PID)"),  "-", "-", "-", "-", paste(round(mean(icc_residual), 2), " (Residual)"), paste(round(mean(icc_individual), 2), "(Individual)"),
                                                    "-", "-", "-", "-", round(mod1_R2["R2D", "Estimate"], 2)),
                             CI_Model_1 = c( paste(round(quantile(T1$Intercept, 0.025), 2), "-", round(quantile(T1$Intercept, 0.975), 2)), paste(round(quantile(T1$Age, 0.025), 2), "-", round(quantile(T1$Age, 0.975), 2)),
                                             paste(round(quantile(T1$Male, 0.025), 2), "-", round(quantile(T1$Male, 0.975), 2)), paste(round(quantile(T1$SCI, 0.025), 2), "-", round(quantile(T1$SCI, 0.975), 2)),
                                             paste(round(quantile(T1$SFluency, 0.025), 2), "-", round(quantile(T1$SFluency, 0.975), 2)), "-", "-",
                                             "-", "-", "-",
                                             paste(round(quantile((T1$sigma_D)^2, 0.025), 2), "-", round(quantile((T1$sigma_D)^2, 0.975), 2)), 
                                             paste(round(quantile((T1$sd_PID__D_Intercept)^2, 0.025), 2), "-", round(quantile((T1$sd_PID__D_Intercept)^2, 0.975), 2)), "-", "-", "-", "-",
                                             paste(round(quantile(icc_residual, 0.025), 2), "-", round(quantile(icc_residual, 0.975), 2)), 
                                             paste(round(quantile(icc_individual, 0.025), 2), "-", round(quantile(icc_individual, 0.975), 2)), "-", "-", "-", "-", paste(round(mod1_R2["R2D", "Q2.5"], 2), "-", round(mod1_R2["R2D", "Q97.5"], 2))),
                             
                             
                             Estimates_Model_2 = c( round(mean(T2$Intercept),2), round(mean(T2$Age),2), round(mean(T2$Male),2), round(mean(T2$SCI),2), round(mean(T2$SFluency),2),
                                                    "-", "-",
                                                    "-", "-", "-", round(mean((T2$sigma_D)^2), 2), 
                                                    paste(round(mean((T2$`sd_Region:ComID:UniqueFamID:PID__D_Intercept`)^2), 2), " (PID)"), paste(round(mean((T2$`sd_Region:ComID:UniqueFamID__D_Intercept`)^2), 2), " (FamID)"),
                                                    paste(round(mean((T2$`sd_Region:ComID__D_Intercept`)^2), 2), " (ComID)"), paste(round(mean((T2$sd_Region__D_Intercept)^2), 2), " (RegionID)"), "-",
                                                    paste(round(mean(icc_residual_2), 2), " (Residual)"), paste(round(mean(icc_individual_2), 2), "(Individual)"),
                                                    paste(round(mean(icc_family_2), 2), "(Household)"), paste(round(mean(icc_community_2), 2), "(Community)"),
                                                    paste(round(mean(icc_region_2), 2), "(Region)"), "-", round(mod2_R2["R2D", "Estimate"], 2)), 
                             CI_Model_2 = c( paste(round(quantile(T2$Intercept, 0.025), 2), "-", round(quantile(T2$Intercept, 0.975), 2)), paste(round(quantile(T2$Age, 0.025), 2), "-", round(quantile(T2$Age, 0.975), 2)),
                                             paste(round(quantile(T2$Male, 0.025), 2), "-", round(quantile(T2$Male, 0.975), 2)), paste(round(quantile(T2$SCI, 0.025), 2), "-", round(quantile(T2$SCI, 0.975), 2)), 
                                             paste(round(quantile(T2$SFluency, 0.025), 2), "-", round(quantile(T2$SFluency, 0.975), 2)), "-", "-",
                                             "-", "-", "-",
                                             paste(round(quantile((T2$sigma_D)^2, 0.025), 2), "-", round(quantile((T2$sigma_D)^2, 0.975), 2)), 
                                             paste(round(quantile((T2$`sd_Region:ComID:UniqueFamID:PID__D_Intercept`)^2, 0.025), 2), "-", round(quantile((T2$`sd_Region:ComID:UniqueFamID:PID__D_Intercept`)^2, 0.975), 2)), 
                                             paste(round(quantile((T2$`sd_Region:ComID:UniqueFamID__D_Intercept`)^2, 0.025), 2), "-", round(quantile((T2$`sd_Region:ComID:UniqueFamID__D_Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T2$`sd_Region:ComID__D_Intercept`)^2, 0.025), 2), "-", round(quantile((T2$`sd_Region:ComID__D_Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T2$sd_Region__D_Intercept)^2, 0.025), 2), "-", round(quantile((T2$sd_Region__D_Intercept)^2, 0.975), 2)), "-",
                                             paste(round(quantile(icc_residual_2, 0.025), 2), "-", round(quantile(icc_residual_2, 0.975), 2)), 
                                             paste(round(quantile(icc_individual_2, 0.025), 2), "-", round(quantile(icc_individual_2, 0.975), 2)), 
                                             paste(round(quantile(icc_family_2, 0.025), 2), "-", round(quantile(icc_family_2, 0.975), 2)),
                                             paste(round(quantile(icc_community_2, 0.025), 2), "-", round(quantile(icc_community_2, 0.975), 2)),
                                             paste(round(quantile(icc_region_2, 0.025), 2), "-", round(quantile(icc_region_2, 0.975), 2)), "-", paste( round(mod2_R2["R2D", "Q2.5"],2), "-", round(mod2_R2["R2D", "Q97.5"],2) )),
                            
                              Estimates_Model_3 = c( round(mean(T3$Intercept),2), round(mean(T3$Age),2), round(mean(T3$Male),2), round(mean(T3$SCI),2), 
                                                     round(mean(T3$SFluency),2), round(mean(T3$bs_D_sInterview_Date_1),2), round(mean(T3$sds_D_sInterview_Date_1),2),
                                                     "-", "-", "-", round(mean((T3$sigma_D)^2), 2), 
                                                    paste(round(mean((T3$`sd_Region:ComID:UniqueFamID:PID__D_Intercept`)^2), 2), " (PID)"), paste(round(mean((T3$`sd_Region:ComID:UniqueFamID__D_Intercept`)^2), 2), " (FamID)"),
                                                    paste(round(mean((T3$`sd_Region:ComID__D_Intercept`)^2), 2), " (ComID)"), paste(round(mean((T3$sd_Region__D_Intercept)^2), 2), " (RegionID)"), 
                                                    paste(round(mean((T3$sd_Interviewer__D_Intercept)^2), 2), " (InterviewerID)"),
                                                    paste(round(mean(icc_residual_3), 2), " (Residual)"), paste(round(mean(icc_individual_3), 2), "(Individual)"),
                                                    paste(round(mean(icc_family_3), 2), "(Household)"), paste(round(mean(icc_community_3), 2), "(Community)"),
                                                    paste(round(mean(icc_region_3), 2), "(Region)"), paste(round(mean(icc_interviewer_3), 2), "(Interviewer)"), 
                                                    round(mod3_R3["R2D", "Estimate"], 2)), 
                             CI_Model_3 = c( paste(round(quantile(T3$Intercept, 0.025), 2), "-", round(quantile(T3$Intercept, 0.975), 2)), paste(round(quantile(T3$Age, 0.025), 2), "-", round(quantile(T3$Age, 0.975), 2)),
                                             paste(round(quantile(T3$Male, 0.025), 2), "-", round(quantile(T3$Male, 0.975), 2)), paste(round(quantile(T3$SCI, 0.025), 2), "-", round(quantile(T3$SCI, 0.975), 2)), 
                                             paste(round(quantile(T3$SFluency, 0.025), 2), "-", round(quantile(T3$SFluency, 0.975), 2)), 
                                             paste(round(quantile(T3$bs_D_sInterview_Date_1, 0.025), 2), "-", round(quantile(T3$bs_D_sInterview_Date_1, 0.975), 2)), 
                                             paste(round(quantile(T3$sds_D_sInterview_Date_1, 0.025), 2), "-", round(quantile(T3$sds_D_sInterview_Date_1, 0.975), 2)),
                                             "-",
                                              "-", "-",
                                             paste(round(quantile((T3$sigma_D)^2, 0.025), 2), "-", round(quantile((T3$sigma_D)^2, 0.975), 2)), 
                                             paste(round(quantile((T3$`sd_Region:ComID:UniqueFamID:PID__D_Intercept`)^2, 0.025), 2), "-", round(quantile((T3$`sd_Region:ComID:UniqueFamID:PID__D_Intercept`)^2, 0.975), 2)), 
                                             paste(round(quantile((T3$`sd_Region:ComID:UniqueFamID__D_Intercept`)^2, 0.025), 2), "-", round(quantile((T3$`sd_Region:ComID:UniqueFamID__D_Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T3$`sd_Region:ComID__D_Intercept`)^2, 0.025), 2), "-", round(quantile((T3$`sd_Region:ComID__D_Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T3$sd_Region__D_Intercept)^2, 0.025), 2), "-", round(quantile((T3$sd_Region__D_Intercept)^2, 0.975), 2)), 
                                             paste(round(quantile((T3$sd_Interviewer__D_Intercept)^2, 0.025), 2), "-", round(quantile((T3$sd_Interviewer__D_Intercept)^2, 0.975), 2)),
                                             paste(round(quantile(icc_residual_3, 0.025), 2), "-", round(quantile(icc_residual_3, 0.975), 2)), 
                                             paste(round(quantile(icc_individual_3, 0.025), 2), "-", round(quantile(icc_individual_3, 0.975), 2)), 
                                             paste(round(quantile(icc_family_3, 0.025), 2), "-", round(quantile(icc_family_3, 0.975), 2)),
                                             paste(round(quantile(icc_community_3, 0.025), 2), "-", round(quantile(icc_community_3, 0.975), 2)),
                                             paste(round(quantile(icc_region_3, 0.025), 2), "-", round(quantile(icc_region_3, 0.975), 2)), 
                                             paste(round(quantile(icc_interviewer_3, 0.025), 2), "-", round(quantile(icc_interviewer_3, 0.975), 2)),
                                             paste( round(mod3_R3["R2D", "Q2.5"], 2), "-", round(mod3_R3["R2D", "Q97.5"], 2)  )),
                             
                             Estimates_Model_4 = c( round(mean(T4$Intercept),2), round(mean(T4$Age),2), round(mean(T4$Male),2), round(mean(T4$SCI),2), 
                                                    round(mean(T4$SFluency),2), round(mean(T4$bs_D_sInterview_Date_1),2), round(mean(T4$sds_D_sInterview_Date_1),2),
                                                    round(mean(T4$Town_Distance),2), round(mean(T4$C_Size),2), round(mean(T4$H_Size),2),
                                                    round(mean((T4$sigma_D)^2), 2), 
                                                    paste(round(mean((T4$`sd_Region:ComID:UniqueFamID:PID__D_Intercept`)^2), 2), " (PID)"), paste(round(mean((T4$`sd_Region:ComID:UniqueFamID__D_Intercept`)^2), 2), " (FamID)"),
                                                    paste(round(mean((T4$`sd_Region:ComID__D_Intercept`)^2), 2), " (ComID)"), paste(round(mean((T4$sd_Region__D_Intercept)^2), 2), " (RegionID)"),
                                                    paste(round(mean((T4$sd_Interviewer__D_Intercept)^2), 2), " (InterviewerID)"),
                                                    paste(round(mean(icc_residual_4), 2), " (Residual)"), paste(round(mean(icc_individual_4), 2), "(Individual)"),
                                                    paste(round(mean(icc_family_4), 2), "(Household)"), paste(round(mean(icc_community_4), 2), "(Community)"),
                                                    paste(round(mean(icc_region_4), 2), "(Region)"), paste(round(mean(icc_interviewer_4), 2), "(Interviewer)"), round(mod4_R4["R2D", "Estimate"], 2) ),
                             CI_Model_4 = c( paste(round(quantile(T4$Intercept, 0.025), 2), "-", round(quantile(T4$Intercept, 0.975), 2)), paste(round(quantile(T4$Age, 0.025), 2), "-", round(quantile(T4$Age, 0.975), 2)),
                                             paste(round(quantile(T4$Male, 0.025), 2), "-", round(quantile(T4$Male, 0.975), 2)), paste(round(quantile(T4$SCI, 0.025), 2), "-", round(quantile(T4$SCI, 0.975), 2)), 
                                             paste(round(quantile(T4$SFluency, 0.025), 2), "-", round(quantile(T4$SFluency, 0.975), 2)),
                                             paste(round(quantile(T4$bs_D_sInterview_Date_1, 0.025), 2), "-", round(quantile(T4$bs_D_sInterview_Date_1, 0.975), 2)),
                                             paste(round(quantile(T4$sds_D_sInterview_Date_1, 0.025), 2), "-", round(quantile(T4$sds_D_sInterview_Date_1, 0.975), 2)),
                                             paste(round(quantile(T4$Town_Distance, 0.025), 2), "-", round(quantile(T4$Town_Distance, 0.975), 2)),
                                             paste(round(quantile(T4$C_Size, 0.025), 2), "-", round(quantile(T4$C_Size, 0.975), 2)),
                                             paste(round(quantile(T4$H_Size, 0.025), 2), "-", round(quantile(T4$H_Size, 0.975), 2)),
                                             
                                             paste(round(quantile((T4$sigma_D)^2, 0.025), 2), "-", round(quantile((T4$sigma_D)^2, 0.975), 2)), 
                                             paste(round(quantile((T4$`sd_Region:ComID:UniqueFamID:PID__D_Intercept`)^2, 0.025), 2), "-", round(quantile((T4$`sd_Region:ComID:UniqueFamID:PID__D_Intercept`)^2, 0.975), 2)), 
                                             paste(round(quantile((T4$`sd_Region:ComID:UniqueFamID__D_Intercept`)^2, 0.025), 2), "-", round(quantile((T4$`sd_Region:ComID:UniqueFamID__D_Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T4$`sd_Region:ComID__D_Intercept`)^2, 0.025), 2), "-", round(quantile((T4$`sd_Region:ComID__D_Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T4$sd_Region__D_Intercept)^2, 0.025), 2), "-", round(quantile((T4$sd_Region__D_Intercept)^2, 0.975), 2)),
                                             paste(round(quantile((T4$sd_Interviewer__D_Intercept)^2, 0.025), 2), "-", round(quantile((T4$sd_Interviewer__D_Intercept)^2, 0.975), 2)),
                                             paste(round(quantile(icc_residual_4, 0.025), 2), "-", round(quantile(icc_residual_4, 0.975), 2)), 
                                             paste(round(quantile(icc_individual_4, 0.025), 2), "-", round(quantile(icc_individual_4, 0.975), 2)), 
                                             paste(round(quantile(icc_family_4, 0.025), 2), "-", round(quantile(icc_family_4, 0.975), 2)),
                                             paste(round(quantile(icc_community_4, 0.025), 2), "-", round(quantile(icc_community_4, 0.975), 2)),
                                             paste(round(quantile(icc_region_4, 0.025), 2), "-", round(quantile(icc_region_4, 0.975), 2)), 
                                             paste(round(quantile(icc_interviewer_4, 0.025), 2), "-", round(quantile(icc_interviewer_4, 0.975), 2)), paste( round(mod4_R4["R2D", "Q2.5"],2) , "-", round(mod4_R4["R2D", "Q97.5"],2)  ))
                             
)


####### Summarising outputs for imputation models that were run during model fitting

## Creating data frame for imputed model outputs
imputed_model_summary <- data.frame(Missing_Variables = c("Social Conflict Index", "-", "-", "-",
                                                          "Spanish Fluency", "-", "-", "-",
                                                          "Household Size", "-", "-", "-", "-"),
                                    Predictors = c("Intercept", "Age", "Male", "Residual Variance", 
                                                   "Intercept", "Age", "Sex", "Residual Variance", 
                                                   "Intercept", "Age", "Sex", "Community Size", "Residual Variance"),
                                    
                                    Estimates_Model_1 = c(round(mean(T1$b_SCI_Intercept), 2), 
                                                          round(mean(T1$b_SCI_Age), 2), 
                                                          round(mean(T1$b_SCI_Sex1), 2), 
                                                          round(mean((T1$sigma_SCI)^2), 2),
                                                          round(mean(T1$b_SFluency_Intercept), 2), 
                                                          round(mean(T1$b_SFluency_Age), 2), 
                                                          round(mean(T1$b_SFluency_Sex1), 2), 
                                                          round(mean((T1$sigma_SFluency)^2), 2),
                                                          "-", "-", "-", "-", "-"),
                                    CI_Model_1 = c(paste(round(quantile(T1$b_SCI_Intercept, 0.025), 2), "-", round(quantile(T1$b_SCI_Intercept, 0.975), 2)), 
                                                   paste(round(quantile(T1$b_SCI_Age, 0.025), 2), "-", round(quantile(T1$b_SCI_Age, 0.975), 2)),
                                                   paste(round(quantile(T1$b_SCI_Sex1, 0.025), 2), "-", round(quantile(T1$b_SCI_Sex1, 0.975), 2)),
                                                   paste(round(quantile((T1$sigma_SCI)^2, 0.025), 2), "-", round(quantile((T1$sigma_SCI)^2, 0.975), 2)),
                                                   paste(round(quantile(T1$b_SFluency_Intercept, 0.025), 2), "-", round(quantile(T1$b_SFluency_Intercept, 0.975), 2)),
                                                   paste(round(quantile(T1$b_SFluency_Age, 0.025), 2), "-", round(quantile(T1$b_SFluency_Age, 0.975), 2)),
                                                   paste(round(quantile(T1$b_SFluency_Sex1, 0.025), 2), "-", round(quantile(T1$b_SFluency_Sex1, 0.975), 2)),
                                                   paste(round(quantile((T1$sigma_SFluency)^2, 0.025), 2), "-", round(quantile((T1$sigma_SFluency)^2, 0.975), 2)),
                                                   "-", "-", "-", "-", "-"),
                                     
                                    Estimates_Model_2 = c(round(mean(T2$b_SCI_Intercept), 2), 
                                                          round(mean(T2$b_SCI_Age), 2), 
                                                          round(mean(T2$b_SCI_Sex1), 2), 
                                                          round(mean((T2$sigma_SCI)^2), 2),
                                                          round(mean(T2$b_SFluency_Intercept), 2), 
                                                          round(mean(T2$b_SFluency_Age), 2), 
                                                          round(mean(T2$b_SFluency_Sex1), 2), 
                                                          round(mean((T2$sigma_SFluency)^2), 2),
                                                          "-", "-", "-", "-", "-"),
                                    CI_Model_2 = c(paste(round(quantile(T2$b_SCI_Intercept, 0.025), 2), "-", round(quantile(T2$b_SCI_Intercept, 0.975), 2)), 
                                                   paste(round(quantile(T2$b_SCI_Age, 0.025), 2), "-", round(quantile(T2$b_SCI_Age, 0.975), 2)),
                                                   paste(round(quantile(T2$b_SCI_Sex1, 0.025), 2), "-", round(quantile(T2$b_SCI_Sex1, 0.975), 2)),
                                                   paste(round(quantile((T2$sigma_SCI)^2, 0.025), 2), "-", round(quantile((T2$sigma_SCI)^2, 0.975), 2)),
                                                   paste(round(quantile(T2$b_SFluency_Intercept, 0.025), 2), "-", round(quantile(T2$b_SFluency_Intercept, 0.975), 2)),
                                                   paste(round(quantile(T2$b_SFluency_Age, 0.025), 2), "-", round(quantile(T2$b_SFluency_Age, 0.975), 2)),
                                                   paste(round(quantile(T2$b_SFluency_Sex1, 0.025), 2), "-", round(quantile(T2$b_SFluency_Sex1, 0.975), 2)),
                                                   paste(round(quantile((T2$sigma_SFluency)^2, 0.025), 2), "-", round(quantile((T2$sigma_SFluency)^2, 0.975), 2)),
                                                   "-", "-", "-", "-", "-"),
                                    
                                    Estimates_Model_3 = c(round(mean(T3$b_SCI_Intercept), 2), 
                                                          round(mean(T3$b_SCI_Age), 2), 
                                                          round(mean(T3$b_SCI_Sex1), 2), 
                                                          round(mean((T3$sigma_SCI)^2), 2),
                                                          round(mean(T3$b_SFluency_Intercept), 2),
                                                          round(mean(T3$b_SFluency_Age), 2), 
                                                          round(mean(T3$b_SFluency_Sex1), 2), 
                                                          round(mean((T3$sigma_SFluency)^2), 2),
                                                          "-", "-", "-", "-", "-"),
                                    CI_Model_3 = c(paste(round(quantile(T3$b_SCI_Intercept, 0.025), 2), "-", round(quantile(T3$b_SCI_Intercept, 0.975), 2)), 
                                                   paste(round(quantile(T3$b_SCI_Age, 0.025), 2), "-", round(quantile(T3$b_SCI_Age, 0.975), 2)),
                                                   paste(round(quantile(T3$b_SCI_Sex1, 0.025), 2), "-", round(quantile(T3$b_SCI_Sex1, 0.975), 2)),
                                                   paste(round(quantile((T3$sigma_SCI)^2, 0.025), 2), "-", round(quantile((T3$sigma_SCI)^2, 0.975), 2)),
                                                   paste(round(quantile(T3$b_SFluency_Intercept, 0.025), 2), "-", round(quantile(T3$b_SFluency_Intercept, 0.975), 2)),
                                                   paste(round(quantile(T3$b_SFluency_Age, 0.025), 2), "-", round(quantile(T3$b_SFluency_Age, 0.975), 2)),
                                                   paste(round(quantile(T3$b_SFluency_Sex1, 0.025), 2), "-", round(quantile(T3$b_SFluency_Sex1, 0.975), 2)),
                                                   paste(round(quantile((T3$sigma_SFluency)^2, 0.025), 2), "-", round(quantile((T3$sigma_SFluency)^2, 0.975), 2)),
                                                   "-", "-", "-", "-", "-"),
                                    
                                    Estimates_Model_4 = c(round(mean(T4$b_SCI_Intercept), 2), 
                                                          round(mean(T4$b_SCI_Age), 2), 
                                                          round(mean(T4$b_SCI_Sex1), 2), 
                                                          round(mean((T4$sigma_SCI)^2), 2),
                                                          round(mean(T4$b_SFluency_Intercept), 2), 
                                                          round(mean(T4$b_SFluency_Age), 2), 
                                                          round(mean(T4$b_SFluency_Sex1), 2), 
                                                          round(mean((T4$sigma_SFluency)^2), 2),
                                                          round(mean(T4$b_HHSize_Intercept), 2), 
                                                          round(mean(T4$b_HHSize_Age), 2), 
                                                          round(mean(T4$b_HHSize_Sex1), 2), 
                                                          round(mean(T4$b_HHSize_Com_Size), 2), 
                                                          round(mean((T4$sigma_HHSize)^2), 2)
                                                          ),
                                    CI_Model_4 = c(paste(round(quantile(T4$b_SCI_Intercept, 0.025), 2), "-", round(quantile(T4$b_SCI_Intercept, 0.975), 2)), 
                                                   paste(round(quantile(T4$b_SCI_Age, 0.025), 2), "-", round(quantile(T4$b_SCI_Age, 0.975), 2)),
                                                   paste(round(quantile(T4$b_SCI_Sex1, 0.025), 2), "-", round(quantile(T4$b_SCI_Sex1, 0.975), 2)),
                                                   paste(round(quantile((T4$sigma_SCI)^2, 0.025), 2), "-", round(quantile((T4$sigma_SCI)^2, 0.975), 2)),
                                                   paste(round(quantile(T4$b_SFluency_Intercept, 0.025), 2), "-", round(quantile(T4$b_SFluency_Intercept, 0.975), 2)),
                                                   paste(round(quantile(T4$b_SFluency_Age, 0.025), 2), "-", round(quantile(T4$b_SFluency_Age, 0.975), 2)),
                                                   paste(round(quantile(T4$b_SFluency_Sex1, 0.025), 2), "-", round(quantile(T4$b_SFluency_Sex1, 0.975), 2)),
                                                   paste(round(quantile((T4$sigma_SFluency)^2, 0.025), 2), "-", round(quantile((T4$sigma_SFluency)^2, 0.975), 2)),
                                                   paste(round(quantile(T4$b_HHSize_Intercept, 0.025), 2), "-", round(quantile(T4$b_HHSize_Intercept, 0.975), 2)),
                                                   paste(round(quantile(T4$b_HHSize_Age, 0.025), 2), "-", round(quantile(T4$b_HHSize_Age, 0.975), 2)),
                                                   paste(round(quantile(T4$b_HHSize_Sex1, 0.025), 2), "-", round(quantile(T4$b_HHSize_Sex1, 0.975), 2)),
                                                   paste(round(quantile(T4$b_HHSize_Com_Size, 0.025), 2), "-", round(quantile(T4$b_HHSize_Com_Size, 0.975), 2)),
                                                   paste(round(quantile((T4$sigma_HHSize)^2, 0.025), 2), "-", round(quantile((T4$sigma_HHSize)^2, 0.975), 2)) 
                                                   ))
                                    
                                  
                                  

#################### Generating the summary tables that are used in the manuscript
##################################################################################
##################################################################################

################### First the descriptive summary table for all the predictors
summary_stats_ft <- flextable(summary_stats)

# Bold the column headers, apply a grey backdrop to the first column and autofit the table
summary_stats_ft <- bold(summary_stats_ft, part = "header")
summary_stats_ft <- bg(summary_stats_ft, j = 1, bg = "#D3D3D3", part = "body")
summary_stats_ft <- autofit(summary_stats_ft)

## Can copy the table directly from here on to word
summary_stats_ft


################### Now the summary table of the multilevel structure of our data table
multilevel_summary_stats_ft <- flextable(multilevel_summary_stats)

# Bold the column headers, apply a grey backdrop to the first column and autofit the table
multilevel_summary_stats_ft <- bold(multilevel_summary_stats_ft, part = "header")
multilevel_summary_stats_ft <- bg(multilevel_summary_stats_ft, j = 1, bg = "#D3D3D3", part = "body")
multilevel_summary_stats_ft <- autofit(multilevel_summary_stats_ft)

## Can copy the table directly from here on to word
multilevel_summary_stats_ft



################## Now the model summary output

## Converting to a flextable format
models_summary_ft <- flextable(models_summary)

## Adding alternative colours to distinguish different model outputs
models_summary_ft <- bg(models_summary_ft, j = c(4,5,8,9), bg = "#D3D3D3", part = "body")
models_summary_ft <- bg(models_summary_ft, j = c(2,3,6,7), bg = "beige", part = "body")
models_summary_ft <- bold(models_summary_ft, part = "header")
models_summary_ft <- autofit(models_summary_ft)

## Copy-paste the output on word
models_summary_ft


################## Now the imputed models output
imp_model_summary_ft <- flextable(imputed_model_summary)

## Adding alternative colours to distinguish different model outputs
imp_model_summary_ft <- bg(imp_model_summary_ft, j = c(5,6,9,10), bg = "#D3D3D3", part = "body")
imp_model_summary_ft <- bg(imp_model_summary_ft, j = c(3,4,7,8), bg = "beige", part = "body")
imp_model_summary_ft <- bold(imp_model_summary_ft, part = "header")
imp_model_summary_ft <- autofit(imp_model_summary_ft)

## Copy-paste the output on word
imp_model_summary_ft

