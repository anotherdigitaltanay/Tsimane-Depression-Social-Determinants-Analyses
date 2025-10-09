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
dep_data_v1 <- dep_data 

## Setting appropriate variable types
dep_data_v1$InterviewDate <- as.POSIXct(dep_data_v1$InterviewDate, tz = "UTC")







################### Summary Tables and Plots

#### Summarizing the multilevel structure of the dataset in a summary stats table
#################################################################################

# Make summary dataframe for the multilevel structure of our dataset
multilevel_summary_stats <- dep_data %>%
  summarise(
    Variable = c("Regions", "Communities", "Households", "Individuals", "Total Observations"),
    N = c(n_distinct(region), n_distinct(ComID), n_distinct(UniqueFamID), n_distinct(PID), n() )) %>%
  mutate(Variable = factor(Variable, levels = c("Regions", "Communities", "Households", "Individuals", "Total Observations")))



# Basic vertical bar plot
simple_summary <- ggplot(multilevel_summary_stats, aes(x = Variable, y = N)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = N), vjust = -0.5, size = 4) +
  labs(
    title = "(e) Multilevel Data Structure Summary",
    x = "Data Grouping Level",
    y = "Number of Groups at each Level"
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



#### Visualising the multilevel structure of the dataset via plots
##################################################################

## First we count how many communities are present in each region
com_per_region <- dep_data %>%
  distinct(region, ComID) %>%                           # Outlines the communities present in each region
  count(region) %>%                                     # Now precisely counting how many communities present in each region (in a summary estimate)
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
  count(PID) %>%                                     
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


#### Visualising Distribution of Datapoints across interviewers
##################################################################

# First, create anonmyized interviewer names
interviewer_key <- dep_data %>%
  distinct(Interviewer) %>%
  mutate(interviewer_anon = paste0("Interviewer_", row_number()))

# Next, let's create anonmyized region names
region_key <- dep_data %>%
  distinct(region) %>%
  mutate(region_anon = paste0("Region_", row_number()))

## Sync these anonymous names to the dataset
dep_data <- dep_data %>%
  left_join(interviewer_key, by = "Interviewer") %>%
  left_join(region_key, by = "region")

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


################ Combined Plot of Multilevel structure

## Combine plots from all four models
combined_hierarchy_plot <- (plot1 | plot2) / (plot3 | plot4) / (simple_summary | interviewer_plot)




######## Summarizing all the predictors used in our analyses
#########################################################

######## Calculating number of males in our dataset
## First, we retain only one observation per individual (since there are repeated observations from certain individuals which might inflate the prevalence of males/females)
unique_individuals <- dep_data_v1 %>% 
  distinct(PID, .keep_all = TRUE)

## Now calculate the percentage
percentage_males <- round((sum(unique_individuals$Male)/nrow(unique_individuals))*100, 2)


######## Calculating mean, sd, median, lower and upper quintile of town distance and community size in our dataset
## Retaining one observation per community
unique_communities <- dep_data_v1 %>% 
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
mean_date <- as.character(mean(dep_data_v1$InterviewDate))

## We only need the date (not the time for the summary stats, so removing time in the line below)
mean_date_only <- substr(mean_date, 1, 10)
median_date <- as.character(median(dep_data_v1$InterviewDate))
min_date <- as.character(min(dep_data_v1$InterviewDate))
max_date <- as.character(max(dep_data_v1$InterviewDate))
lower_quin_date <- as.character(quantile(dep_data_v1$InterviewDate, 0.25))
upper_quin_date <- as.character(quantile(dep_data_v1$InterviewDate, 0.75))


## Now summarising all predictors in a summary table
summary_stats <- dep_data_v1 %>%
  summarise(
    Variable = c("Depression Sum Score (possible range: 16 - 64)", "Age (in years)", "Sex (% of Males in the sample)", 
                 "Social Conflict Index (possible range: 0 - 6)", "Spanish Fluency (possible range: 0 - 2)",
                 "Route Distance of Community to Town (in kms)", "Maximum Community Size recorded during fieldwork",
                 "Household Size", "Depression Interview Date"),
    Summary_Statistics_Calculated_Over = c("Entire Sample: 2002", "Entire Sample: 2002", "Unique Individuals: 1390", "Entire Sample: 2002", "Entire Sample: 2002",
                                           "Unique Communities: 62", "Unique Communities: 62", "Entire Sample: 2002", "Entire Sample: 2002"),
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


