############### This script summarises the multilevel structure of the Tsimane depression dataset       #################
############### It generates the pertinent summaries and plots on the entire dataset (with missing observations)               #################


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
library(patchwork)
library(knitr)
library(tidybayes)
library(broom.mixed)
library(bayesplot)
library(flextable) 
library(sjPlot)
library(labelled)
library(mice)

## Setting seed for reproducibility
set.seed(7777)


# Running this so that one can see upto 10000 rows of printed results
options(max.print = 10000)

## Loading the cleaned dataset
depression_data <- read.csv("Cleaned_Depression_Dataset.csv")


## Reducing the dataset even further now by selecting the absolutely necessary columns
depression_data_v2 <- depression_data %>%
  select(DepressionScoreM2, PID, FamID, ComID, region, Interviewer, InterviewDate, Age, Male, SocialConflictIndex, SpanishFluency, route_distance_town, max_community_size, household_size)

## Now exclude observations having missing depression scores
depression_data_v2 <- depression_data_v2 %>%
  filter(!is.na(DepressionScoreM2))

## Examining missingness of other variables now. 
## We have 3 observations missing FamIDs, 188 missing Social Conflict Index, 490 missing Spanish Fluency and 51 missing household_size
lookfor(depression_data_v2)

## Assigning random FamIDs to these 3 observations so that we don't lose data on family-level

## Identifying rows in the main depression dataset where FamID is missing
## 3 rows identified
na_rows <- which(is.na(depression_data_v2$FamID))

## Randomly generate 3 unique 7-digit numbers that are assigned as FamIDs
## 7-digit so that it becomes pretty obvious that these famIDs were assigned to them
random_famIDs <- sample(1000000:9999999, 3)

## Assigning these FamIDs now
depression_data_v2$FamID[na_rows] <- random_famIDs

## Worked
lookfor(depression_data_v2)

## Removing one observation below 18 years of age as Tsimane project heads remarked that this is likely due to measurement error
## After excluding this, missingness pattern changes. Remove 1 observation from patterns observed in lines 212
depression_data_v2 <- depression_data_v2 %>% 
  filter(Age > 18)


## Converting InterviewDate to POSXIct format so that standardising it using a numeric format becomes easier
depression_data_v2$InterviewDate <- as.POSIXct(depression_data_v2$InterviewDate, tz = "UTC")

## Doing some quick summary stats
## 68 communities, 10 regions, 8 interviewers, more females than males, 1803 individuals
depression_data_v2 %>% count(ComID)
depression_data_v2 %>% count(region)
depression_data_v2 %>% count(Interviewer)
hist(depression_data_v2$Age)
depression_data_v2 %>% count(Male)
depression_data_v2 %>% count(PID)

## As FamIDs can be shared between communities, we create a new column concatenating ComIDs & FamIDs so that each community has an unique FamID
depression_data_v2 <- depression_data_v2 %>% mutate(UniqueFamID = paste(ComID, FamID, sep='_'))

## Giving each individual a unique PID-FamID-ComID combination such that the same individuals across different families at different timepoints are treated as distinct
depression_data_v2 <- depression_data_v2 %>%
  mutate(PID_FAMID = paste(PID, UniqueFamID, sep = "_"))

## 1098 families/households -- 1876 individuals
depression_data_v2 %>% count(UniqueFamID)
depression_data_v2 %>% count(PID_FAMID)



################### Summary Tables and Plots

######################################### Defining a global plot aesthetics theme that will be applied to all plots
###########################################################################################################################
###########################################################################################################################

# Defining a constant size for geom_text (which uses millimeters, not points)
manuscript_text_size <- 3.5 

# Setting the plot aesthetics
custom_theme <- theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.line = element_line(color = "black", size = 0.5), # Adds distinct axis lines
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom", 
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() 
  )

# Set it globally! All ggplot/mcmc_areas objects will now use this by default.
theme_set(custom_theme)


#### Summarizing the multilevel structure of the dataset in a summary stats table
#################################################################################

# Make summary dataframe for the multilevel structure of our dataset
multilevel_summary_stats <- depression_data_v2 %>%
  summarise(
    Variable = c("Communities", "Households", "Individuals", "Total Observations"),
    N = c(n_distinct(ComID), n_distinct(UniqueFamID), n_distinct(PID), n() )) %>%
  mutate(Variable = factor(Variable, levels = c("Communities", "Households", "Individuals", "Total Observations")))



# Basic vertical bar plot
simple_summary <- ggplot(multilevel_summary_stats, aes(x = Variable, y = N)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = N), vjust = -0.5, size = 4) +
  labs(
    title = "(d) Multilevel Data Structure Summary",
    x = "Data Grouping Level",
    y = "Number of Groups at each Level"
  ) 


#### Visualising the multilevel structure of the dataset via plots
##################################################################


##################################################################
## First we count how many households are present in each communities
hh_per_com <- depression_data_v2 %>%
  distinct(ComID, UniqueFamID) %>%                           
  count(ComID) %>%                                     
  count(n, name = "num_coms") 

## Plot
plot1 <- ggplot(hh_per_com, aes(x = factor(n), y = num_coms)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(
    title = "(a) Distribution of Households per Community",
    x = "Number of Households in Community",
    y = "Number of Communities"
  ) +
  scale_y_continuous(breaks = 0:max(hh_per_com$num_coms)) 

  

##################################################################
## Now we count how many individuals are present in each household
id_per_hh <- depression_data_v2 %>%
  distinct(UniqueFamID, PID) %>%                           
  count(UniqueFamID) %>%                                     
  count(n, name = "num_hh") 

## Plot
plot2 <- ggplot(id_per_hh, aes(x = factor(n), y = num_hh)) +
  geom_bar(stat = "identity", fill = "Blue") +
  labs(
    title = " (b) Distribution of Individuals per Household",
    x = "Number of Individuals in Household",
    y = "Number of Households"
  ) 



##################################################################
## Now we count how many observations are present for each individual
obs_per_id <- depression_data_v2 %>%
  count(PID) %>%                                     
  count(n, name = "num_id") 

## Plot
plot3 <- ggplot(obs_per_id, aes(x = factor(n), y = num_id)) +
  geom_bar(stat = "identity", fill = "Green") +
  labs(
    title = "(c) Distribution of Observations per Individual",
    x = "Number of observations per Individual",
    y = "Number of Individuals"
  ) 



################ Combined Plot of Multilevel structure

## Combine plots from all four models
combined_hierarchy_plot <- (plot1 | plot2) / (plot3 | simple_summary)

## Saving these combined plots at a resolution that is readable on a page
ggsave(filename = "Multilevel_SummaryStats.png", 
       plot = combined_hierarchy_plot, 
       width = 14,        # Canvas width in inches
       height = 12,       # Canvas height in inches
       dpi = 300,         # Minimum standard resolution for academic print
       bg = "white")

######## Summarizing all the predictors used in our analyses
#########################################################

######## Calculating number of males in our dataset
## First, we retain only one observation per individual (since there are repeated observations from certain individuals which might inflate the prevalence of males/females)
unique_individuals <- depression_data_v2 %>% 
  distinct(PID, .keep_all = TRUE)

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
    Summary_Statistics_Calculated_Over = c("Entire Sample: 2665", "Entire Sample: 2665", "Unique Individuals: 1803", "Entire Sample: 2665", "Entire Sample: 2665",
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




