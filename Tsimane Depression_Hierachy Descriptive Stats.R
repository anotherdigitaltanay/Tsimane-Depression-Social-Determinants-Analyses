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
library(ggplot2)
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

## Adding variable labels to the columns (which would allow for a data dictionary), so that it is comprehensible for potential future users willing to use this dataset
## Some columns don't have a label as I didn't have further information on them -- should be available with Tsimane project data cleaning experts as those columns seem pertinent for data wrangling 
## Fortunately, they can't be used for any analytical interest (e.g. Vuelta, fecha)

## Adding the variable labels
var_label(depression_data) <- list(
  PID = "Unique Person Identifier used to anonymize Tsimane Individuals",
  ComID = "Unique Community ID used to anonymise Tsimane communities",
  Year = "Year in which Depression Interview was conducted",
  midpid_older = "Older PIDs used by the Tsimane project. Shouldn't be touched at all as there are gross errors here",
  Male = "1 indicates Male, 0 indicates Female",
  FamID = "Unique Family ID to anonymize families in each Tsimane community",
  InterviewDate = "Exact Date on which the Interview was conducted",
  Coder = "Person who recorded this data on the database",
  X1Sad = "Depression Scale Item 1 regarding Sadness over past month. Likert Scale between 1 - 4 for all 18 items below",
  X2CryEasily = "Depression Scale Item regarding Crying easily over past month",
  X3SelfCritical = "Depression Scale Item regarding being self critical over past month",
  X4LifeNotValuable = "Depression Scale Item regarding being not finding life vauluable over past month",
  X5Useless = "Depression Scale Item regarding finding oneself useless over past month", 
  X6NoInterest = "Depression Scale Item regarding having no interest/motivation over past month", 
  X7Tired = "Depression Scale Item regarding feeling tired over past month", 
  X8CantConcentrate = "Depression Scale Item regarding not being able to concentrate over past month", 
  X9Nervous = "Depression Scale Item regarding being nervous over past month", 
  X10Paranoid = "Depression Scale Item regarding feeling paranoid over past month", 
  X11CantSleep = "Depression Scale Item regarding not being able to sleep over past month", 
  X12CantEat = "Depression Scale Item regarding not being able to eat over past month", 
  X13CantFuck = "Depression Scale Item regarding being able to have sex over past month", 
  X14Pessimistic = "Depression Scale Item regarding being pessimistic over past month", 
  X15Indecision = "Depression Scale Item regarding being indecisive over past month", 
  X16PunishMe = "Depression Scale Item regarding having feelings of self-punishment over past month", 
  X17Irritable = "Depression Scale Item regarding being irritable over past month", 
  X18HeavyThoughts = "Depression Scale Item regarding having heavy thoughts over past month", 
  DepressionScore = "Depression Sum Score obtained by summing the 18 items above",
  DepressionScoreM2 = "Depression Sum Score obtained by summing the 16 items above except irritability and indecision items",
  P4WhatProblemsBotherYouMostInLife = "Open ended responses regarding what problems individuals faced in their lives at the time of the depression interview",
  P41NoFood = "1 denotes problem of no food, 0 denotes absence",
  P48Illness = "1 denotes problem of illness, 0 denotes absence", 
  P42SpousalConflict = "1 denotes problem of spousal conflict, 0 denotes absence", 
  P49KidConflict = "1 denotes problem of kid conflict, 0 denotes absence", 
  P43ConflictWithRelative = "1 denotes problem of relative/kin conflict, 0 denotes absence", 
  P410Gossip = "1 denotes problem of gossip about them, 0 denotes absence", 
  P44ConflictWithOther = "1 denotes problem of conflict with other people, 0 denotes absence", 
  P411NoMoney = "1 denotes problem of no money, 0 denotes absence", 
  P45NoHelp = "1 denotes problem of no help, 0 denotes absence", 
  P412Witchcraft = "1 denotes problem of witchcraft, 0 denotes absence", 
  P46Death = "1 denotes problem of death, 0 denotes absence",
  P413Debt = "1 denotes problem of debt, 0 denotes absence", 
  P47NoSpouseOrKids = "1 denotes absence of spouse or kids, 0 denotes absence of this problem", 
  P414Other = "Open ended prompt to record other problems", 
  P6WhatWouldYouChange = "What would they like to change in their life at the time of the interview -- Open ended prompt",
  Ev1Smiled.or.Laughed = "External validity check on scale of 1-4: How much did they smile during the depression interview",
  Ev2Conversational = "External Validity check on scale of 1-4: How much were they conversational during this interview",
  notas = "Additional notes recorded by the interviewer",
  notas.2 = "Additional notes recorded by the interviewer",
  vid = "Unique visit id each time someone visited to interview a participant",
  SocialConflictIndex = "Sum score obtained after summing the following binary items: P42SpousalConflict + P49KidConflict + P43ConflictWithRelative + P44ConflictWithOther + P410Gossip + P412Witchcraft", 
  DepScore = "Sanity check variable created after manually summing all 16 items excluding irritability and indecision to see whether this equates to DepressionScoreM2 column", 
  Cleaning_Notes_2024 = "Data wrangling notes left where some subjective decisions were taken to assign the interviewer in certain periods after consulting with the anthropologists who verified the interviewers who conducted the depression interviews", 
  Interviewer = "Interviewer who conducted the depression interview", 
  Interviewer_Start_Date = "Earliest date at which the pertinent interviewer started recording depression interviews", 
  route_distance_SB = "Distance to San Borja, a market town", 
  route_distance_town = "Distance to San Borja or another market town, whichever is closer", 
  region = "Region of the area/forest in which the community was located",
  Missing_FamID_Cleaning_Notes = "Wrangling notes left wherever FamID was originally missing in the dataset. It mentions where the missing FamID was extracted from", 
  Diff_DepDate_and_CensusFamIDDate = "Difference in days between the depression interview date and the date on which the Missing FamID was recorded by the project", 
  Education_Data_Date = "Date on which Spanish Fluency (spoken) was recorded", 
  SpanishFluency = "Degree to which individual was fluent in Spanish (0 = None, 1 = Moderate, 2 = Fluent)", 
  Diff_DepDate_and_EducationDataDate = "Difference in days between the depression interview date and the date on which Spanish Fluency was recorded by the project", 
  BMI_Date = "Date on which BMI was recorded", 
  BMI = "Body Mass Index of the Individual", 
  Diff_DepDate_and_BMIDataDate = "Difference in days between the depression interview date and the date on which BMI was recorded by the project", 
  Subsistence_Date = "Date on which Subsistence Involvement Variables were recorded", 
  SI_CanHunt = "Binary item concerning routine involvement in hunting (1 = yes, 0 = no for related items below)", 
  SI_CanChopBigTrees = "Binary item concerning routine involvement in chopping big trees", 
  SI_WalkingAllDay = "Binary item concerning routine involvement in walking all day", 
  SI_LiftingHeavyLoad = "Binary item concerning routine involvement in lifting heavy loads", 
  SI_WeavingBags = "Binary item concerning routine involvement in weaving bags",
  SI_WeavingMats = "Binary item concerning routine involvement in Weaving mats", 
  Diff_DepDate_and_SIDataDate = "Difference in days between the depression interview date and the date on which Subistence Involvement variables were recorded by the project", 
  DisabilityScore_Date = "Date on which Disability score was recorded", 
  DisabilityScore = "Sum of 11 items (present in Stieglitz et al., 2014) measuring how physically fit a person is", 
  Diff_DepDate_and_DisabilityDataDate = "Difference in days between the depression interview date and the date on which Disability score was recorded by the project", 
  max_community_size = "Maximum community size recorded during the interview period of the depression project (between 2006 - 2015)",
  Missing_Com_Size_Cleaning_Notes = "Wrangling notes around allocating max community size, if required", 
  Household_size_date = "Date on which household size was recorded",
  household_size = "Size of a particular family or household",
  Diff_DepDate_and_HouseholdSizeDate = "Difference in days between the depression interview date and the date on which Household size was recorded by the project", 
  Date_of_Birth = "Date of birth", 
  Age = "Age of the individual"
) 

## View data dictionary here -- can nicely see the variable description and missingness of each variable
look_for(depression_data)

## Examine missingness of pertinent variables in more detail

# 1. DepressionScore is missing for 110 observations
## seems to be missing across diverse ages, years, sex and across 38 communities
x <- depression_data %>% filter(is.na(DepressionScoreM2))
hist(x$Age)
hist(x$Year)
x %>% count(Male)
x %>% count(ComID)

# 2. Spanish Fluency is missing for 516 observations 
## missing across diverse ages and sexes, but more towards end of interview years and for 62 communities
x <- depression_data %>% filter(is.na(SpanishFluency))
hist(x$Age)
hist(x$Year)
x %>% count(Male)
x %>% count(ComID)
hist(x$DepressionScoreM2)

# 3. BMI is missing for 1226 observations (nearly half of the dataset)
## missing across interview years for both sexes across 67 communities, but a bit left skewed in terms of age, 
x <- depression_data %>% filter(is.na(BMI))
hist(x$Age)
hist(x$Year)
x %>% count(Male)
x %>% count(ComID)
hist(x$DepressionScoreM2)

# 4. Social Conflict Index is missing for 192 observations
## Missing across ages and sex, but only in 28 communities and majorly missing in the year 2006 (also slightly for more depressed people??)
x <- depression_data %>% filter(is.na(SocialConflictIndex))
hist(x$Age)
hist(x$Year)
x %>% count(Male)
x %>% count(ComID)
hist(x$DepressionScoreM2)

## 5. Subsistence Involvement Items are missing for 1226 observations (nearly half of the dataset)
## missing across interview years for both sexes across 67 communities, but a bit left skewed in terms of age,
x <- depression_data %>% filter(is.na(Diff_DepDate_and_SIDataDate))
hist(x$Age)
hist(x$Year)
x %>% count(Male)
x %>% count(ComID)
hist(x$DepressionScoreM2)

## 6. Disability Scores are missing for 2000 observations (more than half of the dataset)
## Seems more missing for younger ages, towards the later half of the interview years across 67 communities
x <- depression_data %>% filter(is.na(DisabilityScore))
hist(x$Age)
hist(x$Year)
x %>% count(Male)
x %>% count(ComID)
hist(x$DepressionScoreM2)


## 7. Household Size Data are missing for 55 observations 
## Across various ages, but towards latter half of the year across 30 communities
x <- depression_data %>% filter(is.na(household_size))
hist(x$Age)
hist(x$Year)
x %>% count(Male)
x %>% count(ComID)
hist(x$DepressionScoreM2)

## The following variables (Disability Score, BMI, all Subsistence Involvement Items) seem to be missing for nearly half/more of the dataset and doesn't seem to be missing at random
## Hence, we won't impute values for these columns and leave them out of our current analytical pipeline
## Depression sum scores also have some missing values -- these will not be imputed and shall be excluded.
## Only the predictors (Spanish Fluency, Social Conflict Index and Household size) shall be imputed.
## Implementing these changes below

## Selecting columns of analytical interest
depression_data_v2 <- depression_data %>%
  select(DepressionScoreM2, PID, FamID, ComID, region, Interviewer, InterviewDate, Age, Male, SocialConflictIndex, SpanishFluency, route_distance_town, max_community_size, household_size, DisabilityScore)

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

## Computing correlation between age and disability score that can be reported in discussion section of main manuscript
## Vital to talk about the age effect found further in our models
cor(depression_data_v2$Age, depression_data_v2$DisabilityScore, use = "complete.obs")


################### Summary Tables and Plots

#### Summarizing the multilevel structure of the dataset in a summary stats table
#################################################################################

# Make summary dataframe for the multilevel structure of our dataset
multilevel_summary_stats <- depression_data_v2 %>%
  summarise(
    Variable = c("Regions", "Communities", "Households", "Individuals", "Total Observations contributed by all these levels"),
    N = c(n_distinct(region), n_distinct(ComID), n_distinct(UniqueFamID), n_distinct(PID_FAMID), n()))


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
com_per_region <- depression_data_v2 %>%
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
hh_per_com <- depression_data_v2 %>%
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
id_per_hh <- depression_data_v2 %>%
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
obs_per_id <- depression_data_v2 %>%
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
interviewer_key <- depression_data_v2 %>%
  distinct(Interviewer) %>%
  mutate(interviewer_anon = paste0("Interviewer_", row_number()))

# Next, let's create anonmyized region names
region_key <- depression_data_v2 %>%
  distinct(region) %>%
  mutate(region_anon = paste0("Region_", row_number()))

## Sync these anonymous names to the dataset
depression_data_v2 <- depression_data_v2 %>%
  left_join(interviewer_key, by = "Interviewer") %>%
  left_join(region_key, by = "region")

## Count the number of observations per interviewer now
interviewer_count <- depression_data_v2 %>%
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

