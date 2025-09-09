############### This script contains the Tsimane social determinants depression analyses for the EMPH MAPPING Special Issue Paper       #################
############### The analyses here are conducted only on the imputed dataset (i.e. imputation during model fitting)                 #################
############### The script also generates a cleaned csv file that will be used for the complete case analyses (i.e. excluding missing data) in another script #################
############### Summary Tables and Plots that are used in the manuscript are generated towards the end of the script           #################

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
library(viridis)


## Setting seed for reproducibility
set.seed(77832)


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
  select(DepressionScoreM2, 
         X1Sad, X2CryEasily, X3SelfCritical, X4LifeNotValuable, X5Useless, X6NoInterest, X7Tired, X8CantConcentrate, X9Nervous, X10Paranoid, X11CantSleep, X12CantEat, X13CantFuck, X14Pessimistic, X16PunishMe, X18HeavyThoughts,
         PID, FamID, ComID, region, Interviewer, InterviewDate, Age, Male, SocialConflictIndex, SpanishFluency, route_distance_town, max_community_size, household_size, DisabilityScore)


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





############################# Data Analysis ######################################
##################################################################################
##################################################################################
##################################################################################


## Loading the pertinent dataset and making necessary transformations
dep_data <- depression_data_v2

## Renaming for making model formulas more readable
dep_data <- dep_data %>%
  rename("D" = "DepressionScoreM2", 
         "D1_Sad" = "X1Sad", 
         "D2_Cry" = "X2CryEasily", 
         "D3_SelfCritical" = "X3SelfCritical", 
         "D4_LifeNotValuable" = "X4LifeNotValuable", 
         "D5_Useless" = "X5Useless", 
         "D6_NoInterest" = "X6NoInterest", 
         "D7_Tired" = "X7Tired", 
         "D8_CantConcentrate" = "X8CantConcentrate", 
         "D9_Nervous" = "X9Nervous", 
         "D10_Paranoid" = "X10Paranoid", 
         "D11_CantSleep" = "X11CantSleep", 
         "D12_CantEat" = "X12CantEat", 
         "D13_CantFuck" = "X13CantFuck", 
         "D14_Pessimistic" = "X14Pessimistic", 
         "D16_PunishMe" = "X16PunishMe", 
         "D18_HeavyThoughts" = "X18HeavyThoughts",
         "Region" = "region", "Interview_Date" = "InterviewDate", "Sex" = "Male", "SCI" = "SocialConflictIndex", "SFluency" = "SpanishFluency", "Town_Distance" = "route_distance_town", "Com_Size" = "max_community_size", "HH_Size" = "household_size")


## Making transformations and/or setting appropriate variable types
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




########### Running Analyses on Item 1 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item1_formula <- bf(D1_Sad ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                    family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item1 <- brm(item1_formula, 
            data   = dep_data,
            prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D1Sad),
                      prior(normal(0, 0.5), class ="b", resp = D1Sad),
                      prior(exponential(1), class = "sd", resp = D1Sad),
    
                      prior(normal(0, 0.5), class="Intercept", resp = SCI),
                      prior(normal(0, 0.5), class ="b", resp = SCI),
                      
                      prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                      prior(normal(0, 0.5), class ="b", resp = SFluency),
                      
                      prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                      prior(normal(0, 0.5), class ="b", resp = HHSize)
            ),
            warmup = 1000, 
            iter   = 6000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99, max_treedepth = 12),
            seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item1, "item1.RDS")

## Load model
item1 <- readRDS("item1.RDS")

## Model summary
summary(item1)


########### Running Analyses on Item 2 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item2_formula <- bf(D2_Cry ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                    family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item2 <- brm(item2_formula, 
             data   = dep_data,
             prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D2Cry),
                       prior(normal(0, 0.5), class ="b", resp = D2Cry),
                       prior(exponential(1), class = "sd", resp = D2Cry),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SCI),
                       prior(normal(0, 0.5), class ="b", resp = SCI),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                       prior(normal(0, 0.5), class ="b", resp = SFluency),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                       prior(normal(0, 0.5), class ="b", resp = HHSize)
             ),
             warmup = 1000, 
             iter   = 6000, 
             chains = 4, 
             cores  = 8,
             control = list(adapt_delta = 0.99, max_treedepth = 12),
             seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item2, "item2.RDS")

## Load model
item2 <- readRDS("item2.RDS")

## Model summary
summary(item2)

########### Running Analyses on Item 3 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item3_formula <- bf(D3_SelfCritical ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                    family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item3 <- brm(item3_formula, 
             data   = dep_data,
             prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D3SelfCritical),
                       prior(normal(0, 0.5), class ="b", resp = D3SelfCritical),
                       prior(exponential(1), class = "sd", resp = D3SelfCritical),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SCI),
                       prior(normal(0, 0.5), class ="b", resp = SCI),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                       prior(normal(0, 0.5), class ="b", resp = SFluency),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                       prior(normal(0, 0.5), class ="b", resp = HHSize)
             ),
             warmup = 1000, 
             iter   = 6000, 
             chains = 4, 
             cores  = 8,
             control = list(adapt_delta = 0.99, max_treedepth = 12),
             seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item3, "item3.RDS")

## Load model
item3 <- readRDS("item3.RDS")

## Model summary
summary(item3)


########### Running Analyses on Item 4 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item4_formula <- bf(D4_LifeNotValuable ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                    family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item4 <- brm(item4_formula, 
             data   = dep_data,
             prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D4LifeNotValuable),
                       prior(normal(0, 0.5), class ="b", resp = D4LifeNotValuable),
                       prior(exponential(1), class = "sd", resp = D4LifeNotValuable),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SCI),
                       prior(normal(0, 0.5), class ="b", resp = SCI),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                       prior(normal(0, 0.5), class ="b", resp = SFluency),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                       prior(normal(0, 0.5), class ="b", resp = HHSize)
             ),
             warmup = 1000, 
             iter   = 6000, 
             chains = 4, 
             cores  = 8,
             control = list(adapt_delta = 0.99, max_treedepth = 12),
             seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item4, "item4.RDS")

## Load model
item4 <- readRDS("item4.RDS")

## Model summary
summary(item4)


########### Running Analyses on Item 5 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item5_formula <- bf(D5_Useless ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                    family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item5 <- brm(item5_formula, 
             data   = dep_data,
             prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D5Useless),
                       prior(normal(0, 0.5), class ="b", resp = D5Useless),
                       prior(exponential(1), class = "sd", resp = D5Useless),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SCI),
                       prior(normal(0, 0.5), class ="b", resp = SCI),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                       prior(normal(0, 0.5), class ="b", resp = SFluency),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                       prior(normal(0, 0.5), class ="b", resp = HHSize)
             ),
             warmup = 1000, 
             iter   = 6000, 
             chains = 4, 
             cores  = 8,
             control = list(adapt_delta = 0.99, max_treedepth = 12),
             seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item5, "item5.RDS")

## Load model
item5 <- readRDS("item5.RDS")

## Model summary
summary(item5)


########### Running Analyses on Item 6 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item6_formula <- bf(D6_NoInterest ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                    family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item6 <- brm(item6_formula, 
             data   = dep_data,
             prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D6NoInterest),
                       prior(normal(0, 0.5), class ="b", resp = D6NoInterest),
                       prior(exponential(1), class = "sd", resp = D6NoInterest),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SCI),
                       prior(normal(0, 0.5), class ="b", resp = SCI),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                       prior(normal(0, 0.5), class ="b", resp = SFluency),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                       prior(normal(0, 0.5), class ="b", resp = HHSize)
             ),
             warmup = 1000, 
             iter   = 6000, 
             chains = 4, 
             cores  = 8,
             control = list(adapt_delta = 0.99, max_treedepth = 12),
             seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item6, "item6.RDS")

## Load model
item6 <- readRDS("item6.RDS")

## Model summary
summary(item6)


########### Running Analyses on Item 7 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item7_formula <- bf(D7_Tired ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                    family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item7 <- brm(item7_formula, 
             data   = dep_data,
             prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D7Tired),
                       prior(normal(0, 0.5), class ="b", resp = D7Tired),
                       prior(exponential(1), class = "sd", resp = D7Tired),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SCI),
                       prior(normal(0, 0.5), class ="b", resp = SCI),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                       prior(normal(0, 0.5), class ="b", resp = SFluency),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                       prior(normal(0, 0.5), class ="b", resp = HHSize)
             ),
             warmup = 1000, 
             iter   = 6000, 
             chains = 4, 
             cores  = 8,
             control = list(adapt_delta = 0.99, max_treedepth = 12),
             seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item7, "item7.RDS")

## Load model
item7 <- readRDS("item7.RDS")

## Model summary
summary(item7)


########### Running Analyses on Item 8 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item8_formula <- bf(D8_CantConcentrate ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                    family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item8 <- brm(item8_formula, 
             data   = dep_data,
             prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D8CantConcentrate),
                       prior(normal(0, 0.5), class ="b", resp = D8CantConcentrate),
                       prior(exponential(1), class = "sd", resp = D8CantConcentrate),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SCI),
                       prior(normal(0, 0.5), class ="b", resp = SCI),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                       prior(normal(0, 0.5), class ="b", resp = SFluency),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                       prior(normal(0, 0.5), class ="b", resp = HHSize)
             ),
             warmup = 1000, 
             iter   = 6000, 
             chains = 4, 
             cores  = 8,
             control = list(adapt_delta = 0.99, max_treedepth = 12),
             seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item8, "item8.RDS")

## Load model
item8 <- readRDS("item8.RDS")

## Model summary
summary(item8)

########### Running Analyses on Item 9 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item9_formula <- bf(D9_Nervous ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                    family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item9 <- brm(item9_formula, 
             data   = dep_data,
             prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D9Nervous),
                       prior(normal(0, 0.5), class ="b", resp = D9Nervous),
                       prior(exponential(1), class = "sd", resp = D9Nervous),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SCI),
                       prior(normal(0, 0.5), class ="b", resp = SCI),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                       prior(normal(0, 0.5), class ="b", resp = SFluency),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                       prior(normal(0, 0.5), class ="b", resp = HHSize)
             ),
             warmup = 1000, 
             iter   = 6000, 
             chains = 4, 
             cores  = 8,
             control = list(adapt_delta = 0.99, max_treedepth = 12),
             seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item9, "item9.RDS")

## Load model
item9 <- readRDS("item9.RDS")

## Model summary
summary(item9)


########### Running Analyses on Item 10 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item10_formula <- bf(D10_Paranoid ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                    family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item10 <- brm(item10_formula, 
             data   = dep_data,
             prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D10Paranoid),
                       prior(normal(0, 0.5), class ="b", resp = D10Paranoid),
                       prior(exponential(1), class = "sd", resp = D10Paranoid),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SCI),
                       prior(normal(0, 0.5), class ="b", resp = SCI),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                       prior(normal(0, 0.5), class ="b", resp = SFluency),
                       
                       prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                       prior(normal(0, 0.5), class ="b", resp = HHSize)
             ),
             warmup = 1000, 
             iter   = 6000, 
             chains = 4, 
             cores  = 8,
             control = list(adapt_delta = 0.99, max_treedepth = 12),
             seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item10, "item10.RDS")

## Load model
item10 <- readRDS("item10.RDS")

## Model summary
summary(item10)


########### Running Analyses on Item 11 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item11_formula <- bf(D11_CantSleep ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                     family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item11 <- brm(item11_formula, 
              data   = dep_data,
              prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D11CantSleep),
                        prior(normal(0, 0.5), class ="b", resp = D11CantSleep),
                        prior(exponential(1), class = "sd", resp = D11CantSleep),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SCI),
                        prior(normal(0, 0.5), class ="b", resp = SCI),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                        prior(normal(0, 0.5), class ="b", resp = SFluency),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                        prior(normal(0, 0.5), class ="b", resp = HHSize)
              ),
              warmup = 1000, 
              iter   = 6000, 
              chains = 4, 
              cores  = 8,
              control = list(adapt_delta = 0.99, max_treedepth = 12),
              seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item11, "item11.RDS")

## Load model
item11 <- readRDS("item11.RDS")

## Model summary
summary(item11)

########### Running Analyses on Item 12 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item12_formula <- bf(D12_CantEat ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                     family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item12 <- brm(item12_formula, 
              data   = dep_data,
              prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D12CantEat),
                        prior(normal(0, 0.5), class ="b", resp = D12CantEat),
                        prior(exponential(1), class = "sd", resp = D12CantEat),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SCI),
                        prior(normal(0, 0.5), class ="b", resp = SCI),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                        prior(normal(0, 0.5), class ="b", resp = SFluency),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                        prior(normal(0, 0.5), class ="b", resp = HHSize)
              ),
              warmup = 1000, 
              iter   = 6000, 
              chains = 4, 
              cores  = 8,
              control = list(adapt_delta = 0.99, max_treedepth = 12),
              seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item12, "item12.RDS")

## Load model
item12 <- readRDS("item12.RDS")

## Model summary
summary(item12)

########### Running Analyses on Item 13 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item13_formula <- bf(D13_CantFuck ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                     family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item13 <- brm(item13_formula, 
              data   = dep_data,
              prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D13CantFuck),
                        prior(normal(0, 0.5), class ="b", resp = D13CantFuck),
                        prior(exponential(1), class = "sd", resp = D13CantFuck),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SCI),
                        prior(normal(0, 0.5), class ="b", resp = SCI),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                        prior(normal(0, 0.5), class ="b", resp = SFluency),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                        prior(normal(0, 0.5), class ="b", resp = HHSize)
              ),
              warmup = 1000, 
              iter   = 6000, 
              chains = 4, 
              cores  = 8,
              control = list(adapt_delta = 0.99, max_treedepth = 12),
              seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item13, "item13.RDS")

## Load model
item13 <- readRDS("item13.RDS")

## Model summary
summary(item13)


########### Running Analyses on Item 14 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item14_formula <- bf(D14_Pessimistic ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                     family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item14 <- brm(item14_formula, 
              data   = dep_data,
              prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D14Pessimistic),
                        prior(normal(0, 0.5), class ="b", resp = D14Pessimistic),
                        prior(exponential(1), class = "sd", resp = D14Pessimistic),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SCI),
                        prior(normal(0, 0.5), class ="b", resp = SCI),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                        prior(normal(0, 0.5), class ="b", resp = SFluency),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                        prior(normal(0, 0.5), class ="b", resp = HHSize)
              ),
              warmup = 1000, 
              iter   = 6000, 
              chains = 4, 
              cores  = 8,
              control = list(adapt_delta = 0.99, max_treedepth = 12),
              seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item14, "item14.RDS")

## Load model
item14 <- readRDS("item14.RDS")

## Model summary
summary(item14)

########### Running Analyses on Item 16 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item16_formula <- bf(D16_PunishMe ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                     family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item16 <- brm(item16_formula, 
              data   = dep_data,
              prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D16PunishMe),
                        prior(normal(0, 0.5), class ="b", resp = D16PunishMe),
                        prior(exponential(1), class = "sd", resp = D16PunishMe),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SCI),
                        prior(normal(0, 0.5), class ="b", resp = SCI),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                        prior(normal(0, 0.5), class ="b", resp = SFluency),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                        prior(normal(0, 0.5), class ="b", resp = HHSize)
              ),
              warmup = 1000, 
              iter   = 6000, 
              chains = 4, 
              cores  = 8,
              control = list(adapt_delta = 0.99, max_treedepth = 12),
              seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item16, "item16.RDS")

## Load model
item16 <- readRDS("item16.RDS")

## Model summary
summary(item16)
         

########### Running Analyses on Item 18 to quantify interviewer level effects  ##################
#################################################################################################################################

## Specifying the model
item18_formula <- bf(D18_HeavyThoughts ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + 
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | Region/ComID/UniqueFamID/PID),
                     family = cumulative("logit")) +
  
  bf(SCI | mi() ~ 1 + Age + Sex, family = gaussian) + 
  bf(SFluency | mi() ~ 1 + Age + Sex, family = gaussian()) + 
  bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size, family = gaussian()) + set_rescor(FALSE)


## Running the model
item18 <- brm(item18_formula, 
              data   = dep_data,
              prior = c(prior(normal(0, 1.5), class = "Intercept", resp = D18HeavyThoughts),
                        prior(normal(0, 0.5), class ="b", resp = D18HeavyThoughts),
                        prior(exponential(1), class = "sd", resp = D18HeavyThoughts),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SCI),
                        prior(normal(0, 0.5), class ="b", resp = SCI),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                        prior(normal(0, 0.5), class ="b", resp = SFluency),
                        
                        prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                        prior(normal(0, 0.5), class ="b", resp = HHSize)
              ),
              warmup = 1000, 
              iter   = 6000, 
              chains = 4, 
              cores  = 8,
              control = list(adapt_delta = 0.99, max_treedepth = 12),
              seed = 3233) 


## Save Model output (for others to easily load the model output later)
saveRDS(item18, "item18.RDS")

## Load model
item18 <- readRDS("item18.RDS")

## Model summary
summary(item18)
          
         
########### Plotting ICCs of all 16 items first on one graph  ##################
################################################################################################################################# 

### IMPORTANT: Note that some of these files and posteriors are quite big and won't execute in one go due to your machine's memory constraints
## I recommend running the script first, by loading the first 7 items and extracting draws.
## And then repeat for the remaining items


## Extract posterior draws first from all item models 
## Prolly recommend doing so for Items 1 to 7 first. This is what I have done in the lines below
i1 <- as_draws_df(item1)
i2 <- as_draws_df(item2)
i3 <- as_draws_df(item3)
i4 <- as_draws_df(item4)
i5 <- as_draws_df(item5)
i6 <- as_draws_df(item6)
i7 <- as_draws_df(item7)
i8 <- as_draws_df(item8)
i9 <- as_draws_df(item9)
i10 <- as_draws_df(item10)      
i11 <- as_draws_df(item11)
i12 <- as_draws_df(item12)
i13 <- as_draws_df(item13)
i14 <- as_draws_df(item14)
i16 <- as_draws_df(item16)      
i18 <- as_draws_df(item18)         

## Now calculate interviewer adjusted ICC for each item 

## Item 1
icc_int_i1 <- ((i1$sd_Interviewer__D1Sad_Intercept)^2) / ((i1$sd_Region__D1Sad_Intercept)^2 + (i1$`sd_Region:ComID__D1Sad_Intercept`)^2 + (i1$`sd_Region:ComID:UniqueFamID__D1Sad_Intercept`)^2 + 
                                                            (i1$`sd_Region:ComID:UniqueFamID:PID__D1Sad_Intercept`)^2 + (i1$sd_Interviewer__D1Sad_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i1, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 1")

## Item 2
icc_int_i2 <- ((i2$sd_Interviewer__D2Cry_Intercept)^2) / ((i2$sd_Region__D2Cry_Intercept)^2 + (i2$`sd_Region:ComID__D2Cry_Intercept`)^2 + (i2$`sd_Region:ComID:UniqueFamID__D2Cry_Intercept`)^2 + 
                                                            (i2$`sd_Region:ComID:UniqueFamID:PID__D2Cry_Intercept`)^2 + (i2$sd_Interviewer__D2Cry_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i2, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 2")

## Item 3
icc_int_i3 <- ((i3$sd_Interviewer__D3SelfCritical_Intercept)^2) / ((i3$sd_Region__D3SelfCritical_Intercept)^2 + (i3$`sd_Region:ComID__D3SelfCritical_Intercept`)^2 + (i3$`sd_Region:ComID:UniqueFamID__D3SelfCritical_Intercept`)^2 + 
                                                            (i3$`sd_Region:ComID:UniqueFamID:PID__D3SelfCritical_Intercept`)^2 + (i3$sd_Interviewer__D3SelfCritical_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i3, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 3")

## Item 4
icc_int_i4 <- ((i4$sd_Interviewer__D4LifeNotValuable_Intercept)^2) / ((i4$sd_Region__D4LifeNotValuable_Intercept)^2 + (i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`)^2 + (i4$`sd_Region:ComID:UniqueFamID__D4LifeNotValuable_Intercept`)^2 + 
                                                                     (i4$`sd_Region:ComID:UniqueFamID:PID__D4LifeNotValuable_Intercept`)^2 + (i4$sd_Interviewer__D4LifeNotValuable_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i4, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 4")

## Item 5
icc_int_i5 <- ((i5$sd_Interviewer__D5Useless_Intercept)^2) / ((i5$sd_Region__D5Useless_Intercept)^2 + (i5$`sd_Region:ComID__D5Useless_Intercept`)^2 + (i5$`sd_Region:ComID:UniqueFamID__D5Useless_Intercept`)^2 + 
                                                                        (i5$`sd_Region:ComID:UniqueFamID:PID__D5Useless_Intercept`)^2 + (i5$sd_Interviewer__D5Useless_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i5, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 5")

## Item 6
icc_int_i6 <- ((i6$sd_Interviewer__D6NoInterest_Intercept)^2) / ((i6$sd_Region__D6NoInterest_Intercept)^2 + (i6$`sd_Region:ComID__D6NoInterest_Intercept`)^2 + (i6$`sd_Region:ComID:UniqueFamID__D6NoInterest_Intercept`)^2 + 
                                                                (i6$`sd_Region:ComID:UniqueFamID:PID__D6NoInterest_Intercept`)^2 + (i6$sd_Interviewer__D6NoInterest_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i6, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 6")

## Item 7
icc_int_i7 <- ((i7$sd_Interviewer__D7Tired_Intercept)^2) / ((i7$sd_Region__D7Tired_Intercept)^2 + (i7$`sd_Region:ComID__D7Tired_Intercept`)^2 + (i7$`sd_Region:ComID:UniqueFamID__D7Tired_Intercept`)^2 + 
                                                                   (i7$`sd_Region:ComID:UniqueFamID:PID__D7Tired_Intercept`)^2 + (i7$sd_Interviewer__D7Tired_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i7, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 7")


## Now calculate community adjusted ICC for each item
#########################################################

## Item 1
icc_com_i1 <- ((i1$`sd_Region:ComID__D1Sad_Intercept`)^2) / ((i1$sd_Region__D1Sad_Intercept)^2 + (i1$`sd_Region:ComID__D1Sad_Intercept`)^2 + (i1$`sd_Region:ComID:UniqueFamID__D1Sad_Intercept`)^2 + 
                                                            (i1$`sd_Region:ComID:UniqueFamID:PID__D1Sad_Intercept`)^2 + (i1$sd_Interviewer__D1Sad_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i1, col=2, lwd=4 , xlab="ICC (Community) of Item 1")

## Item 2
icc_com_i2 <- ((i2$`sd_Region:ComID__D2Cry_Intercept`)^2) / ((i2$sd_Region__D2Cry_Intercept)^2 + (i2$`sd_Region:ComID__D2Cry_Intercept`)^2 + (i2$`sd_Region:ComID:UniqueFamID__D2Cry_Intercept`)^2 + 
                                                            (i2$`sd_Region:ComID:UniqueFamID:PID__D2Cry_Intercept`)^2 + (i2$sd_Interviewer__D2Cry_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i2, col=2, lwd=4 , xlab="ICC (Community) of Item 2")

## Item 3
icc_com_i3 <- ((i3$`sd_Region:ComID__D3SelfCritical_Intercept`)^2) / ((i3$sd_Region__D3SelfCritical_Intercept)^2 + (i3$`sd_Region:ComID__D3SelfCritical_Intercept`)^2 + (i3$`sd_Region:ComID:UniqueFamID__D3SelfCritical_Intercept`)^2 + 
                                                                     (i3$`sd_Region:ComID:UniqueFamID:PID__D3SelfCritical_Intercept`)^2 + (i3$sd_Interviewer__D3SelfCritical_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i3, col=2, lwd=4 , xlab="ICC (Community) of Item 3")

## Item 4
icc_com_i4 <- ((i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`)^2) / ((i4$sd_Region__D4LifeNotValuable_Intercept)^2 + (i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`)^2 + (i4$`sd_Region:ComID:UniqueFamID__D4LifeNotValuable_Intercept`)^2 + 
                                                                        (i4$`sd_Region:ComID:UniqueFamID:PID__D4LifeNotValuable_Intercept`)^2 + (i4$sd_Interviewer__D4LifeNotValuable_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i4, col=2, lwd=4 , xlab="ICC (Community) of Item 4")

## Item 5
icc_com_i5 <- ((i5$`sd_Region:ComID__D5Useless_Intercept`)^2) / ((i5$sd_Region__D5Useless_Intercept)^2 + (i5$`sd_Region:ComID__D5Useless_Intercept`)^2 + (i5$`sd_Region:ComID:UniqueFamID__D5Useless_Intercept`)^2 + 
                                                                (i5$`sd_Region:ComID:UniqueFamID:PID__D5Useless_Intercept`)^2 + (i5$sd_Interviewer__D5Useless_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i5, col=2, lwd=4 , xlab="ICC (Community) of Item 5")

## Item 6
icc_com_i6 <- ((i6$`sd_Region:ComID__D6NoInterest_Intercept`)^2) / ((i6$sd_Region__D6NoInterest_Intercept)^2 + (i6$`sd_Region:ComID__D6NoInterest_Intercept`)^2 + (i6$`sd_Region:ComID:UniqueFamID__D6NoInterest_Intercept`)^2 + 
                                                                   (i6$`sd_Region:ComID:UniqueFamID:PID__D6NoInterest_Intercept`)^2 + (i6$sd_Interviewer__D6NoInterest_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i6, col=2, lwd=4 , xlab="ICC (Community) of Item 6")

## Item 7
icc_com_i7 <- ((i7$`sd_Region:ComID__D7Tired_Intercept`)^2) / ((i7$sd_Region__D7Tired_Intercept)^2 + (i7$`sd_Region:ComID__D7Tired_Intercept`)^2 + (i7$`sd_Region:ComID:UniqueFamID__D7Tired_Intercept`)^2 + 
                                                              (i7$`sd_Region:ComID:UniqueFamID:PID__D7Tired_Intercept`)^2 + (i7$sd_Interviewer__D7Tired_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i7, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 7")


## Now calculate household adjusted ICC for each item
#########################################################

## Item 1
icc_hh_i1 <- ((i1$`sd_Region:ComID:UniqueFamID__D1Sad_Intercept`)^2) / ((i1$sd_Region__D1Sad_Intercept)^2 + (i1$`sd_Region:ComID__D1Sad_Intercept`)^2 + (i1$`sd_Region:ComID:UniqueFamID__D1Sad_Intercept`)^2 + 
                                                               (i1$`sd_Region:ComID:UniqueFamID:PID__D1Sad_Intercept`)^2 + (i1$sd_Interviewer__D1Sad_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i1, col=2, lwd=4 , xlab="ICC (Household) of Item 1")

## Item 2
icc_hh_i2 <- ((i2$`sd_Region:ComID:UniqueFamID__D2Cry_Intercept`)^2) / ((i2$sd_Region__D2Cry_Intercept)^2 + (i2$`sd_Region:ComID__D2Cry_Intercept`)^2 + (i2$`sd_Region:ComID:UniqueFamID__D2Cry_Intercept`)^2 + 
                                                               (i2$`sd_Region:ComID:UniqueFamID:PID__D2Cry_Intercept`)^2 + (i2$sd_Interviewer__D2Cry_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i2, col=2, lwd=4 , xlab="ICC (Household) of Item 2")

## Item 3
icc_hh_i3 <- ((i3$`sd_Region:ComID:UniqueFamID__D3SelfCritical_Intercept`)^2) / ((i3$sd_Region__D3SelfCritical_Intercept)^2 + (i3$`sd_Region:ComID__D3SelfCritical_Intercept`)^2 + (i3$`sd_Region:ComID:UniqueFamID__D3SelfCritical_Intercept`)^2 + 
                                                                        (i3$`sd_Region:ComID:UniqueFamID:PID__D3SelfCritical_Intercept`)^2 + (i3$sd_Interviewer__D3SelfCritical_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i3, col=2, lwd=4 , xlab="ICC (Household) of Item 3")

## Item 4
icc_hh_i4 <- ((i4$`sd_Region:ComID:UniqueFamID__D4LifeNotValuable_Intercept`)^2) / ((i4$sd_Region__D4LifeNotValuable_Intercept)^2 + (i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`)^2 + (i4$`sd_Region:ComID:UniqueFamID__D4LifeNotValuable_Intercept`)^2 + 
                                                                           (i4$`sd_Region:ComID:UniqueFamID:PID__D4LifeNotValuable_Intercept`)^2 + (i4$sd_Interviewer__D4LifeNotValuable_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i4, col=2, lwd=4 , xlab="ICC (Household) of Item 4")

## Item 5
icc_hh_i5 <- ((i5$`sd_Region:ComID:UniqueFamID__D5Useless_Intercept`)^2) / ((i5$sd_Region__D5Useless_Intercept)^2 + (i5$`sd_Region:ComID__D5Useless_Intercept`)^2 + (i5$`sd_Region:ComID:UniqueFamID__D5Useless_Intercept`)^2 + 
                                                                   (i5$`sd_Region:ComID:UniqueFamID:PID__D5Useless_Intercept`)^2 + (i5$sd_Interviewer__D5Useless_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i5, col=2, lwd=4 , xlab="ICC (Household) of Item 5")

## Item 6
icc_hh_i6 <- ((i6$`sd_Region:ComID:UniqueFamID__D6NoInterest_Intercept`)^2) / ((i6$sd_Region__D6NoInterest_Intercept)^2 + (i6$`sd_Region:ComID__D6NoInterest_Intercept`)^2 + (i6$`sd_Region:ComID:UniqueFamID__D6NoInterest_Intercept`)^2 + 
                                                                      (i6$`sd_Region:ComID:UniqueFamID:PID__D6NoInterest_Intercept`)^2 + (i6$sd_Interviewer__D6NoInterest_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i6, col=2, lwd=4 , xlab="ICC (Household) of Item 6")

## Item 7
icc_hh_i7 <- ((i7$`sd_Region:ComID:UniqueFamID__D7Tired_Intercept`)^2) / ((i7$sd_Region__D7Tired_Intercept)^2 + (i7$`sd_Region:ComID__D7Tired_Intercept`)^2 + (i7$`sd_Region:ComID:UniqueFamID__D7Tired_Intercept`)^2 + 
                                                                 (i7$`sd_Region:ComID:UniqueFamID:PID__D7Tired_Intercept`)^2 + (i7$sd_Interviewer__D7Tired_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i7, col=2, lwd=4 , xlab="ICC (Household) of Item 7")




############## Intermediate steps to preserve computer memory
#############################################################

## Combining initial interviewer icc vectors into a dataframe
icc_item_data <- data.frame(
  value = c(icc_int_i1, icc_int_i2, icc_int_i3, icc_int_i4, icc_int_i5, icc_int_i6, icc_int_i7),
  group = rep(c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6", "Item7"), each = length(icc_int_i1))
)

## Combining initial community icc vectors into a dataframe
icc_item_data_com <- data.frame(
  value = c(icc_com_i1, icc_com_i2, icc_com_i3, icc_com_i4, icc_com_i5, icc_com_i6, icc_com_i7),
  group = rep(c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6", "Item7"), each = length(icc_int_i1))
)

## Combining initial household icc vectors into a dataframe
icc_item_data_hh <- data.frame(
  value = c(icc_hh_i1, icc_hh_i2, icc_hh_i3, icc_hh_i4, icc_hh_i5, icc_hh_i6, icc_hh_i7),
  group = rep(c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6", "Item7"), each = length(icc_int_i1))
)


## Saving this all to csv files
write.csv(icc_item_data, "Interviewer_ICC_I1_I7.csv")
write.csv(icc_item_data_com, "Community_ICC_I1_I7.csv")      
write.csv(icc_item_data_hh, "Household_ICC_I1_I7.csv")        


#############################################################
#############################################################
### Reload these files and think of starting this script afresh, where we are now calculating the ICCs of the remaining items

icc_item_data <- read.csv("Interviewer_ICC_I1_I7.csv")
icc_item_data_com <- read.csv("Community_ICC_I1_I7.csv")
icc_item_data_hh <- read.csv("Household_ICC_I1_I7.csv")

# Okay, now let's calculate the Interviewer Adjusted ICCs for Item 8 - 18
#############################################################

## Item 8
icc_int_i8 <- ((i8$sd_Interviewer__D8CantConcentrate_Intercept)^2) / ((i8$sd_Region__D8CantConcentrate_Intercept)^2 + (i8$`sd_Region:ComID__D8CantConcentrate_Intercept`)^2 + (i8$`sd_Region:ComID:UniqueFamID__D8CantConcentrate_Intercept`)^2 + 
                                                            (i8$`sd_Region:ComID:UniqueFamID:PID__D8CantConcentrate_Intercept`)^2 + (i8$sd_Interviewer__D8CantConcentrate_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i8, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 8")

## Item 9
icc_int_i9 <- ((i9$sd_Interviewer__D9Nervous_Intercept)^2) / ((i9$sd_Region__D9Nervous_Intercept)^2 + (i9$`sd_Region:ComID__D9Nervous_Intercept`)^2 + (i9$`sd_Region:ComID:UniqueFamID__D9Nervous_Intercept`)^2 + 
                                                                        (i9$`sd_Region:ComID:UniqueFamID:PID__D9Nervous_Intercept`)^2 + (i9$sd_Interviewer__D9Nervous_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i9, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 9")

## Item 10
icc_int_i10 <- ((i10$sd_Interviewer__D10Paranoid_Intercept)^2) / ((i10$sd_Region__D10Paranoid_Intercept)^2 + (i10$`sd_Region:ComID__D10Paranoid_Intercept`)^2 + (i10$`sd_Region:ComID:UniqueFamID__D10Paranoid_Intercept`)^2 + 
                                                                (i10$`sd_Region:ComID:UniqueFamID:PID__D10Paranoid_Intercept`)^2 + (i10$sd_Interviewer__D10Paranoid_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i10, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 10")

## Item 11
icc_int_i11 <- ((i11$sd_Interviewer__D11CantSleep_Intercept)^2) / ((i11$sd_Region__D11CantSleep_Intercept)^2 + (i11$`sd_Region:ComID__D11CantSleep_Intercept`)^2 + (i11$`sd_Region:ComID:UniqueFamID__D11CantSleep_Intercept`)^2 + 
                                                                    (i11$`sd_Region:ComID:UniqueFamID:PID__D11CantSleep_Intercept`)^2 + (i11$sd_Interviewer__D11CantSleep_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i11, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 11")

## Item 12
icc_int_i12 <- ((i12$sd_Interviewer__D12CantEat_Intercept)^2) / ((i12$sd_Region__D12CantEat_Intercept)^2 + (i12$`sd_Region:ComID__D12CantEat_Intercept`)^2 + (i12$`sd_Region:ComID:UniqueFamID__D12CantEat_Intercept`)^2 + 
                                                                     (i12$`sd_Region:ComID:UniqueFamID:PID__D12CantEat_Intercept`)^2 + (i12$sd_Interviewer__D12CantEat_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i12, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 12")

## Item 13
icc_int_i13 <- ((i13$sd_Interviewer__D13CantFuck_Intercept)^2) / ((i13$sd_Region__D13CantFuck_Intercept)^2 + (i13$`sd_Region:ComID__D13CantFuck_Intercept`)^2 + (i13$`sd_Region:ComID:UniqueFamID__D13CantFuck_Intercept`)^2 + 
                                                                   (i13$`sd_Region:ComID:UniqueFamID:PID__D13CantFuck_Intercept`)^2 + (i13$sd_Interviewer__D13CantFuck_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i13, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 13")

## Item 14
icc_int_i14 <- ((i14$sd_Interviewer__D14Pessimistic_Intercept)^2) / ((i14$sd_Region__D14Pessimistic_Intercept)^2 + (i14$`sd_Region:ComID__D14Pessimistic_Intercept`)^2 + (i14$`sd_Region:ComID:UniqueFamID__D14Pessimistic_Intercept`)^2 + 
                                                                    (i14$`sd_Region:ComID:UniqueFamID:PID__D14Pessimistic_Intercept`)^2 + (i14$sd_Interviewer__D14Pessimistic_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i14, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 14")

## Item 16
icc_int_i16 <- ((i16$sd_Interviewer__D16PunishMe_Intercept)^2) / ((i16$sd_Region__D16PunishMe_Intercept)^2 + (i16$`sd_Region:ComID__D16PunishMe_Intercept`)^2 + (i16$`sd_Region:ComID:UniqueFamID__D16PunishMe_Intercept`)^2 + 
                                                                       (i16$`sd_Region:ComID:UniqueFamID:PID__D16PunishMe_Intercept`)^2 + (i16$sd_Interviewer__D16PunishMe_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i16, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 16")

## Item 18
icc_int_i18 <- ((i18$sd_Interviewer__D18HeavyThoughts_Intercept)^2) / ((i18$sd_Region__D18HeavyThoughts_Intercept)^2 + (i18$`sd_Region:ComID__D18HeavyThoughts_Intercept`)^2 + (i18$`sd_Region:ComID:UniqueFamID__D18HeavyThoughts_Intercept`)^2 + 
                                                                    (i18$`sd_Region:ComID:UniqueFamID:PID__D18HeavyThoughts_Intercept`)^2 + (i18$sd_Interviewer__D18HeavyThoughts_Intercept)^2  + (((pi)^2)/3))
dens(icc_int_i18, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 18")


## Now calculate community adjusted ICC for each item
#########################################################

## Item 8
icc_com_i8 <- ((i8$`sd_Region:ComID__D8CantConcentrate_Intercept`)^2) / ((i8$sd_Region__D8CantConcentrate_Intercept)^2 + (i8$`sd_Region:ComID__D8CantConcentrate_Intercept`)^2 + (i8$`sd_Region:ComID:UniqueFamID__D8CantConcentrate_Intercept`)^2 + 
                                                                        (i8$`sd_Region:ComID:UniqueFamID:PID__D8CantConcentrate_Intercept`)^2 + (i8$sd_Interviewer__D8CantConcentrate_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i8, col=2, lwd=4 , xlab="ICC (Community) of Item 8")

## Item 9
icc_com_i9 <- ((i9$`sd_Region:ComID__D9Nervous_Intercept`)^2) / ((i9$sd_Region__D9Nervous_Intercept)^2 + (i9$`sd_Region:ComID__D9Nervous_Intercept`)^2 + (i9$`sd_Region:ComID:UniqueFamID__D9Nervous_Intercept`)^2 + 
                                                                (i9$`sd_Region:ComID:UniqueFamID:PID__D9Nervous_Intercept`)^2 + (i9$sd_Interviewer__D9Nervous_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i9, col=2, lwd=4 , xlab="ICC (Community) of Item 9")

## Item 10
icc_com_i10 <- ((i10$`sd_Region:ComID__D10Paranoid_Intercept`)^2) / ((i10$sd_Region__D10Paranoid_Intercept)^2 + (i10$`sd_Region:ComID__D10Paranoid_Intercept`)^2 + (i10$`sd_Region:ComID:UniqueFamID__D10Paranoid_Intercept`)^2 + 
                                                                    (i10$`sd_Region:ComID:UniqueFamID:PID__D10Paranoid_Intercept`)^2 + (i10$sd_Interviewer__D10Paranoid_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i10, col=2, lwd=4 , xlab="ICC (Community) of Item 10")

## Item 11
icc_com_i11 <- ((i11$`sd_Region:ComID__D11CantSleep_Intercept`)^2) / ((i11$sd_Region__D11CantSleep_Intercept)^2 + (i11$`sd_Region:ComID__D11CantSleep_Intercept`)^2 + (i11$`sd_Region:ComID:UniqueFamID__D11CantSleep_Intercept`)^2 + 
                                                                     (i11$`sd_Region:ComID:UniqueFamID:PID__D11CantSleep_Intercept`)^2 + (i11$sd_Interviewer__D11CantSleep_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i11, col=2, lwd=4 , xlab="ICC (Community) of Item 11")

## Item 12
icc_com_i12 <- ((i12$`sd_Region:ComID__D12CantEat_Intercept`)^2) / ((i12$sd_Region__D12CantEat_Intercept)^2 + (i12$`sd_Region:ComID__D12CantEat_Intercept`)^2 + (i12$`sd_Region:ComID:UniqueFamID__D12CantEat_Intercept`)^2 + 
                                                                   (i12$`sd_Region:ComID:UniqueFamID:PID__D12CantEat_Intercept`)^2 + (i12$sd_Interviewer__D12CantEat_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i12, col=2, lwd=4 , xlab="ICC (Community) of Item 12")

## Item 13
icc_com_i13 <- ((i13$`sd_Region:ComID__D13CantFuck_Intercept`)^2) / ((i13$sd_Region__D13CantFuck_Intercept)^2 + (i13$`sd_Region:ComID__D13CantFuck_Intercept`)^2 + (i13$`sd_Region:ComID:UniqueFamID__D13CantFuck_Intercept`)^2 + 
                                                                    (i13$`sd_Region:ComID:UniqueFamID:PID__D13CantFuck_Intercept`)^2 + (i13$sd_Interviewer__D13CantFuck_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i13, col=2, lwd=4 , xlab="ICC (Community) of Item 13")

## Item 14
icc_com_i14 <- ((i14$`sd_Region:ComID__D14Pessimistic_Intercept`)^2) / ((i14$sd_Region__D14Pessimistic_Intercept)^2 + (i14$`sd_Region:ComID__D14Pessimistic_Intercept`)^2 + (i14$`sd_Region:ComID:UniqueFamID__D14Pessimistic_Intercept`)^2 + 
                                                                       (i14$`sd_Region:ComID:UniqueFamID:PID__D14Pessimistic_Intercept`)^2 + (i14$sd_Interviewer__D14Pessimistic_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i14, col=2, lwd=4 , xlab="ICC (Community) of Item 14")

## Item 16
icc_com_i16 <- ((i16$`sd_Region:ComID__D16PunishMe_Intercept`)^2) / ((i16$sd_Region__D16PunishMe_Intercept)^2 + (i16$`sd_Region:ComID__D16PunishMe_Intercept`)^2 + (i16$`sd_Region:ComID:UniqueFamID__D16PunishMe_Intercept`)^2 + 
                                                                    (i16$`sd_Region:ComID:UniqueFamID:PID__D16PunishMe_Intercept`)^2 + (i16$sd_Interviewer__D16PunishMe_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i16, col=2, lwd=4 , xlab="ICC (Community) of Item 16")

## Item 18
icc_com_i18 <- ((i18$`sd_Region:ComID__D18HeavyThoughts_Intercept`)^2) / ((i18$sd_Region__D18HeavyThoughts_Intercept)^2 + (i18$`sd_Region:ComID__D18HeavyThoughts_Intercept`)^2 + (i18$`sd_Region:ComID:UniqueFamID__D18HeavyThoughts_Intercept`)^2 + 
                                                                         (i18$`sd_Region:ComID:UniqueFamID:PID__D18HeavyThoughts_Intercept`)^2 + (i18$sd_Interviewer__D18HeavyThoughts_Intercept)^2  + (((pi)^2)/3))
dens(icc_com_i18, col=2, lwd=4 , xlab="ICC (Interviewer) of Item 18")


## Now calculate household adjusted ICC for each item
#########################################################

## Item 8
icc_hh_i8 <- ((i8$`sd_Region:ComID:UniqueFamID__D8CantConcentrate_Intercept`)^2) / ((i8$sd_Region__D8CantConcentrate_Intercept)^2 + (i8$`sd_Region:ComID__D8CantConcentrate_Intercept`)^2 + (i8$`sd_Region:ComID:UniqueFamID__D8CantConcentrate_Intercept`)^2 + 
                                                                           (i8$`sd_Region:ComID:UniqueFamID:PID__D8CantConcentrate_Intercept`)^2 + (i8$sd_Interviewer__D8CantConcentrate_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i8, col=2, lwd=4 , xlab="ICC (Household) of Item 8")

## Item 9
icc_hh_i9 <- ((i9$`sd_Region:ComID:UniqueFamID__D9Nervous_Intercept`)^2) / ((i9$sd_Region__D9Nervous_Intercept)^2 + (i9$`sd_Region:ComID__D9Nervous_Intercept`)^2 + (i9$`sd_Region:ComID:UniqueFamID__D9Nervous_Intercept`)^2 + 
                                                                   (i9$`sd_Region:ComID:UniqueFamID:PID__D9Nervous_Intercept`)^2 + (i9$sd_Interviewer__D9Nervous_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i9, col=2, lwd=4 , xlab="ICC (Household) of Item 9")

## Item 10
icc_hh_i10 <- ((i10$`sd_Region:ComID:UniqueFamID__D10Paranoid_Intercept`)^2) / ((i10$sd_Region__D10Paranoid_Intercept)^2 + (i10$`sd_Region:ComID__D10Paranoid_Intercept`)^2 + (i10$`sd_Region:ComID:UniqueFamID__D10Paranoid_Intercept`)^2 + 
                                                                       (i10$`sd_Region:ComID:UniqueFamID:PID__D10Paranoid_Intercept`)^2 + (i10$sd_Interviewer__D10Paranoid_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i10, col=2, lwd=4 , xlab="ICC (Household) of Item 10")

## Item 11
icc_hh_i11 <- ((i11$`sd_Region:ComID:UniqueFamID__D11CantSleep_Intercept`)^2) / ((i11$sd_Region__D11CantSleep_Intercept)^2 + (i11$`sd_Region:ComID__D11CantSleep_Intercept`)^2 + (i11$`sd_Region:ComID:UniqueFamID__D11CantSleep_Intercept`)^2 + 
                                                                        (i11$`sd_Region:ComID:UniqueFamID:PID__D11CantSleep_Intercept`)^2 + (i11$sd_Interviewer__D11CantSleep_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i11, col=2, lwd=4 , xlab="ICC (Household) of Item 11")

## Item 12
icc_hh_i12 <- ((i12$`sd_Region:ComID:UniqueFamID__D12CantEat_Intercept`)^2) / ((i12$sd_Region__D12CantEat_Intercept)^2 + (i12$`sd_Region:ComID__D12CantEat_Intercept`)^2 + (i12$`sd_Region:ComID:UniqueFamID__D12CantEat_Intercept`)^2 + 
                                                                      (i12$`sd_Region:ComID:UniqueFamID:PID__D12CantEat_Intercept`)^2 + (i12$sd_Interviewer__D12CantEat_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i12, col=2, lwd=4 , xlab="ICC (Household) of Item 12")

## Item 13
icc_hh_i13 <- ((i13$`sd_Region:ComID:UniqueFamID__D13CantFuck_Intercept`)^2) / ((i13$sd_Region__D13CantFuck_Intercept)^2 + (i13$`sd_Region:ComID__D13CantFuck_Intercept`)^2 + (i13$`sd_Region:ComID:UniqueFamID__D13CantFuck_Intercept`)^2 + 
                                                                       (i13$`sd_Region:ComID:UniqueFamID:PID__D13CantFuck_Intercept`)^2 + (i13$sd_Interviewer__D13CantFuck_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i13, col=2, lwd=4 , xlab="ICC (Household) of Item 13")

## Item 14
icc_hh_i14 <- ((i14$`sd_Region:ComID:UniqueFamID__D14Pessimistic_Intercept`)^2) / ((i14$sd_Region__D14Pessimistic_Intercept)^2 + (i14$`sd_Region:ComID__D14Pessimistic_Intercept`)^2 + (i14$`sd_Region:ComID:UniqueFamID__D14Pessimistic_Intercept`)^2 + 
                                                                          (i14$`sd_Region:ComID:UniqueFamID:PID__D14Pessimistic_Intercept`)^2 + (i14$sd_Interviewer__D14Pessimistic_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i14, col=2, lwd=4 , xlab="ICC (Household) of Item 14")

## Item 16
icc_hh_i16 <- ((i16$`sd_Region:ComID:UniqueFamID__D16PunishMe_Intercept`)^2) / ((i16$sd_Region__D16PunishMe_Intercept)^2 + (i16$`sd_Region:ComID__D16PunishMe_Intercept`)^2 + (i16$`sd_Region:ComID:UniqueFamID__D16PunishMe_Intercept`)^2 + 
                                                                       (i16$`sd_Region:ComID:UniqueFamID:PID__D16PunishMe_Intercept`)^2 + (i16$sd_Interviewer__D16PunishMe_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i16, col=2, lwd=4 , xlab="ICC (Household) of Item 16")

## Item 18
icc_hh_i18 <- ((i18$`sd_Region:ComID:UniqueFamID__D18HeavyThoughts_Intercept`)^2) / ((i18$sd_Region__D18HeavyThoughts_Intercept)^2 + (i18$`sd_Region:ComID__D18HeavyThoughts_Intercept`)^2 + (i18$`sd_Region:ComID:UniqueFamID__D18HeavyThoughts_Intercept`)^2 + 
                                                                            (i18$`sd_Region:ComID:UniqueFamID:PID__D18HeavyThoughts_Intercept`)^2 + (i18$sd_Interviewer__D18HeavyThoughts_Intercept)^2  + (((pi)^2)/3))
dens(icc_hh_i18, col=2, lwd=4 , xlab="ICC (Household) of Item 18")


############## Final steps to now combine previous ICC data
#############################################################

## Combining the recent interviewer icc vectors into a dataframe
icc_item_data_v2 <- data.frame(
  value = c(icc_int_i8, icc_int_i9, icc_int_i10, icc_int_i11, icc_int_i12, icc_int_i13, icc_int_i14, icc_int_i16, icc_int_i18),
  group = rep(c("Item8", "Item9", "Item10", "Item11", "Item12", "Item13", "Item14", "Item16", "Item18"), each = length(icc_int_i8))
)

## Combining recent community icc vectors into a dataframe
icc_item_data_com_v2 <- data.frame(
  value = c(icc_com_i8, icc_com_i9, icc_com_i10, icc_com_i11, icc_com_i12, icc_com_i13, icc_com_i14, icc_com_i16, icc_com_i18),
  group = rep(c("Item8", "Item9", "Item10", "Item11", "Item12", "Item13", "Item14", "Item16", "Item18"), each = length(icc_com_i8))
)

## Combining initial community icc vectors into a dataframe
icc_item_data_hh_v2 <- data.frame(
  value = c(icc_hh_i8, icc_hh_i9, icc_hh_i10, icc_hh_i11, icc_hh_i12, icc_hh_i13, icc_hh_i14, icc_hh_i16, icc_hh_i18),
  group = rep(c("Item8", "Item9", "Item10", "Item11", "Item12", "Item13", "Item14", "Item16", "Item18"), each = length(icc_hh_i8))
)


### Now we aim to combine our initial and recently created icc dataframes so that we can visualise ICCs across items

## First, remove the first extra column (added due to loading it as a csv) from the previous dataframes
icc_item_data <- icc_item_data[, -1]
icc_item_data_com <- icc_item_data_com[, -1]
icc_item_data_hh <- icc_item_data_hh[, -1]

## Now combine the dataframes
icc_item_data_combined <- rbind(icc_item_data, icc_item_data_v2)
icc_item_data_com_combined <- rbind(icc_item_data_com, icc_item_data_com_v2)
icc_item_data_hh_combined <- rbind(icc_item_data_hh, icc_item_data_hh_v2)

## Convert the group column here into a factor and order it to make the color legend on the final plot easier to read
icc_item_data_combined$group <- factor(icc_item_data_combined$group, levels = c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6", "Item7", "Item8", "Item9", "Item10", "Item11", "Item12", "Item13", "Item14", "Item16", "Item18"))
icc_item_data_com_combined$group <- factor(icc_item_data_com_combined$group, levels = c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6", "Item7", "Item8", "Item9", "Item10", "Item11", "Item12", "Item13", "Item14", "Item16", "Item18"))
icc_item_data_hh_combined$group <- factor(icc_item_data_hh_combined$group, levels = c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6", "Item7", "Item8", "Item9", "Item10", "Item11", "Item12", "Item13", "Item14", "Item16", "Item18"))

## Now let's plot interviewer ICC of all items in one graph
int_icc_plot <- ggplot(icc_item_data_combined, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "(a) ICC for Interviewers Across Different Items",
       x = "ICC",
       y = "Density",
       color = "Depression Scale Item") +
  theme_minimal()  +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )

## Now let's plot community ICC of all items in one graph
com_icc_plot <- ggplot(icc_item_data_com_combined, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "(a) ICC for Community Across Different Items",
       x = "ICC",
       y = "Density",
       color = "Depression Scale Item") +
  theme_minimal()  +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )

## Now let's plot household ICC of all items in one graph
hh_icc_plot <- ggplot(icc_item_data_hh_combined, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "(b) ICC for Household Across Different Items",
       x = "ICC",
       y = "Density",
       color = "Depression Scale Item") +
  theme_minimal()  +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )


### Okay, let's illustrate caterpillar plots for the same ICCs now to enhance readability

#################### First for interviewers 

## Manually calculating the mean, adn the 95% intervals 
icc_int_summary <- icc_item_data_combined %>%
  group_by(group) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975)
  ) %>%
  ungroup()

## Plot
int_icc_caterpillar_plot <- ggplot(icc_int_summary, aes(x = mean, y = group)) +
  geom_point(color = "darkcyan", size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "darkcyan") +
  scale_y_discrete(limits = rev(levels(icc_int_summary$group))) +
  theme_minimal(base_size = 14) +
  labs(
    title = "(b) Caterpillar Plot of Interviewer ICCs across Items",
    x = "ICC",
    y = "Item"
  ) +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )

#################### Now for communities

## Manually calculating the mean, adn the 95% intervals 
icc_com_summary <- icc_item_data_com_combined %>%
  group_by(group) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975)
  ) %>%
  ungroup()

## Plot
com_icc_caterpillar_plot <- ggplot(icc_com_summary, aes(x = mean, y = group)) +
  geom_point(color = "red", size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "red") +
  scale_y_discrete(limits = rev(levels(icc_com_summary$group))) +
  theme_minimal(base_size = 14) +
  labs(
    title = "(c) Caterpillar Plot of Community ICCs across Items",
    x = "ICC",
    y = "Item"
  ) +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )


#################### Now for households

## Manually calculating the mean, and the 95% intervals 
icc_hh_summary <- icc_item_data_hh_combined %>%
  group_by(group) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975)
  ) %>%
  ungroup()

## Plot
hh_icc_caterpillar_plot <- ggplot(icc_hh_summary, aes(x = mean, y = group)) +
  geom_point(color = "orange", size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "orange") +
  scale_y_discrete(limits = rev(levels(icc_hh_summary$group))) +
  theme_minimal(base_size = 14) +
  labs(
    title = " (d) Caterpillar Plot of Household ICCs across Items",
    x = "ICC",
    y = "Item"
  ) +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )


### Now, first combining interviewer ICC plots (both caterpillar and distributions)
int_icc_plot_combined <- (int_icc_plot | int_icc_caterpillar_plot)

### Now, first combining other ICC plots (both caterpillar and distributions)
hh_com_icc_plot_combined <- (com_icc_plot | hh_icc_plot) / (com_icc_caterpillar_plot | hh_icc_caterpillar_plot)


# PLOTTING VARYING ASSOCIATIONS OF HIGHER LEVEL DETERMINANTS BETWEEN ITEMS
##########################################################################
#########################################################################

# Let's start with the predictors
##########################################################################

# First, distance to market town
## Again, we are doing certain items initially (due to my local machine's memory constraints)
# We are exponentiating to make the associations interpretable to odds-ratio
I1_dist <- exp(i1$b_D1Sad_Town_Distance)
I2_dist <- exp(i2$b_D2Cry_Town_Distance)
I3_dist <- exp(i3$b_D3SelfCritical_Town_Distance)
I4_dist <- exp(i4$b_D4LifeNotValuable_Town_Distance)
I5_dist <- exp(i5$b_D5Useless_Town_Distance)
I6_dist <- exp(i6$b_D6NoInterest_Town_Distance)
I7_dist <- exp(i7$b_D7Tired_Town_Distance)
I8_dist <- exp(i8$b_D8CantConcentrate_Town_Distance)

## Now doing this for Community Size
I1_comsize <- exp(i1$b_D1Sad_Com_Size)
I2_comsize <- exp(i2$b_D2Cry_Com_Size)
I3_comsize <- exp(i3$b_D3SelfCritical_Com_Size)
I4_comsize <- exp(i4$b_D4LifeNotValuable_Com_Size)
I5_comsize <- exp(i5$b_D5Useless_Com_Size)
I6_comsize <- exp(i6$b_D6NoInterest_Com_Size)
I7_comsize <- exp(i7$b_D7Tired_Com_Size)
I8_comsize <- exp(i8$b_D8CantConcentrate_Com_Size)

## Now doing this for Household Size
I1_hhsize <- exp(i1$bsp_D1Sad_miHH_Size)
I2_hhsize <- exp(i2$bsp_D2Cry_miHH_Size)
I3_hhsize <- exp(i3$bsp_D3SelfCritical_miHH_Size)
I4_hhsize <- exp(i4$bsp_D4LifeNotValuable_miHH_Size)
I5_hhsize <- exp(i5$bsp_D5Useless_miHH_Size)
I6_hhsize <- exp(i6$bsp_D6NoInterest_miHH_Size)
I7_hhsize <- exp(i7$bsp_D7Tired_miHH_Size)
I8_hhsize <- exp(i8$bsp_D8CantConcentrate_miHH_Size)


############## Intermediate steps to preserve computer memory
#############################################################

# Combining distance to market town posteriors into a data frame (columns = items)
posterior_com_dist <- as.data.frame(cbind(
  Item1 = I1_dist,
  Item2 = I2_dist,
  Item3 = I3_dist,
  Item4 = I4_dist,
  Item5 = I5_dist,
  Item6 = I6_dist,
  Item7 = I7_dist,
  Item8 = I8_dist
))

# Combining community size posteriors into a data frame (columns = items)
posterior_com_size <- as.data.frame(cbind(
  Item1 = I1_comsize,
  Item2 = I2_comsize,
  Item3 = I3_comsize,
  Item4 = I4_comsize,
  Item5 = I5_comsize,
  Item6 = I6_comsize,
  Item7 = I7_comsize,
  Item8 = I8_comsize
))

# Combining household size posteriors into a data frame (columns = items)
posterior_hh_size <- as.data.frame(cbind(
  Item1 = I1_hhsize,
  Item2 = I2_hhsize,
  Item3 = I3_hhsize,
  Item4 = I4_hhsize,
  Item5 = I5_hhsize,
  Item6 = I6_hhsize,
  Item7 = I7_hhsize,
  Item8 = I8_hhsize
))


## Saving this all to csv files
write.csv(posterior_com_dist, "Com_Dist_I1_I8.csv")
write.csv(posterior_com_size, "Com_Size_I1_I8.csv")      
write.csv(posterior_hh_size, "HH_Size_I1_I8.csv")        


#############################################################
#############################################################
### Reload these files and think of starting this script afresh, where we are now extracting the posteriors of the remaining items

posterior_com_dist <- read.csv("Com_Dist_I1_I8.csv")
posterior_com_size <- read.csv("Com_Size_I1_I8.csv")
posterior_hh_size <- read.csv("HH_Size_I1_I8.csv")


#############################################################
#############################################################
## Now extracting posteriors of predictors for the other 8 items

### First for distance to market town
I9_dist <- exp(i9$b_D9Nervous_Town_Distance)
I10_dist <- exp(i10$b_D10Paranoid_Town_Distance)
I11_dist <- exp(i11$b_D11CantSleep_Town_Distance)
I12_dist <- exp(i12$b_D12CantEat_Town_Distance)
I13_dist <- exp(i13$b_D13CantFuck_Town_Distance)
I14_dist <- exp(i14$b_D14Pessimistic_Town_Distance)
I16_dist <- exp(i16$b_D16PunishMe_Town_Distance)
I18_dist <- exp(i18$b_D18HeavyThoughts_Town_Distance)

### Now for community size
I9_comsize <- exp(i9$b_D9Nervous_Com_Size)
I10_comsize <- exp(i10$b_D10Paranoid_Com_Size)
I11_comsize <- exp(i11$b_D11CantSleep_Com_Size)
I12_comsize <- exp(i12$b_D12CantEat_Com_Size)
I13_comsize <- exp(i13$b_D13CantFuck_Com_Size)
I14_comsize <- exp(i14$b_D14Pessimistic_Com_Size)
I16_comsize <- exp(i16$b_D16PunishMe_Com_Size)
I18_comsize <- exp(i18$b_D18HeavyThoughts_Com_Size)

### Now for household size
I9_hhsize <- exp(i9$bsp_D9Nervous_miHH_Size)
I10_hhsize <- exp(i10$bsp_D10Paranoid_miHH_Size)
I11_hhsize <- exp(i11$bsp_D11CantSleep_miHH_Size)
I12_hhsize <- exp(i12$bsp_D12CantEat_miHH_Size)
I13_hhsize <- exp(i13$bsp_D13CantFuck_miHH_Size)
I14_hhsize <- exp(i14$bsp_D14Pessimistic_miHH_Size)
I16_hhsize <- exp(i16$bsp_D16PunishMe_miHH_Size)
I18_hhsize <- exp(i18$bsp_D18HeavyThoughts_miHH_Size)

## Now combining all of these into a dataframe

# Combining distance to market town posteriors into a data frame 
posterior_com_dist_v2 <- as.data.frame(cbind(
  Item9 = I9_dist,
  Item10 = I10_dist,
  Item11 = I11_dist,
  Item12 = I12_dist,
  Item13 = I13_dist,
  Item14 = I14_dist,
  Item16 = I16_dist,
  Item18 = I18_dist
))

# Now Combining community size posteriors into a data frame 
posterior_com_size_v2 <- as.data.frame(cbind(
  Item9  = I9_comsize,
  Item10 = I10_comsize,
  Item11 = I11_comsize,
  Item12 = I12_comsize,
  Item13 = I13_comsize,
  Item14 = I14_comsize,
  Item16 = I16_comsize,
  Item18 = I18_comsize
))

# Now Combining household size posteriors into a data frame 
posterior_hh_size_v2 <- as.data.frame(cbind(
  Item9  = I9_hhsize,
  Item10 = I10_hhsize,
  Item11 = I11_hhsize,
  Item12 = I12_hhsize,
  Item13 = I13_hhsize,
  Item14 = I14_hhsize,
  Item16 = I16_hhsize,
  Item18 = I18_hhsize
))


### Now we aim to combine our initial and recently created posterior dataframes so that we can visualise posteriors of predictors across items

## First, remove the first extra column (added due to loading it as a csv) from the previous dataframes
posterior_com_dist <- posterior_com_dist[, -1]
posterior_com_size <- posterior_com_size[, -1]
posterior_hh_size <- posterior_hh_size[, -1]

## Now combine the dataframes
posterior_com_dist_combined <- bind_cols(posterior_com_dist, posterior_com_dist_v2)
posterior_com_size_combined <- bind_cols(posterior_com_size, posterior_com_size_v2)
posterior_hh_size_combined <- bind_cols(posterior_hh_size, posterior_hh_size_v2)


### Now, PLOTTING TIME

### Plotting posteriors now for distance to market town across items
com_dist_density_plots <- mcmc_areas(
  posterior_com_dist_combined,
  prob = 0.90,     # inner interval
  prob_outer = 0.95, # outer interval
  point_est = "mean"
) +
  geom_vline(xintercept = 1.00, color = "black", linetype = "solid", size = 0.6) +
  scale_x_continuous(breaks = seq(0.5, 1.75, by = 0.25), limits = c(0.5, 1.75)) +
  labs(title = "(a) Distance to Market Town") +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 16),
    axis.text.y = element_text(family = "Helvetica", size = 18))


## Plotting posteriors now for community size across items
com_size_density_plots <- mcmc_areas(
  posterior_com_size_combined,
  prob = 0.90,     # inner interval
  prob_outer = 0.95, # outer interval
  point_est = "mean"
) +
  geom_vline(xintercept = 1.00, color = "black", linetype = "solid", size = 0.6) +
  scale_x_continuous(breaks = seq(0.5, 1.75, by = 0.25), limits = c(0.5, 1.75)) +
  labs(title = "(b) Community Size") +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 16),
    axis.text.y = element_text(family = "Helvetica", size = 18))


## Plotting posteriors now for household size across items
hh_size_density_plots <- mcmc_areas(
  posterior_hh_size_combined,
  prob = 0.90,     # inner interval
  prob_outer = 0.95, # outer interval
  point_est = "mean"
) +
  geom_vline(xintercept = 1.00, color = "black", linetype = "solid", size = 0.6) +
  scale_x_continuous(breaks = seq(0.5, 1.75, by = 0.25), limits = c(0.5, 1.75)) + 
  labs(title = "(c) Household Size") +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 16),
    axis.text.y = element_text(family = "Helvetica", size = 18))

##### Combine all plots into 1 plot
post_plots_combined <- (com_dist_density_plots | com_size_density_plots | hh_size_density_plots)


######## Summarizing all model outputs in one table (Item 1 - Item 4)
#########################################################

### Okay, let's quickly calculate the ICC for the residual, individual and region for Item 1
i1_icc_residual <- ( ((pi)^2)/3) / ( 
  (i1$sd_Region__D1Sad_Intercept)^2 + 
    (i1$`sd_Region:ComID__D1Sad_Intercept`)^2 + 
    (i1$`sd_Region:ComID:UniqueFamID__D1Sad_Intercept`)^2 + 
    (i1$`sd_Region:ComID:UniqueFamID:PID__D1Sad_Intercept`)^2 + 
    (i1$sd_Interviewer__D1Sad_Intercept)^2  +  
    (((pi)^2)/3)
  )


i1_icc_indi <- ((i1$`sd_Region:ComID:UniqueFamID:PID__D1Sad_Intercept`)^2) / ((i1$sd_Region__D1Sad_Intercept)^2 + (i1$`sd_Region:ComID__D1Sad_Intercept`)^2 + (i1$`sd_Region:ComID:UniqueFamID__D1Sad_Intercept`)^2 + 
                                                                          (i1$`sd_Region:ComID:UniqueFamID:PID__D1Sad_Intercept`)^2 + (i1$sd_Interviewer__D1Sad_Intercept)^2  + (((pi)^2)/3))

i1_icc_reg <- ((i1$`sd_Region:ComID__D1Sad_Intercept`)^2) / ((i1$sd_Region__D1Sad_Intercept)^2 + (i1$`sd_Region:ComID__D1Sad_Intercept`)^2 + (i1$`sd_Region:ComID:UniqueFamID__D1Sad_Intercept`)^2 + 
                                                                                (i1$`sd_Region:ComID:UniqueFamID:PID__D1Sad_Intercept`)^2 + (i1$sd_Interviewer__D1Sad_Intercept)^2  + (((pi)^2)/3))

### Now the same for Item 2

i2_icc_residual <- ( ((pi)^2)/3) / ( 
  (i2$sd_Region__D2Cry_Intercept)^2 + 
    (i2$`sd_Region:ComID__D2Cry_Intercept`)^2 + 
    (i2$`sd_Region:ComID:UniqueFamID__D2Cry_Intercept`)^2 + 
    (i2$`sd_Region:ComID:UniqueFamID:PID__D2Cry_Intercept`)^2 + 
    (i2$sd_Interviewer__D2Cry_Intercept)^2  +  
    (((pi)^2)/3)
)

i2_icc_indi <- ((i2$`sd_Region:ComID:UniqueFamID:PID__D2Cry_Intercept`)^2) / ((i2$sd_Region__D2Cry_Intercept)^2 + (i2$`sd_Region:ComID__D2Cry_Intercept`)^2 + (i2$`sd_Region:ComID:UniqueFamID__D2Cry_Intercept`)^2 + 
                                                                                (i2$`sd_Region:ComID:UniqueFamID:PID__D2Cry_Intercept`)^2 + (i2$sd_Interviewer__D2Cry_Intercept)^2  + (((pi)^2)/3))

i2_icc_reg <- ((i2$`sd_Region:ComID__D2Cry_Intercept`)^2) / ((i2$sd_Region__D2Cry_Intercept)^2 + (i2$`sd_Region:ComID__D2Cry_Intercept`)^2 + (i2$`sd_Region:ComID:UniqueFamID__D2Cry_Intercept`)^2 + 
                                                               (i2$`sd_Region:ComID:UniqueFamID:PID__D2Cry_Intercept`)^2 + (i2$sd_Interviewer__D2Cry_Intercept)^2  + (((pi)^2)/3))


## Now for Item 3
i3_icc_residual <- ( ((pi)^2)/3) / ( 
  (i3$sd_Region__D3SelfCritical_Intercept)^2 + 
    (i3$`sd_Region:ComID__D3SelfCritical_Intercept`)^2 + 
    (i3$`sd_Region:ComID:UniqueFamID__D3SelfCritical_Intercept`)^2 + 
    (i3$`sd_Region:ComID:UniqueFamID:PID__D3SelfCritical_Intercept`)^2 + 
    (i3$sd_Interviewer__D3SelfCritical_Intercept)^2  +  
    (((pi)^2)/3)
)


i3_icc_indi <- ((i3$`sd_Region:ComID:UniqueFamID:PID__D3SelfCritical_Intercept`)^2) / ((i3$sd_Region__D3SelfCritical_Intercept)^2 + (i3$`sd_Region:ComID__D3SelfCritical_Intercept`)^2 + (i3$`sd_Region:ComID:UniqueFamID__D3SelfCritical_Intercept`)^2 + 
                                                                                         (i3$`sd_Region:ComID:UniqueFamID:PID__D3SelfCritical_Intercept`)^2 + (i3$sd_Interviewer__D3SelfCritical_Intercept)^2  + (((pi)^2)/3))

i3_icc_reg <- ((i3$`sd_Region:ComID__D3SelfCritical_Intercept`)^2) / ((i3$sd_Region__D3SelfCritical_Intercept)^2 + (i3$`sd_Region:ComID__D3SelfCritical_Intercept`)^2 + (i3$`sd_Region:ComID:UniqueFamID__D3SelfCritical_Intercept`)^2 + 
                                                                        (i3$`sd_Region:ComID:UniqueFamID:PID__D3SelfCritical_Intercept`)^2 + (i3$sd_Interviewer__D3SelfCritical_Intercept)^2  + (((pi)^2)/3))

## Now the same for Item 4
i4_icc_residual <- ( ((pi)^2)/3) / ( 
  (i4$sd_Region__D4LifeNotValuable_Intercept)^2 + 
    (i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`)^2 + 
    (i4$`sd_Region:ComID:UniqueFamID__D4LifeNotValuable_Intercept`)^2 + 
    (i4$`sd_Region:ComID:UniqueFamID:PID__D4LifeNotValuable_Intercept`)^2 + 
    (i4$sd_Interviewer__D4LifeNotValuable_Intercept)^2  +  
    (((pi)^2)/3)
)


i4_icc_indi <- ((i4$`sd_Region:ComID:UniqueFamID:PID__D4LifeNotValuable_Intercept`)^2) / ((i4$sd_Region__D4LifeNotValuable_Intercept)^2 + (i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`)^2 + (i4$`sd_Region:ComID:UniqueFamID__D4LifeNotValuable_Intercept`)^2 + 
                                                                                            (i4$`sd_Region:ComID:UniqueFamID:PID__D4LifeNotValuable_Intercept`)^2 + (i4$sd_Interviewer__D4LifeNotValuable_Intercept)^2  + (((pi)^2)/3))

i4_icc_reg <- ((i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`)^2) / ((i4$sd_Region__D4LifeNotValuable_Intercept)^2 + (i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`)^2 + (i4$`sd_Region:ComID:UniqueFamID__D4LifeNotValuable_Intercept`)^2 + 
                                                                           (i4$`sd_Region:ComID:UniqueFamID:PID__D4LifeNotValuable_Intercept`)^2 + (i4$sd_Interviewer__D4LifeNotValuable_Intercept)^2  + (((pi)^2)/3))


### Okay, let's quickly calculate the ICC for the residual, individual and region for Item 5
i5_icc_residual <- ( ((pi)^2)/3) / ( 
  (i5$sd_Region__D5Useless_Intercept)^2 + 
    (i5$`sd_Region:ComID__D5Useless_Intercept`)^2 + 
    (i5$`sd_Region:ComID:UniqueFamID__D5Useless_Intercept`)^2 + 
    (i5$`sd_Region:ComID:UniqueFamID:PID__D5Useless_Intercept`)^2 + 
    (i5$sd_Interviewer__D5Useless_Intercept)^2  +  
    (((pi)^2)/3)
)

i5_icc_indi <- ((i5$`sd_Region:ComID:UniqueFamID:PID__D5Useless_Intercept`)^2) / ((i5$sd_Region__D5Useless_Intercept)^2 + (i5$`sd_Region:ComID__D5Useless_Intercept`)^2 + (i5$`sd_Region:ComID:UniqueFamID__D5Useless_Intercept`)^2 + 
                                                                                    (i5$`sd_Region:ComID:UniqueFamID:PID__D5Useless_Intercept`)^2 + (i5$sd_Interviewer__D5Useless_Intercept)^2  + (((pi)^2)/3))

i5_icc_reg <- ((i5$`sd_Region:ComID__D5Useless_Intercept`)^2) / ((i5$sd_Region__D5Useless_Intercept)^2 + (i5$`sd_Region:ComID__D5Useless_Intercept`)^2 + (i5$`sd_Region:ComID:UniqueFamID__D5Useless_Intercept`)^2 + 
                                                                   (i5$`sd_Region:ComID:UniqueFamID:PID__D5Useless_Intercept`)^2 + (i5$sd_Interviewer__D5Useless_Intercept)^2  + (((pi)^2)/3))

### Now the same for Item 6

i6_icc_residual <- ( ((pi)^2)/3) / ( 
  (i6$sd_Region__D6NoInterest_Intercept)^2 + 
    (i6$`sd_Region:ComID__D6NoInterest_Intercept`)^2 + 
    (i6$`sd_Region:ComID:UniqueFamID__D6NoInterest_Intercept`)^2 + 
    (i6$`sd_Region:ComID:UniqueFamID:PID__D6NoInterest_Intercept`)^2 + 
    (i6$sd_Interviewer__D6NoInterest_Intercept)^2  +  
    (((pi)^2)/3)
)

i6_icc_indi <- ((i6$`sd_Region:ComID:UniqueFamID:PID__D6NoInterest_Intercept`)^2) / ((i6$sd_Region__D6NoInterest_Intercept)^2 + (i6$`sd_Region:ComID__D6NoInterest_Intercept`)^2 + (i6$`sd_Region:ComID:UniqueFamID__D6NoInterest_Intercept`)^2 + 
                                                                                       (i6$`sd_Region:ComID:UniqueFamID:PID__D6NoInterest_Intercept`)^2 + (i6$sd_Interviewer__D6NoInterest_Intercept)^2  + (((pi)^2)/3))

i6_icc_reg <- ((i6$`sd_Region:ComID__D6NoInterest_Intercept`)^2) / ((i6$sd_Region__D6NoInterest_Intercept)^2 + (i6$`sd_Region:ComID__D6NoInterest_Intercept`)^2 + (i6$`sd_Region:ComID:UniqueFamID__D6NoInterest_Intercept`)^2 + 
                                                                      (i6$`sd_Region:ComID:UniqueFamID:PID__D6NoInterest_Intercept`)^2 + (i6$sd_Interviewer__D6NoInterest_Intercept)^2  + (((pi)^2)/3))


## Now for Item 7
i7_icc_residual <- ( ((pi)^2)/3) / ( 
  (i7$sd_Region__D7Tired_Intercept)^2 + 
    (i7$`sd_Region:ComID__D7Tired_Intercept`)^2 + 
    (i7$`sd_Region:ComID:UniqueFamID__D7Tired_Intercept`)^2 + 
    (i7$`sd_Region:ComID:UniqueFamID:PID__D7Tired_Intercept`)^2 + 
    (i7$sd_Interviewer__D7Tired_Intercept)^2  +  
    (((pi)^2)/3)
)


i7_icc_indi <- ((i7$`sd_Region:ComID:UniqueFamID:PID__D7Tired_Intercept`)^2) / ((i7$sd_Region__D7Tired_Intercept)^2 + (i7$`sd_Region:ComID__D7Tired_Intercept`)^2 + (i7$`sd_Region:ComID:UniqueFamID__D7Tired_Intercept`)^2 + 
                                                                                  (i7$`sd_Region:ComID:UniqueFamID:PID__D7Tired_Intercept`)^2 + (i7$sd_Interviewer__D7Tired_Intercept)^2  + (((pi)^2)/3))

i7_icc_reg <- ((i7$`sd_Region:ComID__D7Tired_Intercept`)^2) / ((i7$sd_Region__D7Tired_Intercept)^2 + (i7$`sd_Region:ComID__D7Tired_Intercept`)^2 + (i7$`sd_Region:ComID:UniqueFamID__D7Tired_Intercept`)^2 + 
                                                                 (i7$`sd_Region:ComID:UniqueFamID:PID__D7Tired_Intercept`)^2 + (i7$sd_Interviewer__D7Tired_Intercept)^2  + (((pi)^2)/3))

## Now the same for Item 8
i8_icc_residual <- ( ((pi)^2)/3) / ( 
  (i8$sd_Region__D8CantConcentrate_Intercept)^2 + 
    (i8$`sd_Region:ComID__D8CantConcentrate_Intercept`)^2 + 
    (i8$`sd_Region:ComID:UniqueFamID__D8CantConcentrate_Intercept`)^2 + 
    (i8$`sd_Region:ComID:UniqueFamID:PID__D8CantConcentrate_Intercept`)^2 + 
    (i8$sd_Interviewer__D8CantConcentrate_Intercept)^2  +  
    (((pi)^2)/3)
)


i8_icc_indi <- ((i8$`sd_Region:ComID:UniqueFamID:PID__D8CantConcentrate_Intercept`)^2) / ((i8$sd_Region__D8CantConcentrate_Intercept)^2 + (i8$`sd_Region:ComID__D8CantConcentrate_Intercept`)^2 + (i8$`sd_Region:ComID:UniqueFamID__D8CantConcentrate_Intercept`)^2 + 
                                                                                            (i8$`sd_Region:ComID:UniqueFamID:PID__D8CantConcentrate_Intercept`)^2 + (i8$sd_Interviewer__D8CantConcentrate_Intercept)^2  + (((pi)^2)/3))

i8_icc_reg <- ((i8$`sd_Region:ComID__D8CantConcentrate_Intercept`)^2) / ((i8$sd_Region__D8CantConcentrate_Intercept)^2 + (i8$`sd_Region:ComID__D8CantConcentrate_Intercept`)^2 + (i8$`sd_Region:ComID:UniqueFamID__D8CantConcentrate_Intercept`)^2 + 
                                                                           (i8$`sd_Region:ComID:UniqueFamID:PID__D8CantConcentrate_Intercept`)^2 + (i8$sd_Interviewer__D8CantConcentrate_Intercept)^2  + (((pi)^2)/3))




######## Now making the dataframe (Item 1 - Item 4)
#########################################################
models_summary_i1_i4 <- data.frame( Predictors = c("Intercept[1]", 
                                                   "Intercept[2]", 
                                                   "Intercept[3]", 
                                                   "Age", 
                                                   "Sex", 
                                                   "Social Conflict Index (SCI)", 
                                                   
                                                   "Spanish Fluency", 
                                                   "Interview Date (Spline Fixed Effect)", 
                                                   "Interview Date (Spline Smooth Term)",
                                                   "Distance to Town", 
                                                   "Maximum Community Size", 
                                                   "Household Size", 
                                                   "Residual Standard Deviation", 
                                                   
                                                   "Between-Cluster Standard Deviation (in log-odds)", 
                                                   " ", 
                                                   " ", 
                                                   " ", 
                                                   " ", 
                                                   " ",
                                                   
                                                   " ICC", 
                                                   " ", 
                                                   " ", 
                                                   " ", 
                                                   " ", 
                                                   " ",
                                                   " "),
                                   
                                    Estimates_Model_1 = c( round( mean(i1$`b_D1Sad_Intercept[1]`), 2), 
                                                           round( mean(i1$`b_D1Sad_Intercept[2]`), 2), 
                                                           round( mean(i1$`b_D1Sad_Intercept[3]`), 2), 
                                                           round( mean(exp(i1$b_D1Sad_Age)), 2), 
                                                           round( mean(exp(i1$b_D1Sad_Sex1)), 2), 
                                                           round( mean(exp(i1$bsp_D1Sad_miSCI)), 2),
                                                          
                                                           round( mean(exp(i1$bsp_D1Sad_miSFluency)),2), 
                                                           round( mean(i1$bs_D1Sad_sInterview_Date_1),2), 
                                                           round( mean(i1$sds_D1Sad_sInterview_Date_1), 2), 
                                                           round( mean(exp(i1$b_D1Sad_Town_Distance)), 2), 
                                                           round( mean(exp(i1$b_D1Sad_Com_Size)), 2), 
                                                           round( mean(exp(i1$bsp_D1Sad_miHH_Size)), 2), 
                                                           round((((pi)^2)/3), 2),
                                                           
                                                           " ",
                                                           round( mean(i1$`sd_Region:ComID:UniqueFamID:PID__D1Sad_Intercept`), 2), 
                                                           round( mean(i1$`sd_Region:ComID:UniqueFamID__D1Sad_Intercept`), 2), 
                                                           round( mean(i1$`sd_Region:ComID__D1Sad_Intercept`), 2), 
                                                           round( mean(i1$sd_Region__D1Sad_Intercept), 2), 
                                                           round( mean(i1$sd_Interviewer__D1Sad_Intercept), 2),
                                                           
                                                           "-",
                                                           round( mean(i1_icc_residual), 2),
                                                           round( mean(i1_icc_indi), 2),
                                                           round( mean(icc_hh_i1),2), 
                                                           round( mean(icc_com_i1), 2), 
                                                           round( mean(i1_icc_reg), 2),
                                                           round(mean(icc_int_i1), 2)
                                                        ),
                                    
                                    CI_Model_1 = c ( paste( round( quantile(i1$`b_D1Sad_Intercept[1]`, 0.025), 2), "-", round( quantile(i1$`b_D1Sad_Intercept[1]`, 0.975), 2)),
                                                     paste( round( quantile(i1$`b_D1Sad_Intercept[2]`, 0.025), 2), "-", round( quantile(i1$`b_D1Sad_Intercept[2]`, 0.975), 2)),
                                                     paste( round( quantile(i1$`b_D1Sad_Intercept[3]`, 0.025), 2), "-", round( quantile(i1$`b_D1Sad_Intercept[3]`, 0.975), 2)),
                                                     paste( round( quantile( exp(i1$b_D1Sad_Age), 0.025), 2), "-", round( quantile( exp(i1$b_D1Sad_Age), 0.975), 2)),
                                                     paste( round( quantile( exp(i1$b_D1Sad_Sex1), 0.025), 2), "-", round( quantile( exp(i1$b_D1Sad_Sex1), 0.975), 2)),
                                                     paste( round( quantile( exp(i1$bsp_D1Sad_miSCI), 0.025), 2), "-", round( quantile( exp(i1$bsp_D1Sad_miSCI), 0.975), 2)),
                                                     
                                                     paste( round( quantile( exp(i1$bsp_D1Sad_miSFluency), 0.025), 2), "-", round( quantile( exp(i1$bsp_D1Sad_miSFluency), 0.975), 2)),
                                                     paste( round( quantile( i1$bs_D1Sad_sInterview_Date_1, 0.025), 2), "-", round( quantile( i1$bs_D1Sad_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( i1$sds_D1Sad_sInterview_Date_1, 0.025), 2), "-", round( quantile( i1$sds_D1Sad_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( exp(i1$b_D1Sad_Town_Distance), 0.025), 2), "-", round( quantile( exp(i1$b_D1Sad_Town_Distance), 0.975), 2)),
                                                     paste( round( quantile( exp(i1$b_D1Sad_Com_Size), 0.025), 2), "-", round( quantile( exp(i1$b_D1Sad_Com_Size), 0.975), 2)),
                                                     paste( round( quantile( exp(i1$bsp_D1Sad_miHH_Size), 0.025), 2), "-", round( quantile( exp(i1$bsp_D1Sad_miHH_Size), 0.975), 2)),
                                                     " ",
                                                     
                                                     " ",
                                                     paste( round( quantile( i1$`sd_Region:ComID:UniqueFamID:PID__D1Sad_Intercept`, 0.025), 2), "-", round( quantile( i1$`sd_Region:ComID:UniqueFamID:PID__D1Sad_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i1$`sd_Region:ComID:UniqueFamID__D1Sad_Intercept`, 0.025), 2), "-", round( quantile( i1$`sd_Region:ComID:UniqueFamID__D1Sad_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i1$`sd_Region:ComID__D1Sad_Intercept`, 0.025), 2), "-", round( quantile( i1$`sd_Region:ComID__D1Sad_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i1$sd_Region__D1Sad_Intercept, 0.025), 2), "-", round( quantile( i1$sd_Region__D1Sad_Intercept, 0.975), 2)),
                                                     paste( round( quantile( i1$sd_Interviewer__D1Sad_Intercept, 0.025), 2), "-", round( quantile( i1$sd_Interviewer__D1Sad_Intercept, 0.975), 2)),
                                                     
                                                     "-",
                                                     paste( round( quantile( i1_icc_residual, 0.025), 2), "-", round( quantile( i1_icc_residual, 0.975), 2)),
                                                     paste( round( quantile( i1_icc_indi, 0.025), 2), "-", round( quantile( i1_icc_indi, 0.975), 2)),
                                                     paste( round( quantile( icc_hh_i1, 0.025), 2), "-", round( quantile( icc_hh_i1, 0.975), 2)),
                                                     paste( round( quantile( icc_com_i1, 0.025), 2), "-", round( quantile( icc_com_i1, 0.975), 2)),
                                                     paste( round( quantile( i1_icc_reg, 0.025), 2), "-", round( quantile( i1_icc_reg, 0.975), 2)),
                                                     paste( round( quantile( icc_int_i1, 0.025), 2), "-", round( quantile( icc_int_i1, 0.975), 2))
                                                   ),
                                    
                                    Estimates_Model_2 = c( round( mean(i2$`b_D2Cry_Intercept[1]`), 2), 
                                                           round( mean(i2$`b_D2Cry_Intercept[2]`), 2), 
                                                           round( mean(i2$`b_D2Cry_Intercept[3]`), 2), 
                                                           round( mean(exp(i2$b_D2Cry_Age)), 2), 
                                                           round( mean(exp(i2$b_D2Cry_Sex1)), 2), 
                                                           round( mean(exp(i2$bsp_D2Cry_miSCI)), 2),
                                                           
                                                           round( mean(exp(i2$bsp_D2Cry_miSFluency)),2), 
                                                           round( mean(i2$bs_D2Cry_sInterview_Date_1),2), 
                                                           round( mean(i2$sds_D2Cry_sInterview_Date_1), 2), 
                                                           round( mean(exp(i2$b_D2Cry_Town_Distance)), 2), 
                                                           round( mean(exp(i2$b_D2Cry_Com_Size)), 2), 
                                                           round( mean(exp(i2$bsp_D2Cry_miHH_Size)), 2), 
                                                           round((((pi)^2)/3), 2),
                                                           
                                                           " ",
                                                           round( mean(i2$`sd_Region:ComID:UniqueFamID:PID__D2Cry_Intercept`), 2), 
                                                           round( mean(i2$`sd_Region:ComID:UniqueFamID__D2Cry_Intercept`), 2), 
                                                           round( mean(i2$`sd_Region:ComID__D2Cry_Intercept`), 2), 
                                                           round( mean(i2$sd_Region__D2Cry_Intercept), 2), 
                                                           round( mean(i2$sd_Interviewer__D2Cry_Intercept), 2),
                                                           
                                                           "-",
                                                           round( mean(i2_icc_residual), 2),
                                                           round( mean(i2_icc_indi), 2),
                                                           round( mean(icc_hh_i2),2), 
                                                           round( mean(icc_com_i2), 2), 
                                                           round( mean(i2_icc_reg), 2),
                                                           round(mean(icc_int_i2), 2)
                                    ),
                                    
                                    CI_Model_2 = c ( paste( round( quantile(i2$`b_D2Cry_Intercept[1]`, 0.025), 2), "-", round( quantile(i2$`b_D2Cry_Intercept[1]`, 0.975), 2)),
                                                     paste( round( quantile(i2$`b_D2Cry_Intercept[2]`, 0.025), 2), "-", round( quantile(i2$`b_D2Cry_Intercept[2]`, 0.975), 2)),
                                                     paste( round( quantile(i2$`b_D2Cry_Intercept[3]`, 0.025), 2), "-", round( quantile(i2$`b_D2Cry_Intercept[3]`, 0.975), 2)),
                                                     paste( round( quantile( exp(i2$b_D2Cry_Age), 0.025), 2), "-", round( quantile( exp(i2$b_D2Cry_Age), 0.975), 2)),
                                                     paste( round( quantile( exp(i2$b_D2Cry_Sex1), 0.025), 2), "-", round( quantile( exp(i2$b_D2Cry_Sex1), 0.975), 2)),
                                                     paste( round( quantile( exp(i2$bsp_D2Cry_miSCI), 0.025), 2), "-", round( quantile( exp(i2$bsp_D2Cry_miSCI), 0.975), 2)),
                                                     
                                                     paste( round( quantile( exp(i2$bsp_D2Cry_miSFluency), 0.025), 2), "-", round( quantile( exp(i2$bsp_D2Cry_miSFluency), 0.975), 2)),
                                                     paste( round( quantile( i2$bs_D2Cry_sInterview_Date_1, 0.025), 2), "-", round( quantile( i2$bs_D2Cry_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( i2$sds_D2Cry_sInterview_Date_1, 0.025), 2), "-", round( quantile( i2$sds_D2Cry_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( exp(i2$b_D2Cry_Town_Distance), 0.025), 2), "-", round( quantile( exp(i2$b_D2Cry_Town_Distance), 0.975), 2)),
                                                     paste( round( quantile( exp(i2$b_D2Cry_Com_Size), 0.025), 2), "-", round( quantile( exp(i2$b_D2Cry_Com_Size), 0.975), 2)),
                                                     paste( round( quantile( exp(i2$bsp_D2Cry_miHH_Size), 0.025), 2), "-", round( quantile( exp(i2$bsp_D2Cry_miHH_Size), 0.975), 2)),
                                                     " ",
                                                     
                                                     " ",
                                                     paste( round( quantile( i2$`sd_Region:ComID:UniqueFamID:PID__D2Cry_Intercept`, 0.025), 2), "-", round( quantile( i2$`sd_Region:ComID:UniqueFamID:PID__D2Cry_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i2$`sd_Region:ComID:UniqueFamID__D2Cry_Intercept`, 0.025), 2), "-", round( quantile( i2$`sd_Region:ComID:UniqueFamID__D2Cry_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i2$`sd_Region:ComID__D2Cry_Intercept`, 0.025), 2), "-", round( quantile( i2$`sd_Region:ComID__D2Cry_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i2$sd_Region__D2Cry_Intercept, 0.025), 2), "-", round( quantile( i2$sd_Region__D2Cry_Intercept, 0.975), 2)),
                                                     paste( round( quantile( i2$sd_Interviewer__D2Cry_Intercept, 0.025), 2), "-", round( quantile( i2$sd_Interviewer__D2Cry_Intercept, 0.975), 2)),
                                                     
                                                     "-",
                                                     paste( round( quantile( i2_icc_residual, 0.025), 2), "-", round( quantile( i2_icc_residual, 0.975), 2)),
                                                     paste( round( quantile( i2_icc_indi, 0.025), 2), "-", round( quantile( i2_icc_indi, 0.975), 2)),
                                                     paste( round( quantile( icc_hh_i2, 0.025), 2), "-", round( quantile( icc_hh_i2, 0.975), 2)),
                                                     paste( round( quantile( icc_com_i2, 0.025), 2), "-", round( quantile( icc_com_i2, 0.975), 2)),
                                                     paste( round( quantile( i2_icc_reg, 0.025), 2), "-", round( quantile( i2_icc_reg, 0.975), 2)),
                                                     paste( round( quantile( icc_int_i2, 0.025), 2), "-", round( quantile( icc_int_i2, 0.975), 2))
                                    ),
                                    
                                    Estimates_Model_3 = c( round( mean(i3$`b_D3SelfCritical_Intercept[1]`), 2), 
                                                           round( mean(i3$`b_D3SelfCritical_Intercept[2]`), 2), 
                                                           round( mean(i3$`b_D3SelfCritical_Intercept[3]`), 2), 
                                                           round( mean(exp(i3$b_D3SelfCritical_Age)), 2), 
                                                           round( mean(exp(i3$b_D3SelfCritical_Sex1)), 2), 
                                                           round( mean(exp(i3$bsp_D3SelfCritical_miSCI)), 2),
                                                           
                                                           round( mean(exp(i3$bsp_D3SelfCritical_miSFluency)), 2), 
                                                           round( mean(i3$bs_D3SelfCritical_sInterview_Date_1), 2), 
                                                           round( mean(i3$sds_D3SelfCritical_sInterview_Date_1), 2), 
                                                           round( mean(exp(i3$b_D3SelfCritical_Town_Distance)), 2), 
                                                           round( mean(exp(i3$b_D3SelfCritical_Com_Size)), 2), 
                                                           round( mean(exp(i3$bsp_D3SelfCritical_miHH_Size)), 2), 
                                                           round((((pi)^2)/3), 2),
                                                           
                                                           " ",
                                                           round( mean(i3$`sd_Region:ComID:UniqueFamID:PID__D3SelfCritical_Intercept`), 2), 
                                                           round( mean(i3$`sd_Region:ComID:UniqueFamID__D3SelfCritical_Intercept`), 2), 
                                                           round( mean(i3$`sd_Region:ComID__D3SelfCritical_Intercept`), 2), 
                                                           round( mean(i3$sd_Region__D3SelfCritical_Intercept), 2), 
                                                           round( mean(i3$sd_Interviewer__D3SelfCritical_Intercept), 2),
                                                           
                                                           "-",
                                                           round( mean(i3_icc_residual), 2),
                                                           round( mean(i3_icc_indi), 2),
                                                           round( mean(icc_hh_i3), 2), 
                                                           round( mean(icc_com_i3), 2), 
                                                           round( mean(i3_icc_reg), 2),
                                                           round(mean(icc_int_i3), 2)
                                    ),
                                    
                                    CI_Model_3 = c ( paste( round( quantile(i3$`b_D3SelfCritical_Intercept[1]`, 0.025), 2), "-", round( quantile(i3$`b_D3SelfCritical_Intercept[1]`, 0.975), 2)),
                                                     paste( round( quantile(i3$`b_D3SelfCritical_Intercept[2]`, 0.025), 2), "-", round( quantile(i3$`b_D3SelfCritical_Intercept[2]`, 0.975), 2)),
                                                     paste( round( quantile(i3$`b_D3SelfCritical_Intercept[3]`, 0.025), 2), "-", round( quantile(i3$`b_D3SelfCritical_Intercept[3]`, 0.975), 2)),
                                                     paste( round( quantile( exp(i3$b_D3SelfCritical_Age), 0.025), 2), "-", round( quantile( exp(i3$b_D3SelfCritical_Age), 0.975), 2)),
                                                     paste( round( quantile( exp(i3$b_D3SelfCritical_Sex1), 0.025), 2), "-", round( quantile( exp(i3$b_D3SelfCritical_Sex1), 0.975), 2)),
                                                     paste( round( quantile( exp(i3$bsp_D3SelfCritical_miSCI), 0.025), 2), "-", round( quantile( exp(i3$bsp_D3SelfCritical_miSCI), 0.975), 2)),
                                                     
                                                     paste( round( quantile( exp(i3$bsp_D3SelfCritical_miSFluency), 0.025), 2), "-", round( quantile( exp(i3$bsp_D3SelfCritical_miSFluency), 0.975), 2)),
                                                     paste( round( quantile( i3$bs_D3SelfCritical_sInterview_Date_1, 0.025), 2), "-", round( quantile( i3$bs_D3SelfCritical_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( i3$sds_D3SelfCritical_sInterview_Date_1, 0.025), 2), "-", round( quantile( i3$sds_D3SelfCritical_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( exp(i3$b_D3SelfCritical_Town_Distance), 0.025), 2), "-", round( quantile( exp(i3$b_D3SelfCritical_Town_Distance), 0.975), 2)),
                                                     paste( round( quantile( exp(i3$b_D3SelfCritical_Com_Size), 0.025), 2), "-", round( quantile( exp(i3$b_D3SelfCritical_Com_Size), 0.975), 2)),
                                                     paste( round( quantile( exp(i3$bsp_D3SelfCritical_miHH_Size), 0.025), 2), "-", round( quantile( exp(i3$bsp_D3SelfCritical_miHH_Size), 0.975), 2)),
                                                     " ",
                                                     
                                                     " ",
                                                     paste( round( quantile( i3$`sd_Region:ComID:UniqueFamID:PID__D3SelfCritical_Intercept`, 0.025), 2), "-", round( quantile( i3$`sd_Region:ComID:UniqueFamID:PID__D3SelfCritical_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i3$`sd_Region:ComID:UniqueFamID__D3SelfCritical_Intercept`, 0.025), 2), "-", round( quantile( i3$`sd_Region:ComID:UniqueFamID__D3SelfCritical_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i3$`sd_Region:ComID__D3SelfCritical_Intercept`, 0.025), 2), "-", round( quantile( i3$`sd_Region:ComID__D3SelfCritical_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i3$sd_Region__D3SelfCritical_Intercept, 0.025), 2), "-", round( quantile( i3$sd_Region__D3SelfCritical_Intercept, 0.975), 2)),
                                                     paste( round( quantile( i3$sd_Interviewer__D3SelfCritical_Intercept, 0.025), 2), "-", round( quantile( i3$sd_Interviewer__D3SelfCritical_Intercept, 0.975), 2)),
                                                     
                                                     "-",
                                                     paste( round( quantile( i3_icc_residual, 0.025), 2), "-", round( quantile( i3_icc_residual, 0.975), 2)),
                                                     paste( round( quantile( i3_icc_indi, 0.025), 2), "-", round( quantile( i3_icc_indi, 0.975), 2)),
                                                     paste( round( quantile( icc_hh_i3, 0.025), 2), "-", round( quantile( icc_hh_i3, 0.975), 2)),
                                                     paste( round( quantile( icc_com_i3, 0.025), 2), "-", round( quantile( icc_com_i3, 0.975), 2)),
                                                     paste( round( quantile( i3_icc_reg, 0.025), 2), "-", round( quantile( i3_icc_reg, 0.975), 2)),
                                                     paste( round( quantile( icc_int_i3, 0.025), 2), "-", round( quantile( icc_int_i3, 0.975), 2))
                                    ),
                                    
                                    Estimates_Model_4 = c( round( mean(i4$`b_D4LifeNotValuable_Intercept[1]`), 2), 
                                                           round( mean(i4$`b_D4LifeNotValuable_Intercept[2]`), 2), 
                                                           round( mean(i4$`b_D4LifeNotValuable_Intercept[3]`), 2), 
                                                           round( mean(exp(i4$b_D4LifeNotValuable_Age)), 2), 
                                                           round( mean(exp(i4$b_D4LifeNotValuable_Sex1)), 2), 
                                                           round( mean(exp(i4$bsp_D4LifeNotValuable_miSCI)), 2),
                                                           
                                                           round( mean(exp(i4$bsp_D4LifeNotValuable_miSFluency)),2), 
                                                           round( mean(i4$bs_D4LifeNotValuable_sInterview_Date_1),2), 
                                                           round( mean(i4$sds_D4LifeNotValuable_sInterview_Date_1), 2), 
                                                           round( mean(exp(i4$b_D4LifeNotValuable_Town_Distance)), 2), 
                                                           round( mean(exp(i4$b_D4LifeNotValuable_Com_Size)), 2), 
                                                           round( mean(exp(i4$bsp_D4LifeNotValuable_miHH_Size)), 2), 
                                                           round((((pi)^2)/3), 2),
                                                           
                                                           " ",
                                                           round( mean(i4$`sd_Region:ComID:UniqueFamID:PID__D4LifeNotValuable_Intercept`), 2), 
                                                           round( mean(i4$`sd_Region:ComID:UniqueFamID__D4LifeNotValuable_Intercept`), 2), 
                                                           round( mean(i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`), 2), 
                                                           round( mean(i4$sd_Region__D4LifeNotValuable_Intercept), 2), 
                                                           round( mean(i4$sd_Interviewer__D4LifeNotValuable_Intercept), 2),
                                                           
                                                           "-",
                                                           round( mean(i4_icc_residual), 2),
                                                           round( mean(i4_icc_indi), 2),
                                                           round( mean(icc_hh_i4),2), 
                                                           round( mean(icc_com_i4), 2), 
                                                           round( mean(i4_icc_reg), 2),
                                                           round(mean(icc_int_i4), 2)
                                    ),
                                    
                                    CI_Model_4 = c ( paste( round( quantile(i4$`b_D4LifeNotValuable_Intercept[1]`, 0.025), 2), "-", round( quantile(i4$`b_D4LifeNotValuable_Intercept[1]`, 0.975), 2)),
                                                     paste( round( quantile(i4$`b_D4LifeNotValuable_Intercept[2]`, 0.025), 2), "-", round( quantile(i4$`b_D4LifeNotValuable_Intercept[2]`, 0.975), 2)),
                                                     paste( round( quantile(i4$`b_D4LifeNotValuable_Intercept[3]`, 0.025), 2), "-", round( quantile(i4$`b_D4LifeNotValuable_Intercept[3]`, 0.975), 2)),
                                                     paste( round( quantile( exp(i4$b_D4LifeNotValuable_Age), 0.025), 2), "-", round( quantile( exp(i4$b_D4LifeNotValuable_Age), 0.975), 2)),
                                                     paste( round( quantile( exp(i4$b_D4LifeNotValuable_Sex1), 0.025), 2), "-", round( quantile( exp(i4$b_D4LifeNotValuable_Sex1), 0.975), 2)),
                                                     paste( round( quantile( exp(i4$bsp_D4LifeNotValuable_miSCI), 0.025), 2), "-", round( quantile( exp(i4$bsp_D4LifeNotValuable_miSCI), 0.975), 2)),
                                                     
                                                     paste( round( quantile( exp(i4$bsp_D4LifeNotValuable_miSFluency), 0.025), 2), "-", round( quantile( exp(i4$bsp_D4LifeNotValuable_miSFluency), 0.975), 2)),
                                                     paste( round( quantile( i4$bs_D4LifeNotValuable_sInterview_Date_1, 0.025), 2), "-", round( quantile( i4$bs_D4LifeNotValuable_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( i4$sds_D4LifeNotValuable_sInterview_Date_1, 0.025), 2), "-", round( quantile( i4$sds_D4LifeNotValuable_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( exp(i4$b_D4LifeNotValuable_Town_Distance), 0.025), 2), "-", round( quantile( exp(i4$b_D4LifeNotValuable_Town_Distance), 0.975), 2)),
                                                     paste( round( quantile( exp(i4$b_D4LifeNotValuable_Com_Size), 0.025), 2), "-", round( quantile( exp(i4$b_D4LifeNotValuable_Com_Size), 0.975), 2)),
                                                     paste( round( quantile( exp(i4$bsp_D4LifeNotValuable_miHH_Size), 0.025), 2), "-", round( quantile( exp(i4$bsp_D4LifeNotValuable_miHH_Size), 0.975), 2)),
                                                     " ",
                                                     
                                                     " ",
                                                     paste( round( quantile( i4$`sd_Region:ComID:UniqueFamID:PID__D4LifeNotValuable_Intercept`, 0.025), 2), "-", round( quantile( i4$`sd_Region:ComID:UniqueFamID:PID__D4LifeNotValuable_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i4$`sd_Region:ComID:UniqueFamID__D4LifeNotValuable_Intercept`, 0.025), 2), "-", round( quantile( i4$`sd_Region:ComID:UniqueFamID__D4LifeNotValuable_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`, 0.025), 2), "-", round( quantile( i4$`sd_Region:ComID__D4LifeNotValuable_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i4$sd_Region__D4LifeNotValuable_Intercept, 0.025), 2), "-", round( quantile( i4$sd_Region__D4LifeNotValuable_Intercept, 0.975), 2)),
                                                     paste( round( quantile( i4$sd_Interviewer__D4LifeNotValuable_Intercept, 0.025), 2), "-", round( quantile( i4$sd_Interviewer__D4LifeNotValuable_Intercept, 0.975), 2)),
                                                     
                                                     "-",
                                                     paste( round( quantile( i4_icc_residual, 0.025), 2), "-", round( quantile( i4_icc_residual, 0.975), 2)),
                                                     paste( round( quantile( i4_icc_indi, 0.025), 2), "-", round( quantile( i4_icc_indi, 0.975), 2)),
                                                     paste( round( quantile( icc_hh_i4, 0.025), 2), "-", round( quantile( icc_hh_i4, 0.975), 2)),
                                                     paste( round( quantile( icc_com_i4, 0.025), 2), "-", round( quantile( icc_com_i4, 0.975), 2)),
                                                     paste( round( quantile( i4_icc_reg, 0.025), 2), "-", round( quantile( i4_icc_reg, 0.975), 2)),
                                                     paste( round( quantile( icc_int_i4, 0.025), 2), "-", round( quantile( icc_int_i4, 0.975), 2))
                                    )
                                    
                                    )
                                                         

models_summary_i5_i8 <- data.frame( Predictors = c("Intercept[1]", 
                                                   "Intercept[2]", 
                                                   "Intercept[3]", 
                                                   "Age", 
                                                   "Sex", 
                                                   "Social Conflict Index (SCI)", 
                                                   
                                                   "Spanish Fluency", 
                                                   "Interview Date (Spline Fixed Effect)", 
                                                   "Interview Date (Spline Smooth Term)",
                                                   "Distance to Town", 
                                                   "Maximum Community Size", 
                                                   "Household Size", 
                                                   "Residual Standard Deviation", 
                                                   
                                                   "Between-Cluster Standard Deviation (in log-odds)", 
                                                   " ", 
                                                   " ", 
                                                   " ", 
                                                   " ", 
                                                   " ",
                                                   
                                                   " ICC", 
                                                   " ", 
                                                   " ", 
                                                   " ", 
                                                   " ", 
                                                   " ",
                                                   " "),
                                    
                                    Estimates_Model_5 = c( round( mean(i5$`b_D5Useless_Intercept[1]`), 2), 
                                                           round( mean(i5$`b_D5Useless_Intercept[2]`), 2), 
                                                           round( mean(i5$`b_D5Useless_Intercept[3]`), 2), 
                                                           round( mean(exp(i5$b_D5Useless_Age)), 2), 
                                                           round( mean(exp(i5$b_D5Useless_Sex1)), 2), 
                                                           round( mean(exp(i5$bsp_D5Useless_miSCI)), 2),
                                                           
                                                           round( mean(exp(i5$bsp_D5Useless_miSFluency)),2), 
                                                           round( mean(i5$bs_D5Useless_sInterview_Date_1),2), 
                                                           round( mean(i5$sds_D5Useless_sInterview_Date_1), 2), 
                                                           round( mean(exp(i5$b_D5Useless_Town_Distance)), 2), 
                                                           round( mean(exp(i5$b_D5Useless_Com_Size)), 2), 
                                                           round( mean(exp(i5$bsp_D5Useless_miHH_Size)), 2), 
                                                           round((((pi)^2)/3), 2),
                                                           
                                                           " ",
                                                           round( mean(i5$`sd_Region:ComID:UniqueFamID:PID__D5Useless_Intercept`), 2), 
                                                           round( mean(i5$`sd_Region:ComID:UniqueFamID__D5Useless_Intercept`), 2), 
                                                           round( mean(i5$`sd_Region:ComID__D5Useless_Intercept`), 2), 
                                                           round( mean(i5$sd_Region__D5Useless_Intercept), 2), 
                                                           round( mean(i5$sd_Interviewer__D5Useless_Intercept), 2),
                                                           
                                                           "-",
                                                           round( mean(i5_icc_residual), 2),
                                                           round( mean(i5_icc_indi), 2),
                                                           round( mean(icc_hh_i5),2), 
                                                           round( mean(icc_com_i5), 2), 
                                                           round( mean(i5_icc_reg), 2),
                                                           round(mean(icc_int_i5), 2)
                                    ),
                                    
                                    CI_Model_5 = c ( paste( round( quantile(i5$`b_D5Useless_Intercept[1]`, 0.025), 2), "-", round( quantile(i5$`b_D5Useless_Intercept[1]`, 0.975), 2)),
                                                     paste( round( quantile(i5$`b_D5Useless_Intercept[2]`, 0.025), 2), "-", round( quantile(i5$`b_D5Useless_Intercept[2]`, 0.975), 2)),
                                                     paste( round( quantile(i5$`b_D5Useless_Intercept[3]`, 0.025), 2), "-", round( quantile(i5$`b_D5Useless_Intercept[3]`, 0.975), 2)),
                                                     paste( round( quantile( exp(i5$b_D5Useless_Age), 0.025), 2), "-", round( quantile( exp(i5$b_D5Useless_Age), 0.975), 2)),
                                                     paste( round( quantile( exp(i5$b_D5Useless_Sex1), 0.025), 2), "-", round( quantile( exp(i5$b_D5Useless_Sex1), 0.975), 2)),
                                                     paste( round( quantile( exp(i5$bsp_D5Useless_miSCI), 0.025), 2), "-", round( quantile( exp(i5$bsp_D5Useless_miSCI), 0.975), 2)),
                                                     
                                                     paste( round( quantile( exp(i5$bsp_D5Useless_miSFluency), 0.025), 2), "-", round( quantile( exp(i5$bsp_D5Useless_miSFluency), 0.975), 2)),
                                                     paste( round( quantile( i5$bs_D5Useless_sInterview_Date_1, 0.025), 2), "-", round( quantile( i5$bs_D5Useless_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( i5$sds_D5Useless_sInterview_Date_1, 0.025), 2), "-", round( quantile( i5$sds_D5Useless_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( exp(i5$b_D5Useless_Town_Distance), 0.025), 2), "-", round( quantile( exp(i5$b_D5Useless_Town_Distance), 0.975), 2)),
                                                     paste( round( quantile( exp(i5$b_D5Useless_Com_Size), 0.025), 2), "-", round( quantile( exp(i5$b_D5Useless_Com_Size), 0.975), 2)),
                                                     paste( round( quantile( exp(i5$bsp_D5Useless_miHH_Size), 0.025), 2), "-", round( quantile( exp(i5$bsp_D5Useless_miHH_Size), 0.975), 2)),
                                                     " ",
                                                     
                                                     " ",
                                                     paste( round( quantile( i5$`sd_Region:ComID:UniqueFamID:PID__D5Useless_Intercept`, 0.025), 2), "-", round( quantile( i5$`sd_Region:ComID:UniqueFamID:PID__D5Useless_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i5$`sd_Region:ComID:UniqueFamID__D5Useless_Intercept`, 0.025), 2), "-", round( quantile( i5$`sd_Region:ComID:UniqueFamID__D5Useless_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i5$`sd_Region:ComID__D5Useless_Intercept`, 0.025), 2), "-", round( quantile( i5$`sd_Region:ComID__D5Useless_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i5$sd_Region__D5Useless_Intercept, 0.025), 2), "-", round( quantile( i5$sd_Region__D5Useless_Intercept, 0.975), 2)),
                                                     paste( round( quantile( i5$sd_Interviewer__D5Useless_Intercept, 0.025), 2), "-", round( quantile( i5$sd_Interviewer__D5Useless_Intercept, 0.975), 2)),
                                                     
                                                     "-",
                                                     paste( round( quantile( i5_icc_residual, 0.025), 2), "-", round( quantile( i5_icc_residual, 0.975), 2)),
                                                     paste( round( quantile( i5_icc_indi, 0.025), 2), "-", round( quantile( i5_icc_indi, 0.975), 2)),
                                                     paste( round( quantile( icc_hh_i5, 0.025), 2), "-", round( quantile( icc_hh_i5, 0.975), 2)),
                                                     paste( round( quantile( icc_com_i5, 0.025), 2), "-", round( quantile( icc_com_i5, 0.975), 2)),
                                                     paste( round( quantile( i5_icc_reg, 0.025), 2), "-", round( quantile( i5_icc_reg, 0.975), 2)),
                                                     paste( round( quantile( icc_int_i5, 0.025), 2), "-", round( quantile( icc_int_i5, 0.975), 2))
                                    ),
                                    
                                    Estimates_Model_6 = c( round( mean(i6$`b_D6NoInterest_Intercept[1]`), 2), 
                                                           round( mean(i6$`b_D6NoInterest_Intercept[2]`), 2), 
                                                           round( mean(i6$`b_D6NoInterest_Intercept[3]`), 2), 
                                                           round( mean(exp(i6$b_D6NoInterest_Age)), 2), 
                                                           round( mean(exp(i6$b_D6NoInterest_Sex1)), 2), 
                                                           round( mean(exp(i6$bsp_D6NoInterest_miSCI)), 2),
                                                           
                                                           round( mean(exp(i6$bsp_D6NoInterest_miSFluency)),2), 
                                                           round( mean(i6$bs_D6NoInterest_sInterview_Date_1),2), 
                                                           round( mean(i6$sds_D6NoInterest_sInterview_Date_1), 2), 
                                                           round( mean(exp(i6$b_D6NoInterest_Town_Distance)), 2), 
                                                           round( mean(exp(i6$b_D6NoInterest_Com_Size)), 2), 
                                                           round( mean(exp(i6$bsp_D6NoInterest_miHH_Size)), 2), 
                                                           round((((pi)^2)/3), 2),
                                                           
                                                           " ",
                                                           round( mean(i6$`sd_Region:ComID:UniqueFamID:PID__D6NoInterest_Intercept`), 2), 
                                                           round( mean(i6$`sd_Region:ComID:UniqueFamID__D6NoInterest_Intercept`), 2), 
                                                           round( mean(i6$`sd_Region:ComID__D6NoInterest_Intercept`), 2), 
                                                           round( mean(i6$sd_Region__D6NoInterest_Intercept), 2), 
                                                           round( mean(i6$sd_Interviewer__D6NoInterest_Intercept), 2),
                                                           
                                                           "-",
                                                           round( mean(i6_icc_residual), 2),
                                                           round( mean(i6_icc_indi), 2),
                                                           round( mean(icc_hh_i6),2), 
                                                           round( mean(icc_com_i6), 2), 
                                                           round( mean(i6_icc_reg), 2),
                                                           round(mean(icc_int_i6), 2)
                                    ),
                                    
                                    CI_Model_6 = c ( paste( round( quantile(i6$`b_D6NoInterest_Intercept[1]`, 0.025), 2), "-", round( quantile(i6$`b_D6NoInterest_Intercept[1]`, 0.975), 2)),
                                                     paste( round( quantile(i6$`b_D6NoInterest_Intercept[2]`, 0.025), 2), "-", round( quantile(i6$`b_D6NoInterest_Intercept[2]`, 0.975), 2)),
                                                     paste( round( quantile(i6$`b_D6NoInterest_Intercept[3]`, 0.025), 2), "-", round( quantile(i6$`b_D6NoInterest_Intercept[3]`, 0.975), 2)),
                                                     paste( round( quantile( exp(i6$b_D6NoInterest_Age), 0.025), 2), "-", round( quantile( exp(i6$b_D6NoInterest_Age), 0.975), 2)),
                                                     paste( round( quantile( exp(i6$b_D6NoInterest_Sex1), 0.025), 2), "-", round( quantile( exp(i6$b_D6NoInterest_Sex1), 0.975), 2)),
                                                     paste( round( quantile( exp(i6$bsp_D6NoInterest_miSCI), 0.025), 2), "-", round( quantile( exp(i6$bsp_D6NoInterest_miSCI), 0.975), 2)),
                                                     
                                                     paste( round( quantile( exp(i6$bsp_D6NoInterest_miSFluency), 0.025), 2), "-", round( quantile( exp(i6$bsp_D6NoInterest_miSFluency), 0.975), 2)),
                                                     paste( round( quantile( i6$bs_D6NoInterest_sInterview_Date_1, 0.025), 2), "-", round( quantile( i6$bs_D6NoInterest_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( i6$sds_D6NoInterest_sInterview_Date_1, 0.025), 2), "-", round( quantile( i6$sds_D6NoInterest_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( exp(i6$b_D6NoInterest_Town_Distance), 0.025), 2), "-", round( quantile( exp(i6$b_D6NoInterest_Town_Distance), 0.975), 2)),
                                                     paste( round( quantile( exp(i6$b_D6NoInterest_Com_Size), 0.025), 2), "-", round( quantile( exp(i6$b_D6NoInterest_Com_Size), 0.975), 2)),
                                                     paste( round( quantile( exp(i6$bsp_D6NoInterest_miHH_Size), 0.025), 2), "-", round( quantile( exp(i6$bsp_D6NoInterest_miHH_Size), 0.975), 2)),
                                                     " ",
                                                     
                                                     " ",
                                                     paste( round( quantile( i6$`sd_Region:ComID:UniqueFamID:PID__D6NoInterest_Intercept`, 0.025), 2), "-", round( quantile( i6$`sd_Region:ComID:UniqueFamID:PID__D6NoInterest_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i6$`sd_Region:ComID:UniqueFamID__D6NoInterest_Intercept`, 0.025), 2), "-", round( quantile( i6$`sd_Region:ComID:UniqueFamID__D6NoInterest_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i6$`sd_Region:ComID__D6NoInterest_Intercept`, 0.025), 2), "-", round( quantile( i6$`sd_Region:ComID__D6NoInterest_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i6$sd_Region__D6NoInterest_Intercept, 0.025), 2), "-", round( quantile( i6$sd_Region__D6NoInterest_Intercept, 0.975), 2)),
                                                     paste( round( quantile( i6$sd_Interviewer__D6NoInterest_Intercept, 0.025), 2), "-", round( quantile( i6$sd_Interviewer__D6NoInterest_Intercept, 0.975), 2)),
                                                     
                                                     "-",
                                                     paste( round( quantile( i6_icc_residual, 0.025), 2), "-", round( quantile( i6_icc_residual, 0.975), 2)),
                                                     paste( round( quantile( i6_icc_indi, 0.025), 2), "-", round( quantile( i6_icc_indi, 0.975), 2)),
                                                     paste( round( quantile( icc_hh_i6, 0.025), 2), "-", round( quantile( icc_hh_i6, 0.975), 2)),
                                                     paste( round( quantile( icc_com_i6, 0.025), 2), "-", round( quantile( icc_com_i6, 0.975), 2)),
                                                     paste( round( quantile( i6_icc_reg, 0.025), 2), "-", round( quantile( i6_icc_reg, 0.975), 2)),
                                                     paste( round( quantile( icc_int_i6, 0.025), 2), "-", round( quantile( icc_int_i6, 0.975), 2))
                                    ),
                                    
                                    Estimates_Model_7 = c( round( mean(i7$`b_D7Tired_Intercept[1]`), 2), 
                                                           round( mean(i7$`b_D7Tired_Intercept[2]`), 2), 
                                                           round( mean(i7$`b_D7Tired_Intercept[3]`), 2), 
                                                           round( mean(exp(i7$b_D7Tired_Age)), 2), 
                                                           round( mean(exp(i7$b_D7Tired_Sex1)), 2), 
                                                           round( mean(exp(i7$bsp_D7Tired_miSCI)), 2),
                                                           
                                                           round( mean(exp(i7$bsp_D7Tired_miSFluency)), 2), 
                                                           round( mean(i7$bs_D7Tired_sInterview_Date_1), 2), 
                                                           round( mean(i7$sds_D7Tired_sInterview_Date_1), 2), 
                                                           round( mean(exp(i7$b_D7Tired_Town_Distance)), 2), 
                                                           round( mean(exp(i7$b_D7Tired_Com_Size)), 2), 
                                                           round( mean(exp(i7$bsp_D7Tired_miHH_Size)), 2), 
                                                           round((((pi)^2)/3), 2),
                                                           
                                                           " ",
                                                           round( mean(i7$`sd_Region:ComID:UniqueFamID:PID__D7Tired_Intercept`), 2), 
                                                           round( mean(i7$`sd_Region:ComID:UniqueFamID__D7Tired_Intercept`), 2), 
                                                           round( mean(i7$`sd_Region:ComID__D7Tired_Intercept`), 2), 
                                                           round( mean(i7$sd_Region__D7Tired_Intercept), 2), 
                                                           round( mean(i7$sd_Interviewer__D7Tired_Intercept), 2),
                                                           
                                                           "-",
                                                           round( mean(i7_icc_residual), 2),
                                                           round( mean(i7_icc_indi), 2),
                                                           round( mean(icc_hh_i7), 2), 
                                                           round( mean(icc_com_i7), 2), 
                                                           round( mean(i7_icc_reg), 2),
                                                           round(mean(icc_int_i7), 2)
                                    ),
                                    
                                    CI_Model_7 = c ( paste( round( quantile(i7$`b_D7Tired_Intercept[1]`, 0.025), 2), "-", round( quantile(i7$`b_D7Tired_Intercept[1]`, 0.975), 2)),
                                                     paste( round( quantile(i7$`b_D7Tired_Intercept[2]`, 0.025), 2), "-", round( quantile(i7$`b_D7Tired_Intercept[2]`, 0.975), 2)),
                                                     paste( round( quantile(i7$`b_D7Tired_Intercept[3]`, 0.025), 2), "-", round( quantile(i7$`b_D7Tired_Intercept[3]`, 0.975), 2)),
                                                     paste( round( quantile( exp(i7$b_D7Tired_Age), 0.025), 2), "-", round( quantile( exp(i7$b_D7Tired_Age), 0.975), 2)),
                                                     paste( round( quantile( exp(i7$b_D7Tired_Sex1), 0.025), 2), "-", round( quantile( exp(i7$b_D7Tired_Sex1), 0.975), 2)),
                                                     paste( round( quantile( exp(i7$bsp_D7Tired_miSCI), 0.025), 2), "-", round( quantile( exp(i7$bsp_D7Tired_miSCI), 0.975), 2)),
                                                     
                                                     paste( round( quantile( exp(i7$bsp_D7Tired_miSFluency), 0.025), 2), "-", round( quantile( exp(i7$bsp_D7Tired_miSFluency), 0.975), 2)),
                                                     paste( round( quantile( i7$bs_D7Tired_sInterview_Date_1, 0.025), 2), "-", round( quantile( i7$bs_D7Tired_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( i7$sds_D7Tired_sInterview_Date_1, 0.025), 2), "-", round( quantile( i7$sds_D7Tired_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( exp(i7$b_D7Tired_Town_Distance), 0.025), 2), "-", round( quantile( exp(i7$b_D7Tired_Town_Distance), 0.975), 2)),
                                                     paste( round( quantile( exp(i7$b_D7Tired_Com_Size), 0.025), 2), "-", round( quantile( exp(i7$b_D7Tired_Com_Size), 0.975), 2)),
                                                     paste( round( quantile( exp(i7$bsp_D7Tired_miHH_Size), 0.025), 2), "-", round( quantile( exp(i7$bsp_D7Tired_miHH_Size), 0.975), 2)),
                                                     " ",
                                                     
                                                     " ",
                                                     paste( round( quantile( i7$`sd_Region:ComID:UniqueFamID:PID__D7Tired_Intercept`, 0.025), 2), "-", round( quantile( i7$`sd_Region:ComID:UniqueFamID:PID__D7Tired_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i7$`sd_Region:ComID:UniqueFamID__D7Tired_Intercept`, 0.025), 2), "-", round( quantile( i7$`sd_Region:ComID:UniqueFamID__D7Tired_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i7$`sd_Region:ComID__D7Tired_Intercept`, 0.025), 2), "-", round( quantile( i7$`sd_Region:ComID__D7Tired_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i7$sd_Region__D7Tired_Intercept, 0.025), 2), "-", round( quantile( i7$sd_Region__D7Tired_Intercept, 0.975), 2)),
                                                     paste( round( quantile( i7$sd_Interviewer__D7Tired_Intercept, 0.025), 2), "-", round( quantile( i7$sd_Interviewer__D7Tired_Intercept, 0.975), 2)),
                                                     
                                                     "-",
                                                     paste( round( quantile( i7_icc_residual, 0.025), 2), "-", round( quantile( i7_icc_residual, 0.975), 2)),
                                                     paste( round( quantile( i7_icc_indi, 0.025), 2), "-", round( quantile( i7_icc_indi, 0.975), 2)),
                                                     paste( round( quantile( icc_hh_i7, 0.025), 2), "-", round( quantile( icc_hh_i7, 0.975), 2)),
                                                     paste( round( quantile( icc_com_i7, 0.025), 2), "-", round( quantile( icc_com_i7, 0.975), 2)),
                                                     paste( round( quantile( i7_icc_reg, 0.025), 2), "-", round( quantile( i7_icc_reg, 0.975), 2)),
                                                     paste( round( quantile( icc_int_i7, 0.025), 2), "-", round( quantile( icc_int_i7, 0.975), 2))
                                    ),
                                    
                                    Estimates_Model_8 = c( round( mean(i8$`b_D8CantConcentrate_Intercept[1]`), 2), 
                                                           round( mean(i8$`b_D8CantConcentrate_Intercept[2]`), 2), 
                                                           round( mean(i8$`b_D8CantConcentrate_Intercept[3]`), 2), 
                                                           round( mean(exp(i8$b_D8CantConcentrate_Age)), 2), 
                                                           round( mean(exp(i8$b_D8CantConcentrate_Sex1)), 2), 
                                                           round( mean(exp(i8$bsp_D8CantConcentrate_miSCI)), 2),
                                                           
                                                           round( mean(exp(i8$bsp_D8CantConcentrate_miSFluency)), 2), 
                                                           round( mean(i8$bs_D8CantConcentrate_sInterview_Date_1), 2), 
                                                           round( mean(i8$sds_D8CantConcentrate_sInterview_Date_1), 2), 
                                                           round( mean(exp(i8$b_D8CantConcentrate_Town_Distance)), 2), 
                                                           round( mean(exp(i8$b_D8CantConcentrate_Com_Size)), 2), 
                                                           round( mean(exp(i8$bsp_D8CantConcentrate_miHH_Size)), 2), 
                                                           round((((pi)^2)/3), 2),
                                                           
                                                           " ",
                                                           round( mean(i8$`sd_Region:ComID:UniqueFamID:PID__D8CantConcentrate_Intercept`), 2), 
                                                           round( mean(i8$`sd_Region:ComID:UniqueFamID__D8CantConcentrate_Intercept`), 2), 
                                                           round( mean(i8$`sd_Region:ComID__D8CantConcentrate_Intercept`), 2), 
                                                           round( mean(i8$sd_Region__D8CantConcentrate_Intercept), 2), 
                                                           round( mean(i8$sd_Interviewer__D8CantConcentrate_Intercept), 2),
                                                           
                                                           "-",
                                                           round( mean(i8_icc_residual), 2),
                                                           round( mean(i8_icc_indi), 2),
                                                           round( mean(icc_hh_i8), 2), 
                                                           round( mean(icc_com_i8), 2), 
                                                           round( mean(i8_icc_reg), 2),
                                                           round(mean(icc_int_i8), 2)
                                    ),
                                    
                                    CI_Model_8 = c ( paste( round( quantile(i8$`b_D8CantConcentrate_Intercept[1]`, 0.025), 2), "-", round( quantile(i8$`b_D8CantConcentrate_Intercept[1]`, 0.975), 2)),
                                                     paste( round( quantile(i8$`b_D8CantConcentrate_Intercept[2]`, 0.025), 2), "-", round( quantile(i8$`b_D8CantConcentrate_Intercept[2]`, 0.975), 2)),
                                                     paste( round( quantile(i8$`b_D8CantConcentrate_Intercept[3]`, 0.025), 2), "-", round( quantile(i8$`b_D8CantConcentrate_Intercept[3]`, 0.975), 2)),
                                                     paste( round( quantile( exp(i8$b_D8CantConcentrate_Age), 0.025), 2), "-", round( quantile( exp(i8$b_D8CantConcentrate_Age), 0.975), 2)),
                                                     paste( round( quantile( exp(i8$b_D8CantConcentrate_Sex1), 0.025), 2), "-", round( quantile( exp(i8$b_D8CantConcentrate_Sex1), 0.975), 2)),
                                                     paste( round( quantile( exp(i8$bsp_D8CantConcentrate_miSCI), 0.025), 2), "-", round( quantile( exp(i8$bsp_D8CantConcentrate_miSCI), 0.975), 2)),
                                                     
                                                     paste( round( quantile( exp(i8$bsp_D8CantConcentrate_miSFluency), 0.025), 2), "-", round( quantile( exp(i8$bsp_D8CantConcentrate_miSFluency), 0.975), 2)),
                                                     paste( round( quantile( i8$bs_D8CantConcentrate_sInterview_Date_1, 0.025), 2), "-", round( quantile( i8$bs_D8CantConcentrate_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( i8$sds_D8CantConcentrate_sInterview_Date_1, 0.025), 2), "-", round( quantile( i8$sds_D8CantConcentrate_sInterview_Date_1, 0.975), 2)),
                                                     paste( round( quantile( exp(i8$b_D8CantConcentrate_Town_Distance), 0.025), 2), "-", round( quantile( exp(i8$b_D8CantConcentrate_Town_Distance), 0.975), 2)),
                                                     paste( round( quantile( exp(i8$b_D8CantConcentrate_Com_Size), 0.025), 2), "-", round( quantile( exp(i8$b_D8CantConcentrate_Com_Size), 0.975), 2)),
                                                     paste( round( quantile( exp(i8$bsp_D8CantConcentrate_miHH_Size), 0.025), 2), "-", round( quantile( exp(i8$bsp_D8CantConcentrate_miHH_Size), 0.975), 2)),
                                                     " ",
                                                     
                                                     " ",
                                                     paste( round( quantile( i8$`sd_Region:ComID:UniqueFamID:PID__D8CantConcentrate_Intercept`, 0.025), 2), "-", round( quantile( i8$`sd_Region:ComID:UniqueFamID:PID__D8CantConcentrate_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i8$`sd_Region:ComID:UniqueFamID__D8CantConcentrate_Intercept`, 0.025), 2), "-", round( quantile( i8$`sd_Region:ComID:UniqueFamID__D8CantConcentrate_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i8$`sd_Region:ComID__D8CantConcentrate_Intercept`, 0.025), 2), "-", round( quantile( i8$`sd_Region:ComID__D8CantConcentrate_Intercept`, 0.975), 2)),
                                                     paste( round( quantile( i8$sd_Region__D8CantConcentrate_Intercept, 0.025), 2), "-", round( quantile( i8$sd_Region__D8CantConcentrate_Intercept, 0.975), 2)),
                                                     paste( round( quantile( i8$sd_Interviewer__D8CantConcentrate_Intercept, 0.025), 2), "-", round( quantile( i8$sd_Interviewer__D8CantConcentrate_Intercept, 0.975), 2)),
                                                     
                                                     "-",
                                                     paste( round( quantile( i8_icc_residual, 0.025), 2), "-", round( quantile( i8_icc_residual, 0.975), 2)),
                                                     paste( round( quantile( i8_icc_indi, 0.025), 2), "-", round( quantile( i8_icc_indi, 0.975), 2)),
                                                     paste( round( quantile( icc_hh_i8, 0.025), 2), "-", round( quantile( icc_hh_i8, 0.975), 2)),
                                                     paste( round( quantile( icc_com_i8, 0.025), 2), "-", round( quantile( icc_com_i8, 0.975), 2)),
                                                     paste( round( quantile( i8_icc_reg, 0.025), 2), "-", round( quantile( i8_icc_reg, 0.975), 2)),
                                                     paste( round( quantile( icc_int_i8, 0.025), 2), "-", round( quantile( icc_int_i8, 0.975), 2))
                                    )
                                    
)

                                   
                    
#################### Generating the summary tables that are used in the manuscript
##################################################################################
##################################################################################

## Converting model summary (Item 1 - Item 4) to a flextable format
i1_i4_mod_summary <- flextable(models_summary_i1_i4)

## Adding alternative colours to distinguish different model outputs
i1_i4_mod_summary <- bg(i1_i4_mod_summary, j = c(4,5,8,9), bg = "#D3D3D3", part = "body")
i1_i4_mod_summary <- bg(i1_i4_mod_summary, j = c(2,3,6,7), bg = "beige", part = "body")
i1_i4_mod_summary <- bold(i1_i4_mod_summary, part = "header")
i1_i4_mod_summary <- autofit(i1_i4_mod_summary)

## Copy-paste the output on word
i1_i4_mod_summary

##################################################################
## Now Converting model summary (Item 5 - Item 8) to a flextable format
models_summary_i5_i8 <- flextable(models_summary_i5_i8)

## Adding alternative colours to distinguish different model outputs
models_summary_i5_i8 <- bg(models_summary_i5_i8, j = c(4,5,8,9), bg = "#D3D3D3", part = "body")
models_summary_i5_i8<- bg(models_summary_i5_i8, j = c(2,3,6,7), bg = "beige", part = "body")
models_summary_i5_i8 <- bold(models_summary_i5_i8, part = "header")
models_summary_i5_i8 <- autofit(models_summary_i5_i8)

## Copy-paste the output on word
models_summary_i5_i8


                  





