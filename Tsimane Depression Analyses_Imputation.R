############### This script contains the Tsimane social determinants depression analyses                                           #################
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

## Setting seed for reproducibility
set.seed(777723)


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


## Converting this to a csv file that will be used for the complete case analyses as this will save time from additional cleaning
## IMP: CHANGE DIRECTORY AS PER YOUR COMPUTER
write.csv(depression_data_v2, file='/Users//tk/Desktop/Social Determinants of Depression/Zurich 2023/Final Manuscript Scripts/Final Analyses/Cleaned_Depression_Dataset_for_Complete_Case_Analysis.csv')





############################# Data Analysis ######################################
##################################################################################
##################################################################################
##################################################################################


## Loading the pertinent dataset and making necessary transformations
dep_data <- depression_data_v2

## Renaming for making model formulas more readable
dep_data <- dep_data %>%
  rename("D" = "DepressionScoreM2", "Region" = "region", "Interview_Date" = "InterviewDate", "Sex" = "Male", "SCI" = "SocialConflictIndex", "SFluency" = "SpanishFluency", "Town_Distance" = "route_distance_town", "Com_Size" = "max_community_size", "HH_Size" = "household_size")

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

str(dep_data)


########### Prior Predictive Simulations #########################################################################################
#################################################################################################################################

## Before running the final models, we need to first see the implications of our priors
## On our base model where we try quantifying variation at different social levels, we will try to see the difference between two prior choices i.e. normal (0, 0.5) v/s normal (0, 1)
## This will inform the prior we use for this and subsequent models

## Specifying the model
mod1_formula <- bf(D ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + (1 |Interviewer) + (1 | ComID/UniqueFamID) + (1 | PID)) +
  bf(SCI | mi() ~ 1 + Age + Sex) + bf(SFluency | mi() ~ 1 + Age + Sex) + set_rescor(FALSE)


####### Running with narrow prior (0, 0.5) 
#######

mod1_prior1 <- brm(mod1_formula, 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept", resp = D),
                      prior(normal(0, 0.5), class="Intercept", resp = SCI),
                      prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                      prior(normal(0, 0.5), class ="b", resp = D),
                      prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D),
                      prior(normal(0, 0.5), class ="b", resp = SCI),
                      prior(normal(0, 0.5), class ="b", resp = SFluency),
                      prior(exponential(1), class = sd, resp = D)
            ),
            warmup = 1000, 
            iter   = 6000, 
            chains = 4, 
            cores  = 8,
            sample_prior = "only",
            control = list(adapt_delta = 0.99),
            seed = 9933) 

## Save Model output (to easily load the model output later)
saveRDS(mod1_prior1, "prior1_sumscore_model1.RDS")

## Load the model
mod1_prior1 <- readRDS("prior1_sumscore_model1.RDS")


####### Running with prior (0, 1) 
#######
mod1_prior2 <- brm(mod1_formula, 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 1), class="Intercept", resp = D),
                      prior(normal(0, 1), class="Intercept", resp = SCI),
                      prior(normal(0, 1), class="Intercept", resp = SFluency),
                      prior(normal(0, 1), class ="b", resp = D),
                      prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D),
                      prior(normal(0, 1), class ="b", resp = SCI),
                      prior(normal(0, 1), class ="b", resp = SFluency),
                      prior(exponential(1), class = sd, resp = D)
            ),
            warmup = 1000, 
            iter   = 6000, 
            chains = 4, 
            cores  = 8,
            sample_prior = "only",
            control = list(adapt_delta = 0.99, max_treedepth = 12), 
            seed = 9935) 

## Save Model output (to easily load the model output later)
saveRDS(mod1_prior2, "prior2_sumscore_model1.RDS")

## Load the model
mod1_prior2 <- readRDS("prior2_sumscore_model1.RDS")

### Now we conduct our prior predictive checks
pp_check(mod1_prior1, resp = "D", type = "stat") 
pp_check(mod1_prior2, resp = "D", type = "stat") 

## Visualising the slopes implied by the different priors
conditional_effects(mod1_prior1)
conditional_effects(mod1_prior2)

###### Inferences regarding priors 

# 1. Note that the minimum possible score on the depression score is 16 (a person answering 1 i.e. less frequently to all items). This is roughly a bit beyond 2 standard deviations. 
# 2. We observe that with normal(0,1), most of the predictors generate depression values between -4 and 4 standard deviations. 
# 3. This is okay on the positive side (+4 standard deviations), but on the negative side, -4 standard deviations on depression sum-scores is impossible 
# 1 std deviation on depression sum-score ~ 7.5. Therefore -4 standard deviations from the mean will be ~3.7 depression sumscore
# On the other hand, the normal (0, 0.5) prior does a much better job at producing more plausible predictions at the negative end
# Hence, we choose to run with the (0, 0.5) prior for this model
str(dep_data$D)


######################### Checking prior implications for model 2 investigating social determinants

## Specifying the model
mod2_formula <- bf(D ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID) + (1 | PID)) +
  bf(SCI | mi() ~ 1 + Age + Sex) + bf(SFluency | mi() ~ 1 + Age + Sex) + bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size) +  set_rescor(FALSE)


####### Running with narrow prior (0, 0.5) 
#######

mod2_prior1 <- brm(mod2_formula, 
                   data   = dep_data,
                   family = gaussian,
                   prior = c(prior(normal(0, 0.5), class="Intercept", resp = D),
                             prior(normal(0, 0.5), class="Intercept", resp = SCI),
                             prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                             prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                             prior(normal(0, 0.5), class ="b", resp = D),
                             prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D),
                             prior(normal(0, 0.5), class ="b", resp = SCI),
                             prior(normal(0, 0.5), class ="b", resp = SFluency),
                             prior(normal(0, 0.5), class ="b", resp = HHSize),
                             prior(exponential(1), class = sd, resp = D)
                   ),
                   warmup = 1000, 
                   iter   = 6000, 
                   chains = 4, 
                   cores  = 8,
                   sample_prior = "only",
                   control = list(adapt_delta = 0.99, max_treedepth = 12),
                   seed = 9937) 


## Save Model output (to easily load the model output later)
saveRDS(mod2_prior1, "prior1_sumscore_model2.RDS")

## Load the model
mod2_prior1 <- readRDS("prior1_sumscore_model2.RDS")


####### Running with prior (0, 1) 
#######

mod2_prior2 <- brm(mod2_formula, 
                   data   = dep_data,
                   family = gaussian,
                   prior = c(prior(normal(0, 1), class="Intercept", resp = D),
                             prior(normal(0, 1), class="Intercept", resp = SCI),
                             prior(normal(0, 1), class="Intercept", resp = SFluency),
                             prior(normal(0, 1), class="Intercept", resp = HHSize),
                             prior(normal(0, 1), class ="b", resp = D),
                             prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D),
                             prior(normal(0, 1), class ="b", resp = SCI),
                             prior(normal(0, 1), class ="b", resp = SFluency),
                             prior(normal(0, 1), class ="b", resp = HHSize),
                             prior(exponential(1), class = sd, resp = D)
                   ),
                   warmup = 1000, 
                   iter   = 6000, 
                   chains = 4, 
                   cores  = 8,
                   sample_prior = "only",
                   control = list(adapt_delta = 0.99, max_treedepth = 12),
                   seed = 9938) 


## Save Model output (to easily load the model output later)
saveRDS(mod2_prior2, "prior2_sumscore_model2.RDS")

## Load the model
mod2_prior2 <- readRDS("prior2_sumscore_model2.RDS")


### Now we conduct our prior predictive checks
pp_check(mod2_prior1, resp = "D", type = "stat") 
pp_check(mod2_prior2, resp = "D", type = "stat") 

## Visualising the slopes implied by the different priors
conditional_effects(mod2_prior1)
conditional_effects(mod2_prior2)

###### Inferences regarding priors

# Prior (0, 0.5) again performs way better than broader pior (0,1) and mosty respects measurement boundaries





########### Now, we run the actual models #########################################################################################
#######################################################################################################################################################


########### Model 1: Quantifying variation at different levels #########################################################################################
#######################################################################################################################################################

mod1 <- brm(mod1_formula, 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept", resp = D),
                      prior(normal(0, 0.5), class="Intercept", resp = SCI),
                      prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                      prior(normal(0, 0.5), class ="b", resp = D),
                      prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D),
                      prior(normal(0, 0.5), class ="b", resp = SCI),
                      prior(normal(0, 0.5), class ="b", resp = SFluency),
                      prior(exponential(1), class = sd, resp = D)
            ),
            warmup = 1000, 
            iter   = 6000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99, max_treedepth = 12),
            seed = 99343) 

## Save Model output (to easily load the model output later)
saveRDS(mod1, "sumscore_model1.RDS")

## Load the model
mod1 <- readRDS("sumscore_model1.RDS")




####### Given that we see minimal variation at the PID and region level, let's make drop these grouping factors to make it less complex
#######

## Specifying the slightly altered model
mod1_formula_v2 <- bf(D ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + (1 |Interviewer) + (1 | ComID/UniqueFamID)) +
  bf(SCI | mi() ~ 1 + Age + Sex) + bf(SFluency | mi() ~ 1 + Age + Sex) + set_rescor(FALSE)

## Running this new model
mod1_v2 <- brm(mod1_formula_v2, 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept", resp = D),
                      prior(normal(0, 0.5), class="Intercept", resp = SCI),
                      prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                      prior(normal(0, 0.5), class ="b", resp = D),
                      prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D),
                      prior(normal(0, 0.5), class ="b", resp = SCI),
                      prior(normal(0, 0.5), class ="b", resp = SFluency),
                      prior(exponential(1), class = sd, resp = D)
            ),
            warmup = 1000, 
            iter   = 6000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99, max_treedepth = 12),
            seed = 99353) 


## Save Model output (to easily load the model output later)
saveRDS(mod1_v2, "sumscore_model1_version2.RDS")

## Load the model
mod1_v2 <- readRDS("sumscore_model1_version2.RDS")



########### Running model 2 to see if higher level variables explain higher level variation ##################
#################################################################################################################################

## Specifying the model
mod2 <- brm(mod2_formula, 
                   data   = dep_data,
                   family = gaussian,
                   prior = c(prior(normal(0, 0.5), class="Intercept", resp = D),
                             prior(normal(0, 0.5), class="Intercept", resp = SCI),
                             prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                             prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                             prior(normal(0, 0.5), class ="b", resp = D),
                             prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D),
                             prior(normal(0, 0.5), class ="b", resp = SCI),
                             prior(normal(0, 0.5), class ="b", resp = SFluency),
                             prior(normal(0, 0.5), class ="b", resp = HHSize),
                             prior(exponential(1), class = sd, resp = D)
                   ),
                   warmup = 1000, 
                   iter   = 6000, 
                   chains = 4, 
                   cores  = 8,
                   control = list(adapt_delta = 0.99, max_treedepth = 12),
                   seed = 99372) 

## Save Model output (to easily load the model output later)
saveRDS(mod2, "sumscore_model2.RDS")

## Load the model
mod2 <- readRDS("sumscore_model2.RDS")


####### Given that we see minimal variation at the PID and region level, let's make drop these grouping factors to make it less complex
#######

## Specifying the slightly altered model
mod2_formula_v2 <- bf(D ~ 1 + Age + Sex + mi(SCI) + mi(SFluency) + s(Interview_Date) + Town_Distance + Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID)) +
  bf(SCI | mi() ~ 1 + Age + Sex) + bf(SFluency | mi() ~ 1 + Age + Sex) + bf(HH_Size | mi() ~ 1 + Age + Sex + Com_Size) +  set_rescor(FALSE)

## Running this altered model
mod2_v2 <- brm(mod2_formula_v2, 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept", resp = D),
                      prior(normal(0, 0.5), class="Intercept", resp = SCI),
                      prior(normal(0, 0.5), class="Intercept", resp = SFluency),
                      prior(normal(0, 0.5), class="Intercept", resp = HHSize),
                      prior(normal(0, 0.5), class ="b", resp = D),
                      prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D),
                      prior(normal(0, 0.5), class ="b", resp = SCI),
                      prior(normal(0, 0.5), class ="b", resp = SFluency),
                      prior(normal(0, 0.5), class ="b", resp = HHSize),
                      prior(exponential(1), class = sd, resp = D)
            ),
            warmup = 1000, 
            iter   = 6000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99, max_treedepth = 12),
            seed = 993725) 

## Save Model output (to easily load the model output later)
saveRDS(mod2_v2, "sumscore_model2_version2.RDS")

## Load the model
mod2_v2 <- readRDS("sumscore_model2_version2.RDS")



########### Let's plot the effects of the models reported in the main text (i.e.one's without PID & Region) ###########################################
#######################################################################################################################################################
#######################################################################################################################################################

## Look at summmaries first
summary(mod1_v2)
summary(mod2_v2)

## Drawing the posteriors from these models
T1 <- as_draws_df(mod1_v2)
T2 <- as_draws_df(mod2_v2)

## Renaming co-efficients for better readability on actual graphs
T1 <- T1 %>% 
  rename("Intercept" = "b_D_Intercept",
       "Age" = "b_D_Age",
       "Male" = "b_D_Sex1",
       "SCI" = "bsp_D_miSCI",
       "SFluency" = "bsp_D_miSFluency"
)

T2 <- T2 %>%
  rename("Intercept" = "b_D_Intercept",
         "Age" = "b_D_Age",
         "Male" = "b_D_Sex1",
         "SCI" = "bsp_D_miSCI",
         "SFluency" = "bsp_D_miSFluency",
         "Town_Distance" = "b_D_Town_Distance",
         "C_Size" = "b_D_Com_Size",
         "H_Size" = "bsp_D_miHH_Size"
  )

## Now select these renamed parameters from the whole posterior draw
T1_params <- c("Intercept", "Age", "Male", "SCI", "SFluency")
T2_params <- c("Intercept", "Age", "Male", "SCI", "SFluency", "Town_Distance", "C_Size", "H_Size")


######################################### Create density plot for Model 1 now
mod1_density_plots <- mcmc_areas(
  T1 %>% select(all_of(T1_params)),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod1_density_plots <- mod1_density_plots +
  scale_x_continuous(limits = c(-0.5, 0.7), breaks = seq(-0.5, 0.7, by = 0.1)) +
  labs(title = "(a) Model 1") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20))

# Calculate how much of the posterior is above 0

# First we reverse the order of the variables as the mcmc_areas() reverses the order while plotting 
T1_params_rev <- rev(T1_params)

# Now, we quickly calculate the P>0
text_labels <- tibble(
  parameter = factor(T1_params_rev),  
  label = paste0("P(>0) = ", round(colMeans(T1[T1_params_rev] > 0), 2)),
  y = seq_along(T1_params)  # sequential y-position from bottom (Intercept) to top (H_Size)
)


# Add text layer to plot
mod1_density_plots <- mod1_density_plots +
  geom_text(data = text_labels,
            aes(x = 0.5, y = y, label = label),
            hjust = 0,
            family = "Helvetica",
            fontface = "italic",
            color = "red",
            size = 8)


######################################### Create density plot for Model 2 now
mod2_density_plots <- mcmc_areas(
  T2 %>% select(all_of(T2_params)),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod2_density_plots <- mod2_density_plots +
  scale_x_continuous(limits = c(-0.5, 0.7), breaks = seq(-0.5, 0.7, by = 0.1)) +
  labs(title = "(b) Model 2") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20))

# Calculate how much of the posterior is above 0

# First we reverse the order of the variables as the mcmc_areas() reverses the order while plotting 
T2_params_rev <- rev(T2_params)

# Now, we quickly calculate the P>0
text_labels <- tibble(
  parameter = factor(T2_params_rev),  
  label = paste0("P(>0) = ", round(colMeans(T2[T2_params_rev] > 0), 2)),
  y = seq_along(T2_params)  # sequential y-position from bottom (Intercept) to top (H_Size)
)


# Add text layer to plot
mod2_density_plots <- mod2_density_plots +
  geom_text(data = text_labels,
            aes(x = 0.5, y = y, label = label),
            hjust = 0,
            family = "Helvetica",
            fontface = "italic",
            color = "red",
            size = 8)





######################################### Now let's plot the effect of interview date for model 1
#################################################################################################

## Draw conditional effects plot
spline_plot_mod1 <- conditional_effects(mod1_v2, "Interview_Date") 

## Get the data conditional effects used to generate this plot
time_effect_data_mod1 <- spline_plot_mod1$D.D_Interview_Date

# To reverse it, use the mean and sd of Interview_Date from the original dataset where interview dates weren't transformed:
mean_interview_date <- attr(dep_data$Interview_Date, "scaled:center")
sd_interview_date <- attr(dep_data$Interview_Date, "scaled:scale")

## Reversing the standardisation and converting it to date format
time_effect_data_mod1$Interview_Date <- (time_effect_data_mod1$Interview_Date * sd_interview_date) + mean_interview_date
time_effect_data_mod1$Interview_Date <- as.POSIXct(time_effect_data_mod1$Interview_Date, origin = "1970-01-01")

# Step 3: Update the plot
time_effect_mod1_plot <- ggplot(time_effect_data_mod1, aes(x = Interview_Date, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1) +
  geom_point(data = time_effect_data_mod1[time_effect_data_mod1$points__, ], aes(x = Interview_Date, y = estimate__)) +
  labs(title = "(c) Spline Prediction - Model 1", x = "Interview Date", y = "Standardized Depression Score") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Title style same as mod2
    axis.title.x = element_text(family = "Helvetica", size = 16),  # xlab font style and size
    axis.title.y = element_text(family = "Helvetica", size = 18),  # ylab font style and size
    axis.text.x = element_text(family = "Helvetica", size = 16),   # To keep x-axis tick labels consistent
    axis.text.y = element_text(family = "Helvetica", size = 18)    # To keep y-axis tick labels consistent
  )


######################################### Now let's plot the effect of interview date for model 2

## Draw conditional effects plot
spline_plot_mod2 <- conditional_effects(mod2_v2, "Interview_Date") 

## Get the data conditional effects used to generate this plot
time_effect_data_mod2 <- spline_plot_mod2$D.D_Interview_Date

## Reversing the standardisation and converting it to date format
time_effect_data_mod2$Interview_Date <- (time_effect_data_mod2$Interview_Date * sd_interview_date) + mean_interview_date
time_effect_data_mod2$Interview_Date <- as.POSIXct(time_effect_data_mod2$Interview_Date, origin = "1970-01-01")

# Step 3: Update the plot
time_effect_mod2_plot <- ggplot(time_effect_data_mod2, aes(x = Interview_Date, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1) +
  geom_point(data = time_effect_data_mod2[time_effect_data_mod2$points__, ], aes(x = Interview_Date, y = estimate__)) +
  labs(title = " (d) Spline Prediction - Model 2", x = "Interview Date", y = "Standardized Depression Score") +
  theme_minimal() + 
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Title style same as mod2
    axis.title.x = element_text(family = "Helvetica", size = 16),  # xlab font style and size
    axis.title.y = element_text(family = "Helvetica", size = 18),  # ylab font style and size
    axis.text.x = element_text(family = "Helvetica", size = 16),   # To keep x-axis tick labels consistent
    axis.text.y = element_text(family = "Helvetica", size = 18)    # To keep y-axis tick labels consistent
  )


######################################### Now let's plot the ICCs
#################################################################################################


############ Starting with Model 1

# Calculate the adjusted ICCs
icc_residual_1 <- ( (T1$sigma_D)^2 ) / 
  ( (T1$sigma_D)^2 + 
    (T1$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
    (T1$`sd_ComID__D_Intercept`)^2 + 
    (T1$sd_Interviewer__D_Intercept)^2 )
dens(icc_residual_1, col=2, lwd=4, xlab="ICC (Residual) of Model 1")

icc_family_1 <- ( (T1$`sd_ComID:UniqueFamID__D_Intercept`)^2 ) / 
  ( (T1$sigma_D)^2 + 
    (T1$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
    (T1$`sd_ComID__D_Intercept`)^2 + 
    (T1$sd_Interviewer__D_Intercept)^2 )
dens(icc_family_1, col=2, lwd=4, xlab="ICC (Family) of Model 1")

icc_community_1 <- ( (T1$`sd_ComID__D_Intercept`)^2 ) / 
  ( (T1$sigma_D)^2 + 
    (T1$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
    (T1$`sd_ComID__D_Intercept`)^2 + 
    (T1$sd_Interviewer__D_Intercept)^2 )
dens(icc_community_1, col=2, lwd=4, xlab="ICC (Community) of Model 1")

icc_interviewer_1 <- ( (T1$sd_Interviewer__D_Intercept)^2 ) / 
  ( (T1$sigma_D)^2 + 
    (T1$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
    (T1$`sd_ComID__D_Intercept`)^2 + 
    (T1$sd_Interviewer__D_Intercept)^2 )
dens(icc_interviewer_1, col=2, lwd=4, xlab="ICC (Interviewer) of Model 1")


# Combine ICC vectors into a dataframe to allow for easy plotting of the five density plots in the same figure
icc_data_1 <- data.frame(
  value = c(icc_residual_1, icc_family_1, icc_community_1, icc_interviewer_1),
  group = rep(c("Residual", "Household", "Community", "Interviewer"), each = length(icc_residual_1))
)

# Convert group variable to factor with specified levels (important for color coding in the plots)
icc_data_1$group <- factor(icc_data_1$group, levels = c("Residual", "Household", "Community", "Interviewer"))


# Final Plot for model 1
icc_plot_mod1 <- ggplot(icc_data_1, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "(a) Model 1 ICC",
       x = "ICC",
       y = "Density",
       color = "ICC Type") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 20)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = c("Residual" = "Green", "Household" = "orange", "Community" = "red", "Interviewer" = "darkcyan")) +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )


############ Now for Model 2

## Calculate ICCs
icc_residual_2 <- ( (T2$sigma_D)^2 ) / 
  ( (T2$sigma_D)^2 + 
      (T2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
      (T2$`sd_ComID__D_Intercept`)^2 + 
      (T2$sd_Interviewer__D_Intercept)^2 )
dens(icc_residual_2, col=2, lwd=4, xlab="ICC (Residual) of Model 2")

icc_family_2 <- ( (T2$`sd_ComID:UniqueFamID__D_Intercept`)^2 ) / 
  ( (T2$sigma_D)^2 + 
      (T2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
      (T2$`sd_ComID__D_Intercept`)^2 + 
      (T2$sd_Interviewer__D_Intercept)^2 )
dens(icc_family_2, col=2, lwd=4, xlab="ICC (Family) of Model 2")

icc_community_2 <- ( (T2$`sd_ComID__D_Intercept`)^2 ) / 
  ( (T2$sigma_D)^2 + 
      (T2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
      (T2$`sd_ComID__D_Intercept`)^2 + 
      (T2$sd_Interviewer__D_Intercept)^2 )
dens(icc_community_2, col=2, lwd=4, xlab="ICC (Community) of Model 2")

icc_interviewer_2 <- ( (T2$sd_Interviewer__D_Intercept)^2 ) / 
  ( (T2$sigma_D)^2 + 
      (T2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
      (T2$`sd_ComID__D_Intercept`)^2 + 
      (T2$sd_Interviewer__D_Intercept)^2 )
dens(icc_interviewer_2, col=2, lwd=4, xlab="ICC (Interviewer) of Model 2")

# Combine ICC vectors into a dataframe to allow for easy plotting of the five density plots in the same figure
icc_data_2 <- data.frame(
  value = c(icc_residual_2, icc_family_2, icc_community_2, icc_interviewer_2),
  group = rep(c("Residual", "Household", "Community", "Interviewer"), each = length(icc_residual_2))
)

# Convert group variable to factor with specified levels (important for color coding in the plots)
icc_data_2$group <- factor(icc_data_2$group, levels = c("Residual", "Household", "Community", "Interviewer"))

# Final Plot for model 2
icc_plot_mod2 <- ggplot(icc_data_2, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "(b) Model 2 ICC",
       x = "ICC",
       y = "Density",
       color = "ICC Type") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 20)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = c("Residual" = "Green", "Household" = "orange", "Community" = "red", "Interviewer" = "darkcyan")) +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )


## Combine all plots together into one mega summary plot

## First for all model co-efficients
combined_sum_scores_plot <- (mod1_density_plots | mod2_density_plots) / (time_effect_mod1_plot | time_effect_mod2_plot)

## Now for ICC plots
combined_icc_plot <- (icc_plot_mod1 | icc_plot_mod2)




########### Let's plot the effects of the models reported in the appendix (i.e.one's with PID) ###########################################
#######################################################################################################################################################
#######################################################################################################################################################

## Look at summmaries first
summary(mod1)
summary(mod2)

## Drawing the posteriors from these models
T1_v2 <- as_draws_df(mod1)
T2_v2 <- as_draws_df(mod2)

## Renaming co-efficients for better readability on actual graphs
T1_v2 <- T1_v2 %>% 
  rename("Intercept" = "b_D_Intercept",
         "Age" = "b_D_Age",
         "Male" = "b_D_Sex1",
         "SCI" = "bsp_D_miSCI",
         "SFluency" = "bsp_D_miSFluency"
  )

T2_v2 <- T2_v2 %>%
  rename("Intercept" = "b_D_Intercept",
         "Age" = "b_D_Age",
         "Male" = "b_D_Sex1",
         "SCI" = "bsp_D_miSCI",
         "SFluency" = "bsp_D_miSFluency",
         "Town_Distance" = "b_D_Town_Distance",
         "C_Size" = "b_D_Com_Size",
         "H_Size" = "bsp_D_miHH_Size"
  )

## Now select these renamed parameters from the whole posterior draw
T1_params <- c("Intercept", "Age", "Male", "SCI", "SFluency")
T2_params <- c("Intercept", "Age", "Male", "SCI", "SFluency", "Town_Distance", "C_Size", "H_Size")


######################################### Create density plot for Model 1 now
mod1_density_plots_v2 <- mcmc_areas(
  T1_v2 %>% select(all_of(T1_params)),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod1_density_plots_v2 <- mod1_density_plots_v2 +
  scale_x_continuous(limits = c(-0.5, 0.7), breaks = seq(-0.5, 0.7, by = 0.1)) +
  labs(title = "(a) Model 1") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )

# Reverse parameter order to match mcmc_areas plot
T1_params_rev <- rev(T1_params)

# Calculate P(>0)
text_labels_v2 <- tibble(
  parameter = factor(T1_params_rev),  
  label = paste0("P(>0) = ", round(colMeans(T1_v2[T1_params_rev] > 0), 2)),
  y = seq_along(T1_params)  # sequential y-position from bottom (Intercept) to top (H_Size)
)

# Add text layer to plot
mod1_density_plots_v2 <- mod1_density_plots_v2 +
  geom_text(data = text_labels_v2,
            aes(x = 0.5, y = y, label = label),
            hjust = 0,
            family = "Helvetica",
            fontface = "italic",
            color = "red",
            size = 8)


######################################### Create density plot for Model 2 now
mod2_density_plots_v2 <- mcmc_areas(
  T2_v2 %>% select(all_of(T2_params)),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod2_density_plots_v2 <- mod2_density_plots_v2 +
  scale_x_continuous(limits = c(-0.5, 0.7), breaks = seq(-0.5, 0.7, by = 0.1)) +
  labs(title = "(b) Model 2") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )

# Reverse parameter order to match mcmc_areas plot
T2_params_rev <- rev(T2_params)

# Calculate P(>0)
text_labels_v2 <- tibble(
  parameter = factor(T2_params_rev),  
  label = paste0("P(>0) = ", round(colMeans(T2_v2[T2_params_rev] > 0), 2)),
  y = seq_along(T2_params)  # sequential y-position from bottom (Intercept) to top (H_Size)
)

# Add text layer to plot
mod2_density_plots_v2 <- mod2_density_plots_v2 +
  geom_text(data = text_labels_v2,
            aes(x = 0.5, y = y, label = label),
            hjust = 0,
            family = "Helvetica",
            fontface = "italic",
            color = "red",
            size = 8)






######################################### Now let's plot the effect of interview date for model 1
#################################################################################################

## Draw conditional effects plot for Model 1
spline_plot_mod1_v2 <- conditional_effects(mod1, "Interview_Date") 

## Get the data conditional effects used to generate this plot
time_effect_data_mod1_v2 <- spline_plot_mod1_v2$D.D_Interview_Date

## Reversing the standardisation and converting it to date format
time_effect_data_mod1_v2$Interview_Date <- (time_effect_data_mod1_v2$Interview_Date * sd_interview_date) + mean_interview_date
time_effect_data_mod1_v2$Interview_Date <- as.POSIXct(time_effect_data_mod1_v2$Interview_Date, origin = "1970-01-01")

# Update the plot
time_effect_mod1_v2_plot <- ggplot(time_effect_data_mod1_v2, aes(x = Interview_Date, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1) +
  geom_point(data = time_effect_data_mod1_v2[time_effect_data_mod1_v2$points__, ], aes(x = Interview_Date, y = estimate__)) +
  labs(title = "(c) Spline Prediction - Model 1", x = "Interview Date", y = "Standardized Depression Score") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),
    axis.title.x = element_text(family = "Helvetica", size = 16),
    axis.title.y = element_text(family = "Helvetica", size = 18),
    axis.text.x = element_text(family = "Helvetica", size = 16),
    axis.text.y = element_text(family = "Helvetica", size = 18)
  )


######################################### Now plot the effect of interview date for Model 2

## Draw conditional effects plot
spline_plot_mod2_v2 <- conditional_effects(mod2, "Interview_Date") 

## Get the data conditional effects used to generate this plot
time_effect_data_mod2_v2 <- spline_plot_mod2_v2$D.D_Interview_Date

## Reversing the standardisation and converting it to date format
time_effect_data_mod2_v2$Interview_Date <- (time_effect_data_mod2_v2$Interview_Date * sd_interview_date) + mean_interview_date
time_effect_data_mod2_v2$Interview_Date <- as.POSIXct(time_effect_data_mod2_v2$Interview_Date, origin = "1970-01-01")

# Update the plot
time_effect_mod2_v2_plot <- ggplot(time_effect_data_mod2_v2, aes(x = Interview_Date, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1) +
  geom_point(data = time_effect_data_mod2_v2[time_effect_data_mod2_v2$points__, ], aes(x = Interview_Date, y = estimate__)) +
  labs(title = "(d) Spline Prediction - Model 2", x = "Interview Date", y = "Standardized Depression Score") +
  theme_minimal() + 
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),
    axis.title.x = element_text(family = "Helvetica", size = 16),
    axis.title.y = element_text(family = "Helvetica", size = 18),
    axis.text.x = element_text(family = "Helvetica", size = 16),
    axis.text.y = element_text(family = "Helvetica", size = 18)
  )



######################################### Now let's plot the ICCs
#################################################################################################


############ Starting with Model 1

## Calculating ICCs for residual, individual, family, community, region, and interviewer levels (Model 1)

# Residual ICC Calculation
icc_residual_1_v2 <-  ((T1_v2$sigma_D)^2) / 
  ((T1_v2$sigma_D)^2 + 
     (T1_v2$sd_PID__D_Intercept)^2 + 
     (T1_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
     (T1_v2$`sd_ComID__D_Intercept`)^2 + 
     (T1_v2$sd_Interviewer__D_Intercept)^2)

dens(icc_residual_1_v2, col=2, lwd=4 , xlab="ICC (Residual) of Model 1")

# Individual ICC Calculation
icc_individual_1_v2 <-  ((T1_v2$sd_PID__D_Intercept)^2) / 
  ((T1_v2$sigma_D)^2 + 
     (T1_v2$sd_PID__D_Intercept)^2 + 
     (T1_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
     (T1_v2$`sd_ComID__D_Intercept`)^2 + 
     (T1_v2$sd_Interviewer__D_Intercept)^2)

dens(icc_individual_1_v2, col=2, lwd=4 , xlab="ICC (Individual) of Model 1")

# Family ICC Calculation
icc_family_1_v2 <- ((T1_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2) / 
  ((T1_v2$sigma_D)^2 + 
     (T1_v2$sd_PID__D_Intercept)^2 + 
     (T1_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
     (T1_v2$`sd_ComID__D_Intercept`)^2 + 
     (T1_v2$sd_Interviewer__D_Intercept)^2)

dens(icc_family_1_v2, col=2, lwd=4 , xlab="ICC (Family) of Model 1")

# Community ICC Calculation
icc_community_1_v2 <- ((T1_v2$`sd_ComID__D_Intercept`)^2) / 
  ((T1_v2$sigma_D)^2 + 
     (T1_v2$sd_PID__D_Intercept)^2 + 
     (T1_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
     (T1_v2$`sd_ComID__D_Intercept`)^2 + 
     (T1_v2$sd_Interviewer__D_Intercept)^2)

dens(icc_community_1_v2, col=2, lwd=4 , xlab="ICC (Community) of Model 1")


# Interviewer ICC Calculation
icc_interviewer_1_v2 <- ((T1_v2$sd_Interviewer__D_Intercept)^2) / 
  ((T1_v2$sigma_D)^2 + 
     (T1_v2$sd_PID__D_Intercept)^2 + 
     (T1_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
     (T1_v2$`sd_ComID__D_Intercept`)^2 + 
     (T1_v2$sd_Interviewer__D_Intercept)^2)

dens(icc_interviewer_1_v2, col=2, lwd=4 , xlab="ICC (Interviewer) of Model 1")

# Combine ICC vectors into a dataframe to allow for easy plotting of the five density plots in the same figure
icc_data_1_v2 <- data.frame(
  value = c(icc_residual_1_v2, icc_individual_1_v2, icc_family_1_v2, icc_community_1_v2, icc_interviewer_1_v2),
  group = rep(c("Residual", "Individual", "Household", "Community", "Interviewer"), each = length(icc_residual_1_v2))
)

# Convert group variable to factor with specified levels (important for color coding in the plots)
icc_data_1_v2$group <- factor(icc_data_1_v2$group, levels = c("Residual", "Individual", "Household", "Community", "Interviewer"))


# Final Plot for model 1
icc_plot_mod1_v2 <- ggplot(icc_data_1_v2, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "(a) Model 1 ICC",
       x = "ICC",
       y = "Density",
       color = "ICC Type") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = c("Residual" = "Green", "Individual" = "black", "Household" = "orange", "Community" = "red", "Interviewer" = "darkcyan")) +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )




############ Now for Model 2

## Calculating ICCs for residual, individual, family, community, region, and interviewer levels (Model 2), using T2_v2

# Residual ICC Calculation for Model 2
icc_residual_2_v2 <-  ((T2_v2$sigma_D)^2) / 
  ((T2_v2$sigma_D)^2 + 
     (T2_v2$sd_PID__D_Intercept)^2 + 
     (T2_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
     (T2_v2$`sd_ComID__D_Intercept`)^2 + 
     (T2_v2$sd_Interviewer__D_Intercept)^2)

dens(icc_residual_2_v2, col=2, lwd=4 , xlab="ICC (Residual) of Model 2")

# Individual ICC Calculation for Model 2
icc_individual_2_v2 <-  ((T2_v2$sd_PID__D_Intercept)^2) / 
  ((T2_v2$sigma_D)^2 + 
     (T2_v2$sd_PID__D_Intercept)^2 + 
     (T2_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
     (T2_v2$`sd_ComID__D_Intercept`)^2 + 
     (T2_v2$sd_Interviewer__D_Intercept)^2)

dens(icc_individual_2_v2, col=2, lwd=4 , xlab="ICC (Individual) of Model 2")

# Family ICC Calculation for Model 2
icc_family_2_v2 <- ((T2_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2) / 
  ((T2_v2$sigma_D)^2 + 
     (T2_v2$sd_PID__D_Intercept)^2 + 
     (T2_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
     (T2_v2$`sd_ComID__D_Intercept`)^2 + 
     (T2_v2$sd_Interviewer__D_Intercept)^2)

dens(icc_family_2_v2, col=2, lwd=4 , xlab="ICC (Family) of Model 2")

# Community ICC Calculation for Model 2
icc_community_2_v2 <- ((T2_v2$`sd_ComID__D_Intercept`)^2) / 
  ((T2_v2$sigma_D)^2 + 
     (T2_v2$sd_PID__D_Intercept)^2 + 
     (T2_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
     (T2_v2$`sd_ComID__D_Intercept`)^2 + 
     (T2_v2$sd_Interviewer__D_Intercept)^2)

dens(icc_community_2_v2, col=2, lwd=4 , xlab="ICC (Community) of Model 2")

# Interviewer ICC Calculation for Model 2
icc_interviewer_2_v2 <- ((T2_v2$sd_Interviewer__D_Intercept)^2) / 
  ((T2_v2$sigma_D)^2 + 
     (T2_v2$sd_PID__D_Intercept)^2 + 
     (T2_v2$`sd_ComID:UniqueFamID__D_Intercept`)^2 + 
     (T2_v2$`sd_ComID__D_Intercept`)^2 + 
     (T2_v2$sd_Interviewer__D_Intercept)^2)

dens(icc_interviewer_2_v2, col=2, lwd=4 , xlab="ICC (Interviewer) of Model 2")


# Combine ICC vectors into a dataframe to allow for easy plotting of the five density plots in the same figure
icc_data_2_v2 <- data.frame(
  value = c(icc_residual_2_v2, icc_individual_2_v2, icc_family_2_v2, icc_community_2_v2, icc_interviewer_2_v2),
  group = rep(c("Residual", "Individual", "Household", "Community", "Interviewer"), each = length(icc_residual_2_v2))
)

# Convert group variable to factor with specified levels (important for color coding in the plots)
icc_data_2_v2$group <- factor(icc_data_2_v2$group, levels = c("Residual", "Individual", "Household", "Community", "Interviewer"))

# Final Plot for model 2
icc_plot_mod2_v2 <- ggplot(icc_data_2_v2, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "(b) Model 2 ICC",
       x = "ICC",
       y = "Density",
       color = "ICC Type") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = c("Residual" = "Green", "Individual" = "black", "Household" = "orange", "Community" = "red", "Interviewer" = "darkcyan")) +
  theme(plot.title = element_text(color = "darkblue", size = 24, hjust = 0.5),
        axis.title.x = element_text(size = 20),  # Increase x axis title size
        axis.title.y = element_text(size = 20),   # Increase y axis title size)
        legend.title = element_text(size = 19),  # Increase legend title size
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 11.5),  # Increase x axis number size
        axis.text.y = element_text(size = 14)
  )


## Combine all plots together into one mega summary plot

## First for all model co-efficients
combined_sum_scores_plot_v2 <- (mod1_density_plots_v2 | mod2_density_plots_v2) / (time_effect_mod1_v2_plot | time_effect_mod2_v2_plot)

## Now for ICC plots
combined_icc_plot_v2 <- (icc_plot_mod1_v2 | icc_plot_mod2_v2)




########### Let's plot the effects of the imputation models that ran alongside the main-text models ##################################################
#######################################################################################################################################################
#######################################################################################################################################################

############# First for Model 1

############### Social Conflict Index
mod1_density_plots_SCI_imputation <- mcmc_areas(
  T1 %>% select(b_SCI_Intercept, b_SCI_Age, b_SCI_Sex1),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod1_density_plots_SCI_imputation <- mod1_density_plots_SCI_imputation +
  scale_x_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.1)) +
  labs(title = "(a) Model 1 Imputation - Social Conflict Index") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )

############### Spanish Fluency

mod1_density_plots_SFluency_imputation <- mcmc_areas(
  T1 %>% select(b_SFluency_Intercept, b_SFluency_Age, b_SFluency_Sex1),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod1_density_plots_SFluency_imputation <- mod1_density_plots_SFluency_imputation +
  scale_x_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.1))  +
  labs(title = "(b) Model 1 Imputation - Spanish Fluency") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )


############# Now for Model 2


############### Social Conflict Index
mod2_density_plots_SCI_imputation <- mcmc_areas(
  T2 %>% select(b_SCI_Intercept, b_SCI_Age, b_SCI_Sex1),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod2_density_plots_SCI_imputation <- mod2_density_plots_SCI_imputation +
  scale_x_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.1)) +
  labs(title = "(c) Model 2 Imputation - Social Conflict Index") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )

############### Spanish Fluency

mod2_density_plots_SFluency_imputation <- mcmc_areas(
  T2 %>% select(b_SFluency_Intercept, b_SFluency_Age, b_SFluency_Sex1),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod2_density_plots_SFluency_imputation <- mod2_density_plots_SFluency_imputation +
  scale_x_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.1))  +
  labs(title = "(d) Model 2 Imputation - Spanish Fluency") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )

############### Household Size

mod2_density_plots_HHSize_imputation <- mcmc_areas(
  T2 %>% select(b_HHSize_Intercept, b_HHSize_Age, b_HHSize_Sex1, b_HHSize_Com_Size),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod2_density_plots_HHSize_imputation <- mod2_density_plots_HHSize_imputation +
  scale_x_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.1))  +
  labs(title = "(e) Model 2 Imputation - Household Size") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )



############ Now plotting their sigmas 

## First wrangling everything into a single dataframe
imputation_sigmas <- data.frame(
  SCI_sigma_Model1 = T1$sigma_SCI,
  SFluency_sigma_Model1 = T1$sigma_SFluency,
  SCI_sigma_Model2 = T2$sigma_SCI,
  SFluency_sigma_Model2 = T2$sigma_SFluency,
  HHSize_sigma_Model2 = T2$sigma_HHSize
)

# Plot
imputation_sigmas_plot <- mcmc_areas(
  imputation_sigmas,
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
imputation_sigmas_plot <- imputation_sigmas_plot +
  labs(title = "(f) Imputation Model Sigmas") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )



############### Combine all plots
imp_models_fixed_effects_plot <- (mod1_density_plots_SCI_imputation | mod1_density_plots_SFluency_imputation) / (mod2_density_plots_SCI_imputation | mod2_density_plots_SFluency_imputation) / (mod2_density_plots_HHSize_imputation | imputation_sigmas_plot)


########### Now let's do the same for the models reported in the appendix  ##################################################
#######################################################################################################################################################
#######################################################################################################################################################

############# First for Model 1

############### Social Conflict Index
mod1_density_plots_SCI_imputation_v2 <- mcmc_areas(
  T1_v2 %>% select(b_SCI_Intercept, b_SCI_Age, b_SCI_Sex1),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod1_density_plots_SCI_imputation_v2 <- mod1_density_plots_SCI_imputation_v2 +
  scale_x_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.1)) +
  labs(title = "(a) Model 1 Imputation - Social Conflict Index") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )

############### Spanish Fluency

mod1_density_plots_SFluency_imputation_v2 <- mcmc_areas(
  T1_v2 %>% select(b_SFluency_Intercept, b_SFluency_Age, b_SFluency_Sex1),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod1_density_plots_SFluency_imputation_v2 <- mod1_density_plots_SFluency_imputation_v2 +
  scale_x_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.1))  +
  labs(title = "(b) Model 1 Imputation - Spanish Fluency") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )


############# Now for Model 2


############### Social Conflict Index
mod2_density_plots_SCI_imputation_v2 <- mcmc_areas(
  T2_v2 %>% select(b_SCI_Intercept, b_SCI_Age, b_SCI_Sex1),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod2_density_plots_SCI_imputation_v2 <- mod2_density_plots_SCI_imputation_v2 +
  scale_x_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.1)) +
  labs(title = "(c) Model 2 Imputation - Social Conflict Index") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )

############### Spanish Fluency

mod2_density_plots_SFluency_imputation_v2 <- mcmc_areas(
  T2_v2 %>% select(b_SFluency_Intercept, b_SFluency_Age, b_SFluency_Sex1),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod2_density_plots_SFluency_imputation_v2 <- mod2_density_plots_SFluency_imputation_v2 +
  scale_x_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.1))  +
  labs(title = "(d) Model 2 Imputation - Spanish Fluency") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )

############### Household Size

mod2_density_plots_HHSize_imputation_v2 <- mcmc_areas(
  T2_v2 %>% select(b_HHSize_Intercept, b_HHSize_Age, b_HHSize_Sex1, b_HHSize_Com_Size),
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
mod2_density_plots_HHSize_imputation_v2 <- mod2_density_plots_HHSize_imputation_v2 +
  scale_x_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.1))  +
  labs(title = "(e) Model 2 Imputation - Household Size") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )



############ Now plotting their sigmas 

## First wrangling everything into a single dataframe
imputation_sigmas_v2 <- data.frame(
  SCI_sigma_Model1 = T1_v2$sigma_SCI,
  SFluency_sigma_Model1 = T1_v2$sigma_SFluency,
  SCI_sigma_Model2 = T2_v2$sigma_SCI,
  SFluency_sigma_Model2 = T2_v2$sigma_SFluency,
  HHSize_sigma_Model2 = T2_v2$sigma_HHSize
)

# Plot
imputation_sigmas_plot_v2 <- mcmc_areas(
  imputation_sigmas_v2,
  prob = 0.95,         
  prob_outer = 0.95,   
  point_est = "mean"
)

# Make it a bit more tidy
imputation_sigmas_plot_v2 <- imputation_sigmas_plot_v2 +
  labs(title = "(f) Imputation Model Sigmas") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 24, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 19),
    axis.text.y = element_text(family = "Helvetica", size = 20)
  )

## Now combine all the plots into one
imp_models_fixed_effects_plot_v2 <- (mod1_density_plots_SCI_imputation_v2 | mod1_density_plots_SFluency_imputation_v2) / (mod2_density_plots_SCI_imputation_v2 | mod2_density_plots_SFluency_imputation_v2) / (mod2_density_plots_HHSize_imputation_v2 | imputation_sigmas_plot_v2)




