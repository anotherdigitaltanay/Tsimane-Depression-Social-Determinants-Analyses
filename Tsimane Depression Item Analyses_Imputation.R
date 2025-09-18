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
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                      prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D1Sad),
    
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
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                       prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D2Cry),
                       
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
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                       prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D3SelfCritical),
                       
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
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                       prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D4LifeNotValuable),
                       
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
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                       prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D5Useless),
                       
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
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                       prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D6NoInterest),
                       
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
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                       prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D7Tired),
                       
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
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                       prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D8CantConcentrate),
                       
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
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                       prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D9Nervous),
                       
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
                      Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                       prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D10Paranoid),
                       
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
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                        prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D11CantSleep),
                        
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
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                        prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D12CantEat),
                        
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
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                        prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D13CantFuck),
                        
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
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                        prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D14Pessimistic),
                        
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
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                        prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D16PunishMe),
                        
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
                       Com_Size + mi(HH_Size) + (1 |Interviewer) + (1 | ComID/UniqueFamID),
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
                        prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date), resp = D18HeavyThoughts),
                        
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
          
         