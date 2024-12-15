############### This script contains the latest depression analyses for the EMPH MAPPING Special Issue Paper                   #################
############### The analyses here are conducted only on the complete dataset (excluding missing observations)                  #################
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



############################# Data Analysis ######################################
##################################################################################
##################################################################################
##################################################################################


########### Running model 1 to attempt a close-replication of previous individual-level analysis of depression ##################
#################################################################################################################################

## Specifying the model
mod1 <- brm(D ~ 1 + Age + Sex + SCI + SFluency + (1 | PID), 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept"),
                      prior(exponential(1), class = sd),
                      prior(normal(0, 0.5), class ="b")),
            warmup = 1000, 
            iter   = 10000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99),
            seed = 601) 

## Save Model output (for others to easily load the model output later)
saveRDS(mod1, "model1_complete_case.RDS")

## Load model
mod1 <- readRDS("model1_complete_case.RDS")

## Looking at model summary
summary(mod1)

## Inspecting the trace plots to see if there is something unusual with the chains
## Looks good
plot(mod1)

#posterior predictive check
pp_check(mod1, ndraws = 100)

## Drawing the posterior distributions from model 1
T1 <- as_draws_df(mod1)

## Calculating ICCs for individual-level variance & individual-differences

## Residual Variance
icc_residual <- ((T1$sigma)^2) / ((T1$sigma)^2 + (T1$sd_PID__Intercept)^2) 
dens(icc_residual, col=2, lwd=4 , xlab="ICC (Residual) of Model 1")

## Individual Level Variance
icc_individual <-  ((T1$sd_PID__Intercept)^2) / ((T1$sigma)^2 + (T1$sd_PID__Intercept)^2)
dens(icc_individual, col=2, lwd=4 , xlab="ICC (Individual) of Model 1")

# Combine ICC vectors into a dataframe to allow for easy plotting of the two density plots in the same figure
icc_data <- data.frame(
  value = c(icc_residual, icc_individual),
  group = rep(c("Residual", "Individual"), each = length(icc_residual))
)

# Convert group variable to factor with specified levels (IMP for color coding in the plots)
icc_data$group <- factor(icc_data$group, levels = c("Residual", "Individual"))

# Final Plot for model 1
plot1 <- ggplot(icc_data, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "ICC Density Plots for Model 1",
       x = "ICC",
       y = "Density",
       color = "ICC Type") +
  theme_minimal()  +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = c("Residual" = "Green", "Individual" = "Blue")) +
  theme(plot.title = element_text(color = "darkblue"))


####################### Plotting model co-efficients

## Renaming Co-efficient Names for easy interpretability (in figure)
T1 <- T1 %>%
  rename("Intercept" = "b_Intercept",
         "Age" = "b_Age",
         "Male" = "b_Sex1",
         "SCI" = "b_SCI",
         "SFluency" = "b_SFluency")


mod1_density_plots <- mcmc_areas(
  T1 %>% select(Intercept, Age, Male, SCI, SFluency),
  prob = 0.90,         ## 90% density intervals
  prob_outer = 0.95,   ## 95% credible intervals
  point_est = "mean"
) 


mod1_density_plots <- mod1_density_plots + 
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, by = 0.1)) +
  labs(title = "Model 1") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 15, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 9),
    axis.text.y = element_text(family = "Helvetica", size = 11))



########### Running model 2 to quantify variation at higher levels (family, community, region) ##################
#################################################################################################################################

## Specifying the model
mod2 <- brm(D ~ 1 + Age + Sex + SCI + SFluency + (1 | Region/ComID/UniqueFamID/PID), 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept"),
                      prior(exponential(1), class = sd),
                      prior(normal(0, 0.5), class ="b")),
            warmup = 1000, 
            iter   = 10000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99),
            seed = 602) 

## Save Model output (for others to easily load the model output later)
saveRDS(mod2, "model2_complete_case.RDS")

## Load model
mod2 <- readRDS("model2_complete_case.RDS")

## Model summary
summary(mod2)

## Inspecting the trace plots to see if there is something unusual with the chains
## Looks good
plot(mod2)

# Posterior Predictive Check
pp_check(mod2, ndraws = 100)

## Drawing the posterior distributions from model 2
T2 <- as_draws_df(mod2)

## Calculating ICCs for residual, individual, family, community & region levels
icc_residual_2 <-   ((T2$sigma)^2) / ((T2$sigma)^2 + (T2$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T2$`sd_Region:ComID__Intercept`)^2 + (T2$sd_Region__Intercept)^2)
dens(icc_residual_2, col=2, lwd=4 , xlab="ICC (Residual) of Model 2")

icc_individual_2 <- ((T2$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2) / ((T2$sigma)^2 + (T2$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T2$`sd_Region:ComID__Intercept`)^2 + (T2$sd_Region__Intercept)^2)
dens(icc_individual_2, col=2, lwd=4 , xlab="ICC (Individual) of Model 2")

icc_family_2 <-  ((T2$`sd_Region:ComID:UniqueFamID__Intercept`)^2) / ((T2$sigma)^2 + (T2$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T2$`sd_Region:ComID__Intercept`)^2 + (T2$sd_Region__Intercept)^2)
dens(icc_family_2, col=2, lwd=4 , xlab="ICC (Family) of Model 2")

icc_community_2 <-  ((T2$`sd_Region:ComID__Intercept`)^2) / ((T2$sigma)^2 + (T2$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T2$`sd_Region:ComID__Intercept`)^2 + (T2$sd_Region__Intercept)^2)
dens(icc_community_2, col=2, lwd=4 , xlab="ICC (Community) of Model 2")

icc_region_2 <-  ((T2$sd_Region__Intercept)^2) / ((T2$sigma)^2 + (T2$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T2$`sd_Region:ComID__Intercept`)^2 + (T2$sd_Region__Intercept)^2)
dens(icc_region_2, col=2, lwd=4 , xlab="ICC (Region) of Model 2")


# Combine ICC vectors into a dataframe to allow for easy plotting of the five density plots in the same figure
icc_data_2 <- data.frame(
  value = c(icc_residual_2, icc_individual_2, icc_family_2, icc_community_2, icc_region_2),
  group = rep(c("Residual", "Individual", "Household", "Community", "Region"), each = length(icc_residual_2))
)

# Convert group variable to factor with specified levels (IMP for color coding in the plots)
icc_data_2$group <- factor(icc_data_2$group, levels = c("Residual", "Individual", "Household", "Community", "Region"))

# Final Plot for model 2
plot2 <- ggplot(icc_data_2, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "ICC Density Plots for Model 2",
       x = "ICC",
       y = "Density",
       color = "ICC Type") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = c("Residual" = "Green", "Individual" = "Blue", "Household" = "orange", "Community" = "red", "Region" = "darkgray")) +
  theme(plot.title = element_text(color = "darkblue"))


####################### Plotting model co-efficients

## Renaming Co-efficient Names for easy interpretability (in figure)
T2 <- T2 %>%
  rename("Intercept" = "b_Intercept",
         "Age" = "b_Age",
         "Male" = "b_Sex1",
         "SCI" = "b_SCI",
         "SFluency" = "b_SFluency")


mod2_density_plots <- mcmc_areas(
  T2 %>% select(Intercept, Age, Male, SCI, SFluency),
  prob = 0.90,         ## 90% density intervals
  prob_outer = 0.95,   ## 95% credible intervals
  point_est = "mean"
)

mod2_density_plots <- mod2_density_plots +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, by = 0.1)) +
  labs(title = "Model 2") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 15, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 9),
    axis.text.y = element_text(family = "Helvetica", size = 11))




########### Running model 3 to adjust for Interviewer and Time Effects ##################
#################################################################################################################################

## Specifying the model
mod3 <- brm(D ~ 1 + Age + Sex + SCI + SFluency + s(Interview_Date) + (1 | Interviewer) + (1 | Region/ComID/UniqueFamID/PID), 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept"),
                      prior(exponential(1), class = sd),
                      prior(normal(0, 0.5), class ="b")),
            warmup = 1000, 
            iter   = 10000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99),
            seed = 603) 

## Save Model output (for others to easily load the model output later)
saveRDS(mod3, "model3_complete_case.RDS")

## Load model
mod3 <- readRDS("model3_complete_case.RDS")

## Model summary
summary(mod3)

## Inspecting the trace plots to see if there is something unusual with the chains
## Looks good
plot(mod3)

# Posterior Predictive Check
pp_check(mod3, ndraws = 100)

## Drawing the posterior distributions from model 3
T3 <- as_draws_df(mod3)

## Calculating ICCs for residual, individual, family, community & region levels
icc_residual_3 <- ((T3$sigma)^2) / ((T3$sigma)^2 + (T3$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T3$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T3$`sd_Region:ComID__Intercept`)^2 + (T3$sd_Region__Intercept)^2 + (T3$sd_Interviewer__Intercept)^2)
dens(icc_residual_3, col=2, lwd=4 , xlab="ICC (Residual) of Model 3")

icc_individual_3 <- ((T3$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2) / ((T3$sigma)^2 + (T3$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T3$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T3$`sd_Region:ComID__Intercept`)^2 + (T3$sd_Region__Intercept)^2 + (T3$sd_Interviewer__Intercept)^2)
dens(icc_individual_3, col=2, lwd=4 , xlab="ICC (Individual) of Model 3")

icc_family_3 <- ((T3$`sd_Region:ComID:UniqueFamID__Intercept`)^2) / ((T3$sigma)^2 + (T3$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T3$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T3$`sd_Region:ComID__Intercept`)^2 + (T3$sd_Region__Intercept)^2 + (T3$sd_Interviewer__Intercept)^2)
dens(icc_family_3, col=2, lwd=4 , xlab="ICC (Family) of Model 3")

icc_community_3 <- ((T3$`sd_Region:ComID__Intercept`)^2) / ((T3$sigma)^2 + (T3$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T3$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T3$`sd_Region:ComID__Intercept`)^2 + (T3$sd_Region__Intercept)^2 + (T3$sd_Interviewer__Intercept)^2)
dens(icc_community_3, col=2, lwd=4 , xlab="ICC (Community) of Model 3")

icc_region_3 <- ((T3$sd_Region__Intercept)^2) / ((T3$sigma)^2 + (T3$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T3$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T3$`sd_Region:ComID__Intercept`)^2 + (T3$sd_Region__Intercept)^2 + (T3$sd_Interviewer__Intercept)^2)
dens(icc_region_3, col=2, lwd=4 , xlab="ICC (Region) of Model 3")

icc_interviewer_3 <- ((T3$sd_Interviewer__Intercept)^2) / ((T3$sigma)^2 + (T3$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T3$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T3$`sd_Region:ComID__Intercept`)^2 + (T3$sd_Region__Intercept)^2 + (T3$sd_Interviewer__Intercept)^2)
dens(icc_interviewer_3, col=2, lwd=4 , xlab="ICC (Interviewer) of Model 3")


# Combine ICC vectors into a dataframe to allow for easy plotting of the five density plots in the same figure
icc_data_3 <- data.frame(
  value = c(icc_residual_3, icc_individual_3, icc_family_3, icc_community_3, icc_region_3, icc_interviewer_3),
  group = rep(c("Residual", "Individual", "Household", "Community", "Region", "Interviewer"), each = length(icc_residual_3))
)

# Convert group variable to factor with specified levels (IMP for color coding in the plots)
icc_data_3$group <- factor(icc_data_3$group, levels = c("Residual", "Individual", "Household", "Community", "Region", "Interviewer"))


# Final Plot for model 3
plot3 <- ggplot(icc_data_3, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "ICC Density Plots for Model 3",
       x = "ICC",
       y = "Density",
       color = "ICC Type") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = c("Residual" = "Green", "Individual" = "Blue", "Household" = "orange", "Community" = "red", "Region" = "darkgray", "Interviewer" = "darkcyan")) +
  theme(plot.title = element_text(color = "darkblue"))


####################### Plotting model co-efficients

## Renaming Co-efficient Names for easy interpretability (in figure)
T3 <- T3 %>%
  rename("Intercept" = "b_Intercept",
         "Age" = "b_Age",
         "Male" = "b_Sex1",
         "SCI" = "b_SCI",
         "SFluency" = "b_SFluency")


mod3_density_plots <- mcmc_areas(
  T3 %>% select(Intercept, Age, Male, SCI, SFluency),
  prob = 0.90,         ## 90% density intervals
  prob_outer = 0.95,   ## 95% credible intervals
  point_est = "mean"
)

mod3_density_plots <- mod3_density_plots +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, by = 0.1)) +
  labs(title = "Model 3") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 15, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 7),
    axis.text.y = element_text(family = "Helvetica", size = 11))


########### Plotting Effect of Interview Date

## Draw conditional effects plot
spline_plot_mod3 <- conditional_effects(mod3, "Interview_Date") 

## Get the data conditional effects used to generate this plot
time_effect_data_mod3 <- spline_plot_mod3$Interview_Date

# Step 2: Reverse the standardization in this dataset to make the graph more readable (i.e. Original Interview Date instead of standardised interview date)
# The original Interview_Date was transformed as follows:
# Interview_Date_numeric = as.numeric(Interview_Date)
# Interview_Date_standardized = (Interview_Date_numeric - mean(Interview_Date_numeric)) / sd(Interview_Date_numeric)

# To reverse it, use the mean and sd of Interview_Date from the original dataset where interview dates weren't transformed:
## Note we use depression_data_v2 instead of dep_data. This is because dep_data has a standardised version of interview dates, which the former dataset doesn't
depression_data_v2 <- na.omit(depression_data)
depression_data_v2$InterviewDate <- as.POSIXct(depression_data_v2$InterviewDate, tz = "UTC")

mean_interview_date <- mean(as.numeric(depression_data_v2$InterviewDate))
sd_interview_date <- sd(as.numeric(depression_data_v2$InterviewDate))

## Reversing the standardisatiob and converting it to date format
time_effect_data_mod3$Interview_Date <- (time_effect_data_mod3$Interview_Date * sd_interview_date) + mean_interview_date
time_effect_data_mod3$Interview_Date <- as.POSIXct(time_effect_data_mod3$Interview_Date, origin = "1970-01-01")

# Step 3: Update the plot
time_effect_mod3_plot <- ggplot(time_effect_data_mod3, aes(x = Interview_Date, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1) +
  geom_point(data = time_effect_data_mod3[time_effect_data_mod3$points__, ], aes(x = Interview_Date, y = estimate__)) +
  labs(x = "Interview Date", y = "Standardized Depression Score") +
  theme_minimal()




########### Running model 4 to see if higher level variables explain higher level variation ##################
#################################################################################################################################

## Specifying the model
mod4 <- brm(D ~ 1 + Age + Sex + SCI + SFluency + s(Interview_Date) + Town_Distance + Com_Size + HH_Size + (1 | Interviewer) + (1 | Region/ComID/UniqueFamID/PID), 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept"),
                      prior(exponential(1), class = sd),
                      prior(normal(0, 0.5), class ="b")),
            warmup = 1000, 
            iter   = 10000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99, max_treedepth = 15),
            seed = 604) 

## Save Model output (for others to easily load the model output later)
saveRDS(mod4, "model4_complete_case.RDS")

## Load model
mod4 <- readRDS("model4_complete_case.RDS")

## Model summary
summary(mod4)

## Inspecting the trace plots to see if there is something unusual with the chains
## Looks good
plot(mod4)

# Posterior Predictive Check
pp_check(mod4, ndraws = 100)

## Drawing the posterior distributions from model 3
T4 <- as_draws_df(mod4)

## Calculating ICCs for residual, individual, family, community & region levels
icc_residual_4 <-  ((T4$sigma)^2) / ((T4$sigma)^2 + (T4$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T4$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T4$`sd_Region:ComID__Intercept`)^2 + (T4$sd_Region__Intercept)^2  + (T4$sd_Interviewer__Intercept)^2)
dens(icc_residual_4, col=2, lwd=4 , xlab="ICC (Residual) of Model 4")

icc_individual_4 <-  ((T4$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2) / ((T4$sigma)^2 + (T4$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T4$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T4$`sd_Region:ComID__Intercept`)^2 + (T4$sd_Region__Intercept)^2  + (T4$sd_Interviewer__Intercept)^2)
dens(icc_individual_4, col=2, lwd=4 , xlab="ICC (Individual) of Model 4")

icc_family_4 <- ((T4$`sd_Region:ComID:UniqueFamID__Intercept`)^2) / ((T4$sigma)^2 + (T4$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T4$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T4$`sd_Region:ComID__Intercept`)^2 + (T4$sd_Region__Intercept)^2  + (T4$sd_Interviewer__Intercept)^2)
dens(icc_family_4, col=2, lwd=4 , xlab="ICC (Family) of Model 4")

icc_community_4 <- ((T4$`sd_Region:ComID__Intercept`)^2) / ((T4$sigma)^2 + (T4$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T4$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T4$`sd_Region:ComID__Intercept`)^2 + (T4$sd_Region__Intercept)^2  + (T4$sd_Interviewer__Intercept)^2)
dens(icc_community_4, col=2, lwd=4 , xlab="ICC (Community) of Model 4")

icc_region_4 <-  ((T4$sd_Region__Intercept)^2) / ((T4$sigma)^2 + (T4$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T4$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T4$`sd_Region:ComID__Intercept`)^2 + (T4$sd_Region__Intercept)^2  + (T4$sd_Interviewer__Intercept)^2)
dens(icc_region_4, col=2, lwd=4 , xlab="ICC (Region) of Model 4")

icc_interviewer_4 <- ((T4$sd_Interviewer__Intercept)^2) / ((T4$sigma)^2 + (T4$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2 + (T4$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + (T4$`sd_Region:ComID__Intercept`)^2 + (T4$sd_Region__Intercept)^2  + (T4$sd_Interviewer__Intercept)^2)
dens(icc_interviewer_4, col=2, lwd=4 , xlab="ICC (Interviewer) of Model 4")

# Combine ICC vectors into a dataframe to allow for easy plotting of the six density plots in the same figure
icc_data_4 <- data.frame(
  value = c(icc_residual_4, icc_individual_4, icc_family_4, icc_community_4, icc_region_4, icc_interviewer_4),
  group = rep(c("Residual", "Individual", "Household", "Community", "Region", "Interviewer"), each = length(icc_residual_4))
)

# Convert group variable to factor with specified levels (IMP for color coding in the plots)
icc_data_4$group <- factor(icc_data_4$group, levels = c("Residual", "Individual", "Household", "Community", "Region", "Interviewer"))



# Final Plot for model 4
plot4 <- ggplot(icc_data_4, aes(x = value, color = group)) +
  geom_density(size = 1.0) +
  labs(title = "ICC Density Plots for Model 4",
       x = "ICC",
       y = "Density",
       color = "ICC Type") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = c("Residual" = "Green", "Individual" = "Blue", "Household" = "orange", "Community" = "red", "Region" = "darkgray", "Interviewer" = "darkcyan")) +
  theme(plot.title = element_text(color = "darkblue"))


####################### Plotting model co-efficients

## Renaming Co-efficient Names for easy interpretability (in figure)
T4 <- T4 %>%
  rename("Intercept" = "b_Intercept",
         "Age" = "b_Age",
         "Male" = "b_Sex1",
         "SCI" = "b_SCI",
         "SFluency" = "b_SFluency",
         "Town_Distance" = "b_Town_Distance",
         "C_Size" = "b_Com_Size",
         "H_Size" = "b_HH_Size"
         )


mod4_density_plots <- mcmc_areas(
  T4 %>% select(Intercept, Age, Male, SCI, SFluency, Town_Distance, C_Size, H_Size),
  prob = 0.90,         ## 90% density intervals
  prob_outer = 0.95,   ## 95% credible intervals
  point_est = "mean"
)

mod4_density_plots <- mod4_density_plots +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, by = 0.1)) +
  labs(title = "Model 4") +
  geom_line(color = "black", size = 1) +
  theme(
    plot.title = element_text(family = "Helvetica", size = 15, face = "bold", hjust = 0.5),  # Center the title
    axis.text.x = element_text(family = "Helvetica", size = 7),
    axis.text.y = element_text(family = "Helvetica", size = 11))


########### Plotting Effect of Interview Date

## Draw conditional effects plot
spline_plot_mod4 <- conditional_effects(mod4, "Interview_Date") 

## Get the data conditional effects used to generate this plot
time_effect_data_mod4 <- spline_plot_mod4$Interview_Date

# Step 2: Reverse the standardization in this dataset to make the graph more readable (i.e. Original Interview Date instead of standardised interview date)
## Mean and SD already calculated above

## Reversing the standardisation and converting it to date format
time_effect_data_mod4$Interview_Date <- (time_effect_data_mod4$Interview_Date * sd_interview_date) + mean_interview_date
time_effect_data_mod4$Interview_Date <- as.POSIXct(time_effect_data_mod3$Interview_Date, origin = "1970-01-01")

# Step 3: Update the plot
time_effect_mod4_plot <- ggplot(time_effect_data_mod4, aes(x = Interview_Date, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1) +
  geom_point(data = time_effect_data_mod4[time_effect_data_mod4$points__, ], aes(x = Interview_Date, y = estimate__)) +
  labs(x = "Interview Date", y = "Standardized Depression Score") +
  theme_minimal()









############### Plots and Tables presented in the manuscript
##############################################################

################ Combined ICC Plot for all 4 models

## Combine plots from all four models
combined_icc_plot <- (plot1 | plot2) / (plot3 | plot4)

# Print the combined plot
print(combined_icc_plot)


################ Combined Model Co-efficients Plot for all 4 models

combined_density_plot <- mod1_density_plots + mod2_density_plots + mod3_density_plots + mod4_density_plots
print(combined_density_plot)



################### Tables

#### Summarizing the multilevel structure of the dataset
##################################################################################################################
##################################################################################################################

# Make summary dataframe for the multilevel structure of our dataset
multilevel_summary_stats <- dep_data %>%
  summarise(
    Variable = c("Individuals", "Households", "Communities", "Regions", "Interviewers", "Total Observations contributed by all these levels"),
    N = c(n_distinct(PID_FAMID), n_distinct(UniqueFamID), n_distinct(ComID), n_distinct(Region), n_distinct(Interviewer), n()))



## We now descriptively summarise all the predictors used in our analyses
##################################################################################################################
##################################################################################################################

## Since the dep_data data frame has variable transformations, it might be difficult to extract summary statistics of the raw values
## So, we first create a new dataset that has the untransformed values
depression_data_v2 <- na.omit(depression_data)

## Converting InterviewDate to POSXIct format so that standardising it using a numeric format becomes easier
depression_data_v2$InterviewDate <- as.POSIXct(depression_data_v2$InterviewDate, tz = "UTC")


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
    Summary_Statistics_Calculated_Over = c("Entire Sample: 2002", "Entire Sample: 2002", "Unique Individuals: 1400", "Entire Sample: 2002", "Entire Sample: 2002",
                                           "Unique Communities: 62", "Unique Communities: 62", "Entire Sample: 2002", "Entire Sample: 2002"),
    Mean = c(round(mean(DepressionScoreM2), 2), round(mean(Age), 2), paste(percentage_males, "% of Males"), round(mean(SocialConflictIndex), 2), 
             round(mean(SpanishFluency), 2), mean_distance, mean_csize, round(mean(household_size), 2), mean_date_only),
    SD = c(round(sd(DepressionScoreM2), 2), round(sd(Age), 2), "-", round(sd(SocialConflictIndex), 2), round(sd(SpanishFluency), 2),
           sd_distance, sd_csize, round(sd(household_size), 2), "-"),
    Median = c(median(DepressionScoreM2), round(median(Age), 2), "-", median(SocialConflictIndex), median(SpanishFluency),
               median_distance, median_csize, median(household_size), median_date),
    Min = c(min(DepressionScoreM2), round(min(Age), 2), "-", min(SocialConflictIndex), min(SpanishFluency),
            min(route_distance_town), min(max_community_size), min(household_size), min_date),
    Max = c(max(DepressionScoreM2), round(max(Age), 2), "-", max(SocialConflictIndex), max(SpanishFluency),
            max(route_distance_town), max(max_community_size), max(household_size), max_date), 
    Q1 = c(quantile(DepressionScoreM2, 0.25), round(quantile(Age, 0.25), 2), "-", quantile(SocialConflictIndex, 0.25), 
           quantile(SpanishFluency, 0.25), round(lower_quin_distance, 2), round(lower_quin_csize, 2),
           quantile(household_size, 0.25), lower_quin_date),
    Q3 = c(quantile(DepressionScoreM2, 0.75), round(quantile(Age, 0.75), 2), "-", quantile(SocialConflictIndex, 0.75),
           quantile(SpanishFluency, 0.75), round(upper_quin_distance, 2), round(upper_quin_csize, 2),
           quantile(household_size, 0.75), upper_quin_date))


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
                                                    "-", "-", "-", round(mean((T1$sigma)^2), 2), 
                                                    paste(round(mean((T1$sd_PID__Intercept)^2), 2), " (PID)"),  "-", "-", "-", "-", paste(round(mean(icc_residual), 2), " (Residual)"), paste(round(mean(icc_individual), 2), "(Individual)"),
                                                    "-", "-", "-", "-", round(mod1_R2["R2", "Estimate"], 2)),
                             CI_Model_1 = c( paste(round(quantile(T1$Intercept, 0.025), 2), "-", round(quantile(T1$Intercept, 0.975), 2)), paste(round(quantile(T1$Age, 0.025), 2), "-", round(quantile(T1$Age, 0.975), 2)),
                                             paste(round(quantile(T1$Male, 0.025), 2), "-", round(quantile(T1$Male, 0.975), 2)), paste(round(quantile(T1$SCI, 0.025), 2), "-", round(quantile(T1$SCI, 0.975), 2)),
                                             paste(round(quantile(T1$SFluency, 0.025), 2), "-", round(quantile(T1$SFluency, 0.975), 2)), "-", "-",
                                             "-", "-", "-",
                                             paste(round(quantile((T1$sigma)^2, 0.025), 2), "-", round(quantile((T1$sigma)^2, 0.975), 2)), 
                                             paste(round(quantile((T1$sd_PID__Intercept)^2, 0.025), 2), "-", round(quantile((T1$sd_PID__Intercept)^2, 0.975), 2)), "-", "-", "-", "-",
                                             paste(round(quantile(icc_residual, 0.025), 2), "-", round(quantile(icc_residual, 0.975), 2)), 
                                             paste(round(quantile(icc_individual, 0.025), 2), "-", round(quantile(icc_individual, 0.975), 2)), "-", "-", "-", "-", paste(round(mod1_R2["R2", "Q2.5"], 2), "-", round(mod1_R2["R2", "Q97.5"], 2))),
                             
                             
                             Estimates_Model_2 = c( round(mean(T2$Intercept),2), round(mean(T2$Age),2), round(mean(T2$Male),2), round(mean(T2$SCI),2), round(mean(T2$SFluency),2),
                                                    "-", "-",
                                                    "-", "-", "-", round(mean((T2$sigma)^2), 2), 
                                                    paste(round(mean((T2$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2), 2), " (PID)"), paste(round(mean((T2$`sd_Region:ComID:UniqueFamID__Intercept`)^2), 2), " (FamID)"),
                                                    paste(round(mean((T2$`sd_Region:ComID__Intercept`)^2), 2), " (ComID)"), paste(round(mean((T2$sd_Region__Intercept)^2), 2), " (RegionID)"), "-",
                                                    paste(round(mean(icc_residual_2), 2), " (Residual)"), paste(round(mean(icc_individual_2), 2), "(Individual)"),
                                                    paste(round(mean(icc_family_2), 2), "(Household)"), paste(round(mean(icc_community_2), 2), "(Community)"),
                                                    paste(round(mean(icc_region_2), 2), "(Region)"), "-", round(mod2_R2["R2", "Estimate"], 2)), 
                             CI_Model_2 = c( paste(round(quantile(T2$Intercept, 0.025), 2), "-", round(quantile(T2$Intercept, 0.975), 2)), paste(round(quantile(T2$Age, 0.025), 2), "-", round(quantile(T2$Age, 0.975), 2)),
                                             paste(round(quantile(T2$Male, 0.025), 2), "-", round(quantile(T2$Male, 0.975), 2)), paste(round(quantile(T2$SCI, 0.025), 2), "-", round(quantile(T2$SCI, 0.975), 2)), 
                                             paste(round(quantile(T2$SFluency, 0.025), 2), "-", round(quantile(T2$SFluency, 0.975), 2)), "-", "-",
                                             "-", "-", "-",
                                             paste(round(quantile((T2$sigma)^2, 0.025), 2), "-", round(quantile((T2$sigma)^2, 0.975), 2)), 
                                             paste(round(quantile((T2$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2, 0.025), 2), "-", round(quantile((T2$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2, 0.975), 2)), 
                                             paste(round(quantile((T2$`sd_Region:ComID:UniqueFamID__Intercept`)^2, 0.025), 2), "-", round(quantile((T2$`sd_Region:ComID:UniqueFamID__Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T2$`sd_Region:ComID__Intercept`)^2, 0.025), 2), "-", round(quantile((T2$`sd_Region:ComID__Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T2$sd_Region__Intercept)^2, 0.025), 2), "-", round(quantile((T2$sd_Region__Intercept)^2, 0.975), 2)), "-",
                                             paste(round(quantile(icc_residual_2, 0.025), 2), "-", round(quantile(icc_residual_2, 0.975), 2)), 
                                             paste(round(quantile(icc_individual_2, 0.025), 2), "-", round(quantile(icc_individual_2, 0.975), 2)), 
                                             paste(round(quantile(icc_family_2, 0.025), 2), "-", round(quantile(icc_family_2, 0.975), 2)),
                                             paste(round(quantile(icc_community_2, 0.025), 2), "-", round(quantile(icc_community_2, 0.975), 2)),
                                             paste(round(quantile(icc_region_2, 0.025), 2), "-", round(quantile(icc_region_2, 0.975), 2)), "-", paste( round(mod2_R2["R2", "Q2.5"],2), "-", round(mod2_R2["R2", "Q97.5"],2) )),
                             
                             Estimates_Model_3 = c( round(mean(T3$Intercept),2), round(mean(T3$Age),2), round(mean(T3$Male),2), round(mean(T3$SCI),2), 
                                                    round(mean(T3$SFluency),2), round(mean(T3$bs_sInterview_Date_1),2), round(mean(T3$sds_sInterview_Date_1),2),
                                                    "-", "-", "-", round(mean((T3$sigma)^2), 2), 
                                                    paste(round(mean((T3$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2), 2), " (PID)"), paste(round(mean((T3$`sd_Region:ComID:UniqueFamID__Intercept`)^2), 2), " (FamID)"),
                                                    paste(round(mean((T3$`sd_Region:ComID__Intercept`)^2), 2), " (ComID)"), paste(round(mean((T3$sd_Region__Intercept)^2), 2), " (RegionID)"), 
                                                    paste(round(mean((T3$sd_Interviewer__Intercept)^2), 2), " (InterviewerID)"),
                                                    paste(round(mean(icc_residual_3), 2), " (Residual)"), paste(round(mean(icc_individual_3), 2), "(Individual)"),
                                                    paste(round(mean(icc_family_3), 2), "(Household)"), paste(round(mean(icc_community_3), 2), "(Community)"),
                                                    paste(round(mean(icc_region_3), 2), "(Region)"), paste(round(mean(icc_interviewer_3), 2), "(Interviewer)"), 
                                                    round(mod3_R3["R2", "Estimate"], 2)), 
                             CI_Model_3 = c( paste(round(quantile(T3$Intercept, 0.025), 2), "-", round(quantile(T3$Intercept, 0.975), 2)), paste(round(quantile(T3$Age, 0.025), 2), "-", round(quantile(T3$Age, 0.975), 2)),
                                             paste(round(quantile(T3$Male, 0.025), 2), "-", round(quantile(T3$Male, 0.975), 2)), paste(round(quantile(T3$SCI, 0.025), 2), "-", round(quantile(T3$SCI, 0.975), 2)), 
                                             paste(round(quantile(T3$SFluency, 0.025), 2), "-", round(quantile(T3$SFluency, 0.975), 2)), 
                                             paste(round(quantile(T3$bs_sInterview_Date_1, 0.025), 2), "-", round(quantile(T3$bs_sInterview_Date_1, 0.975), 2)), 
                                             paste(round(quantile(T3$sds_sInterview_Date_1, 0.025), 2), "-", round(quantile(T3$sds_sInterview_Date_1, 0.975), 2)),
                                             "-",
                                             "-", "-",
                                             paste(round(quantile((T3$sigma)^2, 0.025), 2), "-", round(quantile((T3$sigma)^2, 0.975), 2)), 
                                             paste(round(quantile((T3$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2, 0.025), 2), "-", round(quantile((T3$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2, 0.975), 2)), 
                                             paste(round(quantile((T3$`sd_Region:ComID:UniqueFamID__Intercept`)^2, 0.025), 2), "-", round(quantile((T3$`sd_Region:ComID:UniqueFamID__Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T3$`sd_Region:ComID__Intercept`)^2, 0.025), 2), "-", round(quantile((T3$`sd_Region:ComID__Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T3$sd_Region__Intercept)^2, 0.025), 2), "-", round(quantile((T3$sd_Region__Intercept)^2, 0.975), 2)), 
                                             paste(round(quantile((T3$sd_Interviewer__Intercept)^2, 0.025), 2), "-", round(quantile((T3$sd_Interviewer__Intercept)^2, 0.975), 2)),
                                             paste(round(quantile(icc_residual_3, 0.025), 2), "-", round(quantile(icc_residual_3, 0.975), 2)), 
                                             paste(round(quantile(icc_individual_3, 0.025), 2), "-", round(quantile(icc_individual_3, 0.975), 2)), 
                                             paste(round(quantile(icc_family_3, 0.025), 2), "-", round(quantile(icc_family_3, 0.975), 2)),
                                             paste(round(quantile(icc_community_3, 0.025), 2), "-", round(quantile(icc_community_3, 0.975), 2)),
                                             paste(round(quantile(icc_region_3, 0.025), 2), "-", round(quantile(icc_region_3, 0.975), 2)), 
                                             paste(round(quantile(icc_interviewer_3, 0.025), 2), "-", round(quantile(icc_interviewer_3, 0.975), 2)),
                                             paste( round(mod3_R3["R2", "Q2.5"], 2), "-", round(mod3_R3["R2", "Q97.5"], 2)  )),
                             
                             Estimates_Model_4 = c( round(mean(T4$Intercept),2), round(mean(T4$Age),2), round(mean(T4$Male),2), round(mean(T4$SCI),2), 
                                                    round(mean(T4$SFluency),2), round(mean(T4$bs_sInterview_Date_1),2), round(mean(T4$sds_sInterview_Date_1),2),
                                                    round(mean(T4$Town_Distance),2), round(mean(T4$C_Size),2), round(mean(T4$H_Size),2),
                                                    round(mean((T4$sigma)^2), 2), 
                                                    paste(round(mean((T4$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2), 2), " (PID)"), paste(round(mean((T4$`sd_Region:ComID:UniqueFamID__Intercept`)^2), 2), " (FamID)"),
                                                    paste(round(mean((T4$`sd_Region:ComID__Intercept`)^2), 2), " (ComID)"), paste(round(mean((T4$sd_Region__Intercept)^2), 2), " (RegionID)"),
                                                    paste(round(mean((T4$sd_Interviewer__Intercept)^2), 2), " (InterviewerID)"),
                                                    paste(round(mean(icc_residual_4), 2), " (Residual)"), paste(round(mean(icc_individual_4), 2), "(Individual)"),
                                                    paste(round(mean(icc_family_4), 2), "(Household)"), paste(round(mean(icc_community_4), 2), "(Community)"),
                                                    paste(round(mean(icc_region_4), 2), "(Region)"), paste(round(mean(icc_interviewer_4), 2), "(Interviewer)"), round(mod4_R4["R2", "Estimate"], 2) ),
                             CI_Model_4 = c( paste(round(quantile(T4$Intercept, 0.025), 2), "-", round(quantile(T4$Intercept, 0.975), 2)), paste(round(quantile(T4$Age, 0.025), 2), "-", round(quantile(T4$Age, 0.975), 2)),
                                             paste(round(quantile(T4$Male, 0.025), 2), "-", round(quantile(T4$Male, 0.975), 2)), paste(round(quantile(T4$SCI, 0.025), 2), "-", round(quantile(T4$SCI, 0.975), 2)), 
                                             paste(round(quantile(T4$SFluency, 0.025), 2), "-", round(quantile(T4$SFluency, 0.975), 2)),
                                             paste(round(quantile(T4$bs_sInterview_Date_1, 0.025), 2), "-", round(quantile(T4$bs_sInterview_Date_1, 0.975), 2)),
                                             paste(round(quantile(T4$sds_sInterview_Date_1, 0.025), 2), "-", round(quantile(T4$sds_sInterview_Date_1, 0.975), 2)),
                                             paste(round(quantile(T4$Town_Distance, 0.025), 2), "-", round(quantile(T4$Town_Distance, 0.975), 2)),
                                             paste(round(quantile(T4$C_Size, 0.025), 2), "-", round(quantile(T4$C_Size, 0.975), 2)),
                                             paste(round(quantile(T4$H_Size, 0.025), 2), "-", round(quantile(T4$H_Size, 0.975), 2)),
                                             
                                             paste(round(quantile((T4$sigma)^2, 0.025), 2), "-", round(quantile((T4$sigma)^2, 0.975), 2)), 
                                             paste(round(quantile((T4$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2, 0.025), 2), "-", round(quantile((T4$`sd_Region:ComID:UniqueFamID:PID__Intercept`)^2, 0.975), 2)), 
                                             paste(round(quantile((T4$`sd_Region:ComID:UniqueFamID__Intercept`)^2, 0.025), 2), "-", round(quantile((T4$`sd_Region:ComID:UniqueFamID__Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T4$`sd_Region:ComID__Intercept`)^2, 0.025), 2), "-", round(quantile((T4$`sd_Region:ComID__Intercept`)^2, 0.975), 2)),
                                             paste(round(quantile((T4$sd_Region__Intercept)^2, 0.025), 2), "-", round(quantile((T4$sd_Region__Intercept)^2, 0.975), 2)),
                                             paste(round(quantile((T4$sd_Interviewer__Intercept)^2, 0.025), 2), "-", round(quantile((T4$sd_Interviewer__Intercept)^2, 0.975), 2)),
                                             paste(round(quantile(icc_residual_4, 0.025), 2), "-", round(quantile(icc_residual_4, 0.975), 2)), 
                                             paste(round(quantile(icc_individual_4, 0.025), 2), "-", round(quantile(icc_individual_4, 0.975), 2)), 
                                             paste(round(quantile(icc_family_4, 0.025), 2), "-", round(quantile(icc_family_4, 0.975), 2)),
                                             paste(round(quantile(icc_community_4, 0.025), 2), "-", round(quantile(icc_community_4, 0.975), 2)),
                                             paste(round(quantile(icc_region_4, 0.025), 2), "-", round(quantile(icc_region_4, 0.975), 2)), 
                                             paste(round(quantile(icc_interviewer_4, 0.025), 2), "-", round(quantile(icc_interviewer_4, 0.975), 2)), paste( round(mod4_R4["R2", "Q2.5"],2) , "-", round(mod4_R4["R2", "Q97.5"],2)  ))
                             
)







#################### Generating the summary tables that are used in the manuscript
##################################################################################
##################################################################################

################### First the summary table of the multilevel structure of our data table
multilevel_summary_stats_ft <- flextable(multilevel_summary_stats)

# Bold the column headers, apply a grey backdrop to the first column and autofit the table
multilevel_summary_stats_ft <- bold(multilevel_summary_stats_ft, part = "header")
multilevel_summary_stats_ft <- bg(multilevel_summary_stats_ft, j = 1, bg = "#D3D3D3", part = "body")
multilevel_summary_stats_ft <- autofit(multilevel_summary_stats_ft)

## Can copy the table directly from here on to word
multilevel_summary_stats_ft

################### Then the descriptive summary table for all the predictors
summary_stats_ft <- flextable(summary_stats)

# Bold the column headers, apply a grey backdrop to the first column and autofit the table
summary_stats_ft <- bold(summary_stats_ft, part = "header")
summary_stats_ft <- bg(summary_stats_ft, j = 1, bg = "#D3D3D3", part = "body")
summary_stats_ft <- autofit(summary_stats_ft)

## Can copy the table directly from here on to word
summary_stats_ft

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













