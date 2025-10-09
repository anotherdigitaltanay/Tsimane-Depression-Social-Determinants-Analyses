############### This script contains the latest Tsimane depression analyses                                                    #################
############### The analyses here are conducted only on the complete dataset (excluding missing observations)                  #################
############### Posterior Plots that are used in the manuscript are generated towards the end of the script           #################

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


########### Running model 1 to quantify the amount of variation present at different levels ##################
#################################################################################################################################

## Specifying the model
mod1 <- brm(D ~ 1 + Age + Sex + SCI + SFluency + s(Interview_Date) + (1 | Interviewer) + (1 | Region/ComID/UniqueFamID) + (1| PID), 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept"),
                      prior(exponential(1), class = sd),
                      prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date)),
                      prior(normal(0, 0.5), class ="b")),
            warmup = 1000, 
            iter   = 6000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99, max_treedepth = 12),
            seed = 603) 

## Save Model output (for others to easily load the model output later)
saveRDS(mod1, "sumscore_model_1_complete_case.RDS")

## Load the model
mod1 <- readRDS("sumscore_model_1_complete_case.RDS")



####### Given that we see minimal variation at the PID and region level, let's make drop these grouping factors to make it less complex
#######


## Specifying the model
mod1_v2 <- brm(D ~ 1 + Age + Sex + SCI + SFluency + s(Interview_Date) + (1 | Interviewer) + (1 | ComID/UniqueFamID), 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept"),
                      prior(exponential(1), class = sd),
                      prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date)),
                      prior(normal(0, 0.5), class ="b")),
            warmup = 1000, 
            iter   = 6000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99, max_treedepth = 12),
            seed = 604) 

## Save Model output (for others to easily load the model output later)
saveRDS(mod1_v2, "sumscore_model_1_version2_complete_case.RDS")

## Load the model
mod1_v2 <- readRDS("sumscore_model_1_version2_complete_case.RDS")



########### Running model 2 to see if higher level variables explain higher level variation ##################
#################################################################################################################################

## Specifying the model
mod2 <- brm(D ~ 1 + Age + Sex + SCI + SFluency + s(Interview_Date) + Town_Distance + Com_Size + HH_Size + (1 | Interviewer) + (1 | Region/ComID/UniqueFamID) + (1 | PID), 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept"),
                      prior(exponential(1), class = sd),
                      prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date)),
                      prior(normal(0, 0.5), class ="b")),
            warmup = 1000, 
            iter   = 6000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99, max_treedepth = 12),
            seed = 613) 

## Save Model output (for others to easily load the model output later)
saveRDS(mod2, "sumscore_model_2_complete_case.RDS")


## Load the model
mod2 <- readRDS("sumscore_model_2_complete_case.RDS")



####### Given that we see minimal variation at the PID and region level, let's make drop these grouping factors to make it less complex
#######

mod2_v2 <- brm(D ~ 1 + Age + Sex + SCI + SFluency + s(Interview_Date) + Town_Distance + Com_Size + HH_Size + (1 | Interviewer) + (1 | ComID/UniqueFamID), 
            data   = dep_data,
            family = gaussian,
            prior = c(prior(normal(0, 0.5), class="Intercept"),
                      prior(exponential(1), class = sd),
                      prior(student_t(3, 0, 1), class = "sds", coef = s(Interview_Date)),
                      prior(normal(0, 0.5), class ="b")),
            warmup = 1000, 
            iter   = 6000, 
            chains = 4, 
            cores  = 8,
            control = list(adapt_delta = 0.99, max_treedepth = 12),
            seed = 615) 

## Save Model output (for others to easily load the model output later)
saveRDS(mod2_v2, "sumscore_model_2_version2_complete_case.RDS")

## Load the model
mod2_v2 <- readRDS("sumscore_model_2_version2_complete_case.RDS")



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
  rename("Intercept" = "b_Intercept",
         "Age" = "b_Age",
         "Male" = "b_Sex1",
         "SCI" = "b_SCI",
         "SFluency" = "b_SFluency"
  )

T2 <- T2 %>%
  rename("Intercept" = "b_Intercept",
         "Age" = "b_Age",
         "Male" = "b_Sex1",
         "SCI" = "b_SCI",
         "SFluency" = "b_SFluency",
         "Town_Distance" = "b_Town_Distance",
         "C_Size" = "b_Com_Size",
         "H_Size" = "b_HH_Size"
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
time_effect_data_mod1 <- spline_plot_mod1$Interview_Date

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
time_effect_data_mod2 <- spline_plot_mod2$Interview_Date

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
icc_residual_1 <- ( (T1$sigma)^2 ) /    
  ( (T1$sigma)^2 +        
      (T1$`sd_ComID:UniqueFamID__Intercept`)^2 +        
      (T1$`sd_ComID__Intercept`)^2 +        
      (T1$sd_Interviewer__Intercept)^2 )

dens(icc_residual_1, col=2, lwd=4, xlab="ICC (Residual) of Model 1")


icc_family_1 <- ( (T1$`sd_ComID:UniqueFamID__Intercept`)^2 ) /    
  ( (T1$sigma)^2 +        
      (T1$`sd_ComID:UniqueFamID__Intercept`)^2 +        
      (T1$`sd_ComID__Intercept`)^2 +        
      (T1$sd_Interviewer__Intercept)^2 )

dens(icc_family_1, col=2, lwd=4, xlab="ICC (Family) of Model 1")


icc_community_1 <- ( (T1$`sd_ComID__Intercept`)^2 ) /    
  ( (T1$sigma)^2 +        
      (T1$`sd_ComID:UniqueFamID__Intercept`)^2 +        
      (T1$`sd_ComID__Intercept`)^2 +        
      (T1$sd_Interviewer__Intercept)^2 )

dens(icc_community_1, col=2, lwd=4, xlab="ICC (Community) of Model 1")


icc_interviewer_1 <- ( (T1$sd_Interviewer__Intercept)^2 ) /    
  ( (T1$sigma)^2 +        
      (T1$`sd_ComID:UniqueFamID__Intercept`)^2 +        
      (T1$`sd_ComID__Intercept`)^2 +        
      (T1$sd_Interviewer__Intercept)^2 )

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
  coord_cartesian(ylim = c(0, 25)) +
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
icc_residual_2 <- ( (T2$sigma)^2 ) / 
  ( (T2$sigma)^2 + 
      (T2$`sd_ComID:UniqueFamID__Intercept`)^2 + 
      (T2$`sd_ComID__Intercept`)^2 + 
      (T2$sd_Interviewer__Intercept)^2 )
dens(icc_residual_2, col=2, lwd=4, xlab="ICC (Residual) of Model 2")

icc_family_2 <- ( (T2$`sd_ComID:UniqueFamID__Intercept`)^2 ) / 
  ( (T2$sigma)^2 + 
      (T2$`sd_ComID:UniqueFamID__Intercept`)^2 + 
      (T2$`sd_ComID__Intercept`)^2 + 
      (T2$sd_Interviewer__Intercept)^2 )
dens(icc_family_2, col=2, lwd=4, xlab="ICC (Family) of Model 2")

icc_community_2 <- ( (T2$`sd_ComID__Intercept`)^2 ) / 
  ( (T2$sigma)^2 + 
      (T2$`sd_ComID:UniqueFamID__Intercept`)^2 + 
      (T2$`sd_ComID__Intercept`)^2 + 
      (T2$sd_Interviewer__Intercept)^2 )
dens(icc_community_2, col=2, lwd=4, xlab="ICC (Community) of Model 2")

icc_interviewer_2 <- ( (T2$sd_Interviewer__Intercept)^2 ) / 
  ( (T2$sigma)^2 + 
      (T2$`sd_ComID:UniqueFamID__Intercept`)^2 + 
      (T2$`sd_ComID__Intercept`)^2 + 
      (T2$sd_Interviewer__Intercept)^2 )
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
  coord_cartesian(ylim = c(0, 25)) +
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



########### Let's plot the effects of the models with PID & Region ####################################################################################
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
  rename("Intercept" = "b_Intercept",
         "Age" = "b_Age",
         "Male" = "b_Sex1",
         "SCI" = "b_SCI",
         "SFluency" = "b_SFluency"
  )

T2_v2 <- T2_v2 %>%
  rename("Intercept" = "b_Intercept",
         "Age" = "b_Age",
         "Male" = "b_Sex1",
         "SCI" = "b_SCI",
         "SFluency" = "b_SFluency",
         "Town_Distance" = "b_Town_Distance",
         "C_Size" = "b_Com_Size",
         "H_Size" = "b_HH_Size"
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
time_effect_data_mod1_v2 <- spline_plot_mod1_v2$Interview_Date

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
time_effect_data_mod2_v2 <- spline_plot_mod2_v2$Interview_Date

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

icc_residual_1_v2 <-  ((T1_v2$sigma)^2) / 
  ((T1_v2$sigma)^2 + 
     (T1_v2$sd_PID__Intercept)^2 + 
     (T1_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T1_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T1_v2$sd_Region__Intercept)^2 + 
     (T1_v2$sd_Interviewer__Intercept)^2)

dens(icc_residual_1_v2, col=2, lwd=4 , xlab="ICC (Residual) of Model 1")

icc_individual_1_v2 <-  ((T1_v2$sd_PID__Intercept)^2) / 
  ((T1_v2$sigma)^2 + 
     (T1_v2$sd_PID__Intercept)^2 + 
     (T1_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T1_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T1_v2$sd_Region__Intercept)^2 + 
     (T1_v2$sd_Interviewer__Intercept)^2)

dens(icc_individual_1_v2, col=2, lwd=4 , xlab="ICC (Individual) of Model 1")

icc_family_1_v2 <- ((T1_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2) / 
  ((T1_v2$sigma)^2 + 
     (T1_v2$sd_PID__Intercept)^2 + 
     (T1_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T1_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T1_v2$sd_Region__Intercept)^2 + 
     (T1_v2$sd_Interviewer__Intercept)^2)

dens(icc_family_1_v2, col=2, lwd=4 , xlab="ICC (Family) of Model 1")

icc_community_1_v2 <- ((T1_v2$`sd_Region:ComID__Intercept`)^2) / 
  ((T1_v2$sigma)^2 + 
     (T1_v2$sd_PID__Intercept)^2 + 
     (T1_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T1_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T1_v2$sd_Region__Intercept)^2 + 
     (T1_v2$sd_Interviewer__Intercept)^2)

dens(icc_community_1_v2, col=2, lwd=4 , xlab="ICC (Community) of Model 1")

icc_region_1_v2 <-  ((T1_v2$sd_Region__Intercept)^2) / 
  ((T1_v2$sigma)^2 + 
     (T1_v2$sd_PID__Intercept)^2 + 
     (T1_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T1_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T1_v2$sd_Region__Intercept)^2 + 
     (T1_v2$sd_Interviewer__Intercept)^2)

dens(icc_region_1_v2, col=2, lwd=4 , xlab="ICC (Region) of Model 1")

icc_interviewer_1_v2 <- ((T1_v2$sd_Interviewer__Intercept)^2) / 
  ((T1_v2$sigma)^2 + 
     (T1_v2$sd_PID__Intercept)^2 + 
     (T1_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T1_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T1_v2$sd_Region__Intercept)^2 + 
     (T1_v2$sd_Interviewer__Intercept)^2)

dens(icc_interviewer_1_v2, col=2, lwd=4 , xlab="ICC (Interviewer) of Model 1")

# Combine ICC vectors into a dataframe to allow for easy plotting of the five density plots in the same figure
icc_data_1_v2 <- data.frame(
  value = c(icc_residual_1_v2, icc_individual_1_v2, icc_family_1_v2, icc_community_1_v2, icc_region_1_v2, icc_interviewer_1_v2),
  group = rep(c("Residual", "Individual", "Household", "Community", "Region", "Interviewer"), each = length(icc_residual_1_v2))
)

# Convert group variable to factor with specified levels (important for color coding in the plots)
icc_data_1_v2$group <- factor(icc_data_1_v2$group, levels = c("Residual", "Individual", "Household", "Community", "Region", "Interviewer"))


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
  scale_color_manual(values = c("Residual" = "Green", "Individual" = "black", "Household" = "orange", "Community" = "red", "Region" = "blue", "Interviewer" = "darkcyan")) +
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

icc_residual_2_v2 <-  ((T2_v2$sigma)^2) / 
  ((T2_v2$sigma)^2 + 
     (T2_v2$sd_PID__Intercept)^2 + 
     (T2_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T2_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T2_v2$sd_Region__Intercept)^2 + 
     (T2_v2$sd_Interviewer__Intercept)^2)

dens(icc_residual_2_v2, col=2, lwd=4 , xlab="ICC (Residual) of Model 2")

icc_individual_2_v2 <-  ((T2_v2$sd_PID__Intercept)^2) / 
  ((T2_v2$sigma)^2 + 
     (T2_v2$sd_PID__Intercept)^2 + 
     (T2_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T2_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T2_v2$sd_Region__Intercept)^2 + 
     (T2_v2$sd_Interviewer__Intercept)^2)

dens(icc_individual_2_v2, col=2, lwd=4 , xlab="ICC (Individual) of Model 2")

icc_family_2_v2 <- ((T2_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2) / 
  ((T2_v2$sigma)^2 + 
     (T2_v2$sd_PID__Intercept)^2 + 
     (T2_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T2_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T2_v2$sd_Region__Intercept)^2 + 
     (T2_v2$sd_Interviewer__Intercept)^2)

dens(icc_family_2_v2, col=2, lwd=4 , xlab="ICC (Family) of Model 2")

icc_community_2_v2 <- ((T2_v2$`sd_Region:ComID__Intercept`)^2) / 
  ((T2_v2$sigma)^2 + 
     (T2_v2$sd_PID__Intercept)^2 + 
     (T2_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T2_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T2_v2$sd_Region__Intercept)^2 + 
     (T2_v2$sd_Interviewer__Intercept)^2)

dens(icc_community_2_v2, col=2, lwd=4 , xlab="ICC (Community) of Model 2")

icc_region_2_v2 <-  ((T2_v2$sd_Region__Intercept)^2) / 
  ((T2_v2$sigma)^2 + 
     (T2_v2$sd_PID__Intercept)^2 + 
     (T2_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T2_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T2_v2$sd_Region__Intercept)^2 + 
     (T2_v2$sd_Interviewer__Intercept)^2)

dens(icc_region_2_v2, col=2, lwd=4 , xlab="ICC (Region) of Model 2")

icc_interviewer_2_v2 <- ((T2_v2$sd_Interviewer__Intercept)^2) / 
  ((T2_v2$sigma)^2 + 
     (T2_v2$sd_PID__Intercept)^2 + 
     (T2_v2$`sd_Region:ComID:UniqueFamID__Intercept`)^2 + 
     (T2_v2$`sd_Region:ComID__Intercept`)^2 + 
     (T2_v2$sd_Region__Intercept)^2 + 
     (T2_v2$sd_Interviewer__Intercept)^2)

dens(icc_interviewer_2_v2, col=2, lwd=4 , xlab="ICC (Interviewer) of Model 2")


# Combine ICC vectors into a dataframe to allow for easy plotting of the five density plots in the same figure
icc_data_2_v2 <- data.frame(
  value = c(icc_residual_2_v2, icc_individual_2_v2, icc_family_2_v2, icc_community_2_v2, icc_region_2_v2, icc_interviewer_2_v2),
  group = rep(c("Residual", "Individual", "Household", "Community", "Region", "Interviewer"), each = length(icc_residual_2_v2))
)

# Convert group variable to factor with specified levels (important for color coding in the plots)
icc_data_2_v2$group <- factor(icc_data_2_v2$group, levels = c("Residual", "Individual", "Household", "Community", "Region", "Interviewer"))

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
  scale_color_manual(values = c("Residual" = "Green", "Individual" = "black", "Household" = "orange", "Community" = "red", "Region" = "blue", "Interviewer" = "darkcyan")) +
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







