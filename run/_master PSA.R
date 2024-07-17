#------------------------------------------------------------------------------#
####                       0 Prerequisites                           ####
#------------------------------------------------------------------------------#
rm(list = ls()) # to clean the workspace
set.seed(3791) # set seed for reproducibility

# loading packages
#p_load_gh("DARTH-git/darthtools")
if (!require('pacman')) install.packages('pacman'); library(pacman) # use this package to conveniently install other packages
p_load("dplyr", "tidyr", "reshape2", "devtools", "scales", "ellipse", "ggplot2", "ggrepel", "gridExtra", "lazyeval", "igraph", "truncnorm", "ggraph", "reshape2", "patchwork", "knitr", "stringr", "diagram", "dampack","DiagrammeR") # load (install if required) packages from CRAN
p_load_gh("DARTH-git/darthtools") # load (install if required) packages from GitHub
library(stringr)

# importing Functions
source("R/0_run.R", echo = TRUE) #Load general functions
source("R/1_model_pipeline_functions.R", echo = TRUE) #Load cohort model input functions
source("R/2_decision_tree_functions.R", echo = TRUE) #Load decision model functions
source("R/3_markov_model_functions.R", echo = TRUE) #Load decision model functions
source("R/4_costs_and_utilities_functions.R", echo = TRUE) #Load utility functions
source("R/5_sensitivity_and_scenario.R", echo = TRUE) #Load visualization functions
source("R/6_visualisation_functions.R", echo = TRUE) #Load visualization functions

# loading all required data objects
load("data/1_df_mortality.RData")
load("data/2_p_dt.RData")
load("data/3a_p_severity_undiagnosed.RData")
load("data/4_p_transition.RData")
load("data/5a_v_utilities.RData")
load("data/5b_v_utilities_gp.RData")
load("data/6a_v_incidences_of.RData")
load("data/6b_v_incidences_screening.RData")
load("data/6c_v_prevalence.RData")
load("data/7a_df_utilisation_medicine.RData")
load("data/8a_v_cost_dt.RData")
load("data/8b_v_cost_medicine.RData")
load("data/8c_v_cost_utilisation_diagnostics.RData")
load("data/8d_v_cost_utilisation_intervention.RData")
load("data/8f_v_cost_burden_disease.RData")
load("data/9b_v_psa_se.RData")

#------------------------------------------------------------------------------#
####                       1 Define Cohorts                            ####
#------------------------------------------------------------------------------#
age_categories <- c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years") # age categories to be screened
t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)

#------------------------------------------------------------------------------#
####                       2 Run model                           ####
#------------------------------------------------------------------------------#
# varying parameters
p_dt <- p_dt
p_severity_undiagnosed <- p_severity_undiagnosed
p_transition <- p_transition
v_utilities <- v_utilities
v_utilities_gp<- v_utilities_gp
v_psa_se <- v_psa_se
screening_interval <- 5
discount <- TRUE
pension_age <- 67 

# run base case
strategy <- "psa"

# probabalistic sensitivity analysis
out <- runPSA(n_sample = 1000)
saveRDS(out, file = "results_psa.rds")

library(scales)       


# open psa file
out <- readRDS("results_psa2.rds")

results_df <- data.frame(Simulation = integer(), Incremental_QALY = numeric(), Incremental_Costs = numeric())

# Loop through the `out` list to populate the dataframe
for (i in 1:length(out)) {
  # Extract incremental QALYs and Costs for each simulation
  incremental_qaly <- out[[i]][[1]]
  incremental_costs <- out[[i]][[2]]
  
  # Append this information to the dataframe
  results_df <- rbind(results_df, data.frame(Simulation = i, Incremental_QALY = incremental_qaly, Incremental_Costs = incremental_costs))
}

PSA_icer <- mean(results_df$Incremental_Costs / results_df$Incremental_QALY)

# Assuming 'results_df' contains the data with columns 'Incremental_Costs' and 'Incremental_QALY'
# and you want to differentiate points by some identifier, for example 'Scenario'

# Custom euro formatter
euro_formatter <- function(x) {
  paste0("€", format(x, big.mark = ",", scientific = FALSE))
}

# Calculate the percentage of cost-effective samples
cost_effective_samples <- sum(results_df$Incremental_Costs / results_df$Incremental_QALY <= 20000) / nrow(results_df) * 100

# Enhanced Cost-Effectiveness Plane
ce_plane <- ggplot(results_df, aes(x = Incremental_Costs, y = Incremental_QALY)) +
  geom_point(size = 2, alpha = 0.5) +  # Add points with size and transparency
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +  # Add a horizontal line at y=0
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +  # Add a vertical line at x=0
  geom_abline(slope = 1/20000, intercept = 0, linetype = "solid", color = "#03217d", size = 1) +  # Add ICER line
  labs(
    title = "",
    x = "Incremental Costs",
    y = "Incremental QALYs",
    shape = "Scenario",
    fill = "Scenario"
  ) +
  #scale_color_manual(values = setNames(results_df$Color, results_df$Identifier)) +
  #scale_fill_manual(values = setNames(results_df$Color, results_df$Identifier)) +
  #scale_shape_manual(values = setNames(as.numeric(results_df$Shape), results_df$Identifier)) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",  # Place legend on the right
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(size = 17),
    axis.text.x = element_text(size = 17),
    panel.grid.major = element_line(color = "#adadad"),  
    panel.grid.minor = element_line(color = "#adadad"),
    panel.background = element_rect(fill = "lightgray")
  ) +
  scale_x_continuous(labels = euro_formatter) +
  annotate("label", x = Inf, y = -Inf, label = paste("Cost-effective:", round(cost_effective_samples, 2), "%"), 
           hjust = 1.2, vjust = -0.6, size = 5, color = "White", fill = "#585757", fontface = "bold") +
  annotate("label", x = -200, y = -0.01, label = "WTP Treshold at €20,000", 
           size = 5, color = "White", fill = "#012591", fontface = "bold") 
           




ggsave("figures/ce_plane.png", plot = ce_plane, width = 9, height = 7)









library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

# Define WTP range
results_df <- results_df %>% mutate(ICER = Incremental_Costs / Incremental_QALY)

wtp_values <- seq(0, 100000, length.out = 10000)
df_ceac <- data.frame(wtp_values, percentage_ce = NA)

for (i in 1:nrow(df_ceac)){
    df_ceac$percentage_ce[i] <- mean(ifelse(results_df$ICER < df_ceac$wtp_values[i], 1, 0))  
}

# Plot the cost acceptability curve
ceac <- ggplot(df_ceac, aes(x = wtp_values, y = percentage_ce)) +
  geom_line(size = 1.5, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  labs(
    title = "",
    x = "Willingness-to-Pay (WTP) Threshold",
    y = "Percentage Cost-Effective"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",  # Place legend on the right
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(size = 17),
    axis.text.x = element_text(size = 17),
    panel.grid.major = element_line(color = "#adadad"),  
    panel.grid.minor = element_line(color = "#adadad"),
    panel.background = element_rect(fill = "lightgray")
  ) +
  scale_x_continuous(breaks = seq(0, 100000, by = 20000), minor_breaks = seq(0, 100000, by = 10000),labels = euro_formatter) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2), labels = scales::percent) +
  geom_vline(xintercept = 20000, linetype = "solid", color = "#012591", size = 1) +  # Add a vertical line at x=0
  annotate("label", x = 20000, y = 0.85, label = "WTP Treshold at €20,000", 
           size = 5, color = "White", fill = "#012591", fontface = "bold") 

ggsave("figures/ceac.png", plot = ceac, width = 9, height = 7)

