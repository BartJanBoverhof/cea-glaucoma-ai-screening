#------------------------------------------------------------------------------#
####                       0 Prerequisites                           ####
#------------------------------------------------------------------------------#
rm(list = ls()) # to clean the workspace
set.seed(3791) # set seed for reproducibility

# loading packages
#p_load_gh("DARTH-git/darthtools")
if (!require('pacman')) install.packages('pacman'); library(pacman) # use this package to conveniently install other packages
p_load("dplyr", "tidyr", "reshape2", "devtools", "scales", "ellipse", "ggplot2", "ggrepel", "gridExtra", "lazyeval", "igraph", "truncnorm", "ggraph", "reshape2", "patchwork", "knitr", "stringr", "diagram", "dampack","DiagrammeR","brms") # load (install if required) packages from CRAN
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
load("data/9_sensitivity_se.RData")

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
v_psa_se <- v_sensitivity_se
screening_interval <- 5
discount <- TRUE
pension_age <- 67 

# run base case
strategy <- "psa"

# probabalistic sensitivity analysis
valid_trans <<- list()
valid_array <<- list()

#out <- runPSA(n_sample = 10000, perspective = "healthcare")
#saveRDS(out, file = "data/results_psa_hcp.rds")
out <- readRDS("data/results_psa_hcp.rds")

# ICER and PSA plot
qaly_costs_list <- lapply(out, function(x) x$qaly_costs) # extract list for the qaly and costs (PSA plot & ICER)

#PSA plot
psaPlot(out = qaly_costs_list)

# calculate all costs
costs_list <- lapply(out, function(x) x$costs)

# call the function with the costs_list
# calcualte PSA ICER
ICER <- calculateICER(out)
cost_stats <- costStats(costs_list)
icer_stats <- icerVIStats(out)
qaly_stats <- QALYStats(out)





