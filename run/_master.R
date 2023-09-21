rm(list = ls()) # to clean the workspace

# loading packages
p_load_gh("DARTH-git/darthtools")
if (!require('pacman')) install.packages('pacman'); library(pacman) # use this package to conveniently install other packages
p_load("dplyr", "tidyr", "reshape2", "devtools", "scales", "ellipse", "ggplot2", "ggrepel", "gridExtra", "lazyeval", "igraph", "truncnorm", "ggraph", "reshape2", "patchwork", "knitr", "stringr", "diagram", "dampack","DiagrammeR") # load (install if required) packages from CRAN
p_load_gh("DARTH-git/darthtools") # load (install if required) packages from GitHub

# importing Functions
source("R/1_model_pipeline_functions.R", echo = TRUE) #Load cohort model input functions
source("R/2_decision_tree_functions.R", echo = TRUE) #Load decision model functions
#source("R/3_markov_model_functions.R", echo = TRUE) #Load decision model functions

# loading all required data objects
load("data/1_all_cause_mortality.RData")
load("data/2_p_dt_ai.RData")
load("data/3_p_severity_undiagnosed.RData")
load("data/4a_p_transition_ai.RData")

# obtain cohorts
t_total_cohort <- get_cohort(all_cause_mortality, age_categories = c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years")) # step 1: get cohort
p_dt_ai <- get_dt_probabilities(p_dt_ai, p_severity_undiagnosed, "Ai Screening", visualize = T) # step 2: get dt probabilities
v_cohort_ai <- get_cohort_arm(t_total_cohort, p_dt_ai) # step 3: obtain cohort per decision-tree-arm

