rm(list = ls()) # to clean the workspace

# loading packages
require(tidyverse)
require(DiagrammeR)

# importing Functions
source("R/1_model_pipeline_functions.R", echo = TRUE) #Load cohort model input functions
source("R/2_decision_tree_functions.R", echo = TRUE) #Load decision model functions
source("R/3_markov_model_functions.R", echo = TRUE) #Load decision model functions

# loading all required data objects
load("data/1_all_cause_mortality.RData")
load("data/2_dt_ai.RData")
load("data/3_severity_undiagnosed.RData")

# obtain cohorts
total_cohort <- get_cohort(all_cause_mortality) # step 1: get cohort
dt <- get_dt_probabilities(dt_ai, severity_undiagnosed, "Ai Screening", visualize = T) # step 2: get dt probabilities
cohort <- get_cohort_arm(total_cohort, dt) # step 3: obtain cohort per decision-tree-arm

# calculate markov trace
# preliminaries
cycle_length <- 1

markov_trace <- function(cohort, cycle_length, )

