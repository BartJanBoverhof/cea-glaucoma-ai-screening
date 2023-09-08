rm(list = ls()) # to clean the workspace

# loading packages
require(tidyverse)
require(DiagrammeR)

# Importing Functions
source("R/1_model_pipeline_functions.R", echo = TRUE) #Load cohort model input functions
source("R/2_decision_tree_functions.R", echo = TRUE) #Load decision model functions


# loading data
load("data/1_all_cause_mortality.RData")
load("data/2_dt_ai.RData")
load("data/3_severity_undiagnosed.RData")

# get cohort
total_cohort <- get_cohort(all_cause_mortality)

# get dt probabilities
dt <- get_dt_probabilities(dt_ai, severity_undiagnosed, "Ai Screening", visualize = T) 

'''
# calculate cohorts
get_severity <- function(arm = c("ai","soc")){
  
  
}
  
mild <- dt$path_mild * total_cohort
moderate <- dt$path_moderate * total_cohort
severe <- dt$path_severe * total_cohort
blind <- dt$path_blind * total_cohort
'''

  
  


