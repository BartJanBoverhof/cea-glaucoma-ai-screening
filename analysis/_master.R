rm(list = ls()) # to clean the workspace

# Loading packages
require(tidyverse)
require(DiagrammeR)

# Importing Functions
source("R/1_model_input_functions.R", echo = TRUE) #Load cohort model input functions
source("R/2_decision_model_functions.R", echo = TRUE) #Load decision model functions

# Loading data
load("data/1_all_cause_mortality.RData")
load("data/2_dt_ai.RData")
load("data/3_severity_undiagnosed.RData")

#1 

#2 Obtain decision tree probabilites
get_dt_probabilities(dt_ai, severity_undiagnosed, "Ai Screening", visualize = T) 


