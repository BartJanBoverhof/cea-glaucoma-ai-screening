#------------------------------------------------------------------------------#
####                       01 Prerequisites                           ####
#------------------------------------------------------------------------------#
rm(list = ls()) # to clean the workspace
set.seed(3791) # set seed for reproducibility

# loading packages
p_load_gh("DARTH-git/darthtools")
if (!require('pacman')) install.packages('pacman'); library(pacman) # use this package to conveniently install other packages
p_load("dplyr", "tidyr", "reshape2", "devtools", "scales", "ellipse", "ggplot2", "ggrepel", "gridExtra", "lazyeval", "igraph", "truncnorm", "ggraph", "reshape2", "patchwork", "knitr", "stringr", "diagram", "dampack","DiagrammeR") # load (install if required) packages from CRAN
p_load_gh("DARTH-git/darthtools") # load (install if required) packages from GitHub

# importing Functions
source("R/1_model_pipeline_functions.R", echo = TRUE) #Load cohort model input functions
source("R/2_decision_tree_functions.R", echo = TRUE) #Load decision model functions
source("R/3_markov_model_functions.R", echo = TRUE) #Load decision model functions
#source("R/4_costs_and_utilities_functions.R", echo = TRUE) #Load utility functions
source("R/5_visualisation_functions.R", echo = TRUE) #Load visualization functions


# loading all required data objects
load("data/1_df_mortality.RData")
load("data/2_p_dt.RData")
load("data/3_p_severity_undiagnosed.RData")
load("data/4a_p_transition_ai.RData")
load("data/4b_p_transition_soc.RData")
load("data/5_v_utilities.RData")
load("data/6_v_incidences.RData")


#------------------------------------------------------------------------------#
####                       ## Obtain Cohort                            ####
#------------------------------------------------------------------------------#
# obtain cohorts
t_total_cohort <- getCohort(df_mortality, age_categories = c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years")) # step 1: get cohort


#------------------------------------------------------------------------------#
####                       03 Markov Model                            ####
#------------------------------------------------------------------------------#
df_incidence_clean <- calculateIncidence(v_incidences, start_age = 50) # calculate incidence
df_mortality_clean <- calculateMortality(df_mortality, start_age = 50) # calculate all cause mortality mortality
n_mean_age <- getMeanAge(df_mortality, start_age = 50) # get mean age of cohort

# get dt probabilities
p_dt_ai <- getDtProbabilities(p_dt, p_severity_undiagnosed, "Ai Screening", visualize = T) # step 2: get dt probabilities

# re-scale to cohort of 1000 patients
v_cohort_1000 <- lapply(p_dt_ai[-length(p_dt_ai)], function(x) x*1000)

# markov trace
a_trace_ai <- getMarkovTrace(strategy = "AI",  # run markov model for AI
                             cohort = v_cohort_1000,
                             df_mortality = df_mortality_clean, 
                             p_transition =  p_transition_ai, 
                             age_init = round(n_mean_age),
                             age_max = 100,
                             incidences = df_incidence_clean
                             ) 



## temp ##
strategy = "AI"  # run markov model for AI
cohort = v_cohort_1000
df_mortality = df_mortality_clean
p_transition =  p_transition_ai
age_init = round(n_mean_age)
age_max = 100
incidences = df_incidence_clean



# 07 State Rewards 

## Scale by the cycle length 
# Vector of state utilities under strategy SoC
v_u_ai    <- c(Healthy  = u_healthy, 
               Mild = u_mild, 
               Moderate = u_moderate, 
               Severe  = u_severe,
               Blind = u_blind,
               #Observation = u_observation,
               Death = u_death) * cycle_length
# Vector of state costs under strategy SoC
v_c_ai    <- c(Healthy  = c_healthy, 
               Mild = c_mild, 
               Moderate = c_moderate, 
               Severe  = c_severe,
               Blind = c_blind,
               #Observation = u_observation,
               Death = c_death) * cycle_length

v_u_soc <- v_u_ai
v_c_soc <- v_c_ai

#<UTILITIES AND COSTS SOC>
## Store state rewards 
# Store the vectors of state utilities for each strategy in a list 
l_u <- list(ai  = v_u_ai,
            soc = v_u_soc)
# Store the vectors of state cost for each strategy in a list 
l_c <- list(ai  = v_c_ai,
            soc = v_c_soc)

# assign strategy names to matching items in the lists
names(l_u) <- names(l_c) <- v_names_str


# 08 Compute expected outcomes 
# Create empty vectors to store total utilities and costs 
v_tot_qaly <- v_tot_cost <- vector(mode = "numeric", length = n_str)
names(v_tot_qaly) <- names(v_tot_cost) <- v_names_str

## Loop through each strategy and calculate total utilities and costs 
for (i in 1:n_str) { # i <- 1
  
  v_u_str <- l_u[[i]]   # select the vector of state utilities for the i-th strategy
  v_c_str <- l_c[[i]]   # select the vector of state costs for the i-th strategy
  
  ### Expected QALYs and costs per cycle 
  ## Vector of QALYs and Costs
  # Apply state rewards 
  v_qaly_str <- l_m_M[[i]] %*% v_u_str # sum the utilities of all states for each cycle
  v_cost_str <- l_m_M[[i]] %*% v_c_str # sum the costs of all states for each cycle
  
  ### Discounted total expected QALYs and Costs per strategy and apply within-cycle correction if applicable
  # QALYs
  v_tot_qaly[i] <- t(v_qaly_str) %*% (v_dwe * v_wcc)
  # Costs
  v_tot_cost[i] <- t(v_cost_str) %*% (v_dwc * v_wcc)
  
}


#------------------------------------------------------------------------------#
####                       04 Visualization         ####
#------------------------------------------------------------------------------#
# plot trace output
plot_trace(a_trace_ai[,-ncol(a_trace_ai)])



#------------------------------------------------------------------------------#
####                       05 Cost-effectiveness analysis (CEA)         ####
#------------------------------------------------------------------------------#
## Incremental cost-effectiveness ratios (ICERs) 
df_cea <- calculate_icers(cost       = v_tot_cost, 
                          effect     = v_tot_qaly,
                          strategies = v_names_str)
df_cea

## CEA table in proper format 
table_cea <- format_table_cea(df_cea)
table_cea


## CEA frontier 
plot(df_cea, label = "all", txtsize = 16) +
  expand_limits(x = max(table_cea$QALYs) + 0.1) +
  theme(legend.position = c(0.82, 0.3))




