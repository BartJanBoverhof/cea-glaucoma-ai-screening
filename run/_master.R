#------------------------------------------------------------------------------#
####                       01 Prerequisites                           ####
#------------------------------------------------------------------------------#
rm(list = ls()) # to clean the workspace
set.seed(3791) # set seed for reproducibility

# loading packages
#p_load_gh("DARTH-git/darthtools")
if (!require('pacman')) install.packages('pacman'); library(pacman) # use this package to conveniently install other packages
p_load("dplyr", "tidyr", "reshape2", "devtools", "scales", "ellipse", "ggplot2", "ggrepel", "gridExtra", "lazyeval", "igraph", "truncnorm", "ggraph", "reshape2", "patchwork", "knitr", "stringr", "diagram", "dampack","DiagrammeR") # load (install if required) packages from CRAN
p_load_gh("DARTH-git/darthtools") # load (install if required) packages from GitHub

# importing Functions
source("R/1_model_pipeline_functions.R", echo = TRUE) #Load cohort model input functions
source("R/2_decision_tree_functions.R", echo = TRUE) #Load decision model functions
source("R/3_markov_model_functions.R", echo = TRUE) #Load decision model functions
source("R/4_costs_and_utilities_functions.R", echo = TRUE) #Load utility functions
source("R/5_visualisation_functions.R", echo = TRUE) #Load visualization functions


# loading all required data objects
load("data/1_df_mortality.RData")
load("data/2_p_dt.RData")
load("data/3a_p_severity_undiagnosed.RData")
load("data/3b_p_severity_diagnosed.RData")
load("data/3c_p_severity_low_risk.RData")
load("data/4_p_transition.RData")
load("data/5_v_utilities.RData")
load("data/6_v_incidences.RData")
load("data/7_v_cost_dt.RData")

# re-saving all parameters for PSA
df_mortality <- df_mortality
p_dt <- p_dt
p_severity_undiagnosed <- p_severity_undiagnosed
p_severity_diagnosed <- p_severity_diagnosed
p_severity_low_risk <- p_severity_low_risk
p_transition <- p_transition
v_utilities <- v_utilities
v_incidences <- v_incidences
v_utilities <- v_utilities
start_age <- 50
age_max <- 100
n_cycle_length <- 1
utility_decrement <- 0.02


#------------------------------------------------------------------------------#
####                       0 Obtain Cohort                            ####
#------------------------------------------------------------------------------#
t_total_cohort <- getCohort(df_mortality, age_categories = c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years")) # obtain cohorts
 

#------------------------------------------------------------------------------#
####                       1 Decision Tree                            ####
#------------------------------------------------------------------------------#
df_incidence_clean <- calculateIncidence(v_incidences, start_age = start_age) # calculate incidence
df_mortality_clean <- calculateMortality(df_mortality, start_age = start_age) # calculate all cause mortality mortality
n_mean_age <- getMeanAge(df_mortality, start_age = start_age) # get mean age of cohort

### AI strategy
# obtain starting severity distributions - AI strategy
p_dt_ai_soc <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = "soc", visualize = F) # soc arm
p_dt_ai_low_risk <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_low_risk, strategy = "low_risk", visualize = F) # low risk arm
p_dt_ai_high_risk <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = "high_risk", visualize = F) # high risk arm
p_dt_ai_compliant <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = "compliant", visualize = F) # compliant arm

# check if DT sums to 1
validateDT(p_dt_ai_soc, p_dt_ai_low_risk, p_dt_ai_high_risk, p_dt_ai_compliant)

### SoC strategy
# obtain starting severity distributions - SoC strategy
p_dt_soc <- getStartDistSoc(probabilities = p_dt, severity_distribution = p_severity_diagnosed, visualize = F) 

#------------------------------------------------------------------------------#
####                       2 Markov model                            ####
#------------------------------------------------------------------------------#
# re-scale to cohort of 1000 patients
### AI strategy
v_cohort_ai_soc <- lapply(p_dt_ai_soc, function(x) x*1000)
v_cohort_ai_low_risk <- lapply(p_dt_ai_low_risk, function(x) x*1000)
v_cohort_ai_high_risk <- lapply(p_dt_ai_high_risk, function(x) x*1000)
v_cohort_ai_compliant <- lapply(p_dt_ai_compliant, function(x) x*1000)

### SoC strategy
v_cohort_soc <- lapply(p_dt_soc, function(x) x*1000)


# markov traces
### AI Strategy
# AI strategy - non-compliant with screening (soc)
a_trace_ai_soc<- getMarkovTrace(strategy = "soc",  
                                cohort = v_cohort_ai_soc,
                                df_mortality = df_mortality_clean, 
                                p_transition =  p_transition , 
                                age_init = round(n_mean_age),
                                age_max = age_max,
                                incidences = df_incidence_clean)                               

# AI strategy - non-compliant with referral (low risk)
a_trace_ai_low_risk<- getMarkovTrace(strategy = "low_risk",  
                                     cohort = v_cohort_ai_low_risk,
                                     df_mortality = df_mortality_clean, 
                                     p_transition =  p_transition, 
                                     age_init = round(n_mean_age),
                                     age_max = age_max,
                                     incidences = df_incidence_clean)
                                     
# AI strategy - positive AI result but non-compliant with follow-up (high risk)
a_trace_ai_high_risk<- getMarkovTrace(strategy = "high_risk",  
                                      cohort = v_cohort_ai_high_risk,
                                      df_mortality = df_mortality_clean, 
                                      p_transition =  p_transition, 
                                      age_init = round(n_mean_age),
                                      age_max = age_max,
                                      incidences = df_incidence_clean)
                                                                       
# AI strategy - compliant with screening and referral (compliant)
a_trace_ai_compliant<- getMarkovTrace(strategy = "compliant",  
                                      cohort = v_cohort_ai_compliant,
                                      df_mortality = df_mortality_clean, 
                                      p_transition =  p_transition, 
                                      age_init = round(n_mean_age),
                                      age_max = age_max,
                                      incidences = df_incidence_clean)
# combined AI strategy trace
a_trace_ai <- a_trace_ai_soc + a_trace_ai_low_risk + a_trace_ai_high_risk + a_trace_ai_compliant

### SoC strategy
# standard of care  
a_trace_soc <- getMarkovTrace(strategy = "soc",  
                              cohort = v_cohort_soc,
                              df_mortality = df_mortality_clean, 
                              p_transition =  p_transition, 
                              age_init = round(n_mean_age),
                              age_max = age_max,
                              incidences = df_incidence_clean)

#------------------------------------------------------------------------------#
####                       3 Utilities                           ####
#------------------------------------------------------------------------------#
# AI strategy
qalys_ai <- getQALYs(a_trace = a_trace_ai, 
                     v_utilities = v_utilities, 
                     decrement = utility_decrement, # annual utility decrement
                     n_cycle_length = n_cycle_length) # obtain utilities

#SoC strategy
qalys_soc <- getQALYs(a_trace = a_trace_soc, 
                      v_utilities = v_utilities, 
                      decrement = utility_decrement, 
                      n_cycle_length = n_cycle_length) # obtain utilities

# QALYS ZITTEN HEEL ERG DICHT BIJ ELKAAR. KOMT WAARSCHIJNLIJK DOOR GELIJKE UTILITIES IN BEIDE ARMEN. MAAR CHECK EERST OF DIT KLOPT. 

#------------------------------------------------------------------------------#
####                       4 Costs                           ####
#------------------------------------------------------------------------------#
# decision tree screening costs
getScreeningCosts(a_trace_ai_soc = a_trace_ai_soc, # cohort trace of the patients non-compliant with AI screening
                  a_trace_ai_low_risk = a_trace_ai_low_risk, # cohort trace of the patients with negaive AI result
                  a_trace_ai_high_risk = a_trace_ai_high_risk, # cohort trace of the patients non-compliant with referral 
                  a_trace_ai_compliant = a_trace_ai_compliant, # cohort trace of the patients compliant with screening and referral
                  screening_cost = v_cost_dt) # obtain screening costs

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




