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
library(stringr)

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
load("data/5a_v_utilities.RData")
load("data/5b_v_utilities_age_decrement.RData")
load("data/6_v_incidences.RData")
load("data/7a_df_utilisation_medicine.RData")
load("data/8a_v_cost_dt.RData")
load("data/8b_v_cost_medicine.RData")
load("data/8c_v_cost_utilisation_diagnostics.RData")
load("data/8d_v_cost_utilisation_intervention.RData")
load("data/8e_v_cost_visually_impaired.RData")
load("data/8f_v_cost_blind.RData")

# re-saving all parameters for PSA
df_mortality <- df_mortality
p_dt <- p_dt
p_severity_undiagnosed <- p_severity_undiagnosed
p_severity_diagnosed <- p_severity_diagnosed
p_severity_low_risk <- p_severity_low_risk
p_transition <- p_transition
v_utilities <- v_utilities
v_utilities_age_decrement <- v_utilities_age_decrement
v_incidences <- v_incidences
v_utilities <- v_utilities
age_start <- 50 # start age of the cohort
age_max <- 100 # maximum age through which to model
age_lower <- 50 # initial age of the cohort
age_upper <- 75 # upper age of the cohort
n_cycle_length <- 1
utility_decrement <- 0.02


#------------------------------------------------------------------------------#
####                       0 Obtain Cohort                            ####
#------------------------------------------------------------------------------#
#t_total_cohort <- getCohort(df_mortality, age_categories = c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years")) # obtain cohorts
 

#------------------------------------------------------------------------------#
####                       1 Decision Tree                            ####
#------------------------------------------------------------------------------#
df_incidence_clean <- calculateIncidence(v_incidences, age_start = age_start, age_max = age_max) # calculate incidence
df_mortality_clean <- calculateMortality(df_mortality, age_start = age_start, age_max = age_max) # calculate all cause mortality
n_mean_age <- getMeanAge(df_mortality, age_start = age_lower, age_max = age_upper) # get mean age of cohort

### AI strategy
# obtain starting severity distributions - AI strategy
p_dt_ai_soc <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = "soc", visualize = F, model_compliance = TRUE) # soc arm
p_dt_ai_low_risk <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_low_risk, strategy = "low_risk", visualize = F, model_compliance = TRUE) # low risk arm
p_dt_ai_high_risk <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = "high_risk", visualize = F, model_compliance = TRUE) # high risk arm
p_dt_ai_compliant <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = "compliant", visualize = F, model_compliance = TRUE) # compliant arm

# combine the DT into one single starting distribution
p_dt_ai <- CombineDT(p_dt_ai_soc, p_dt_ai_low_risk, p_dt_ai_high_risk, p_dt_ai_compliant)

# probability of patients to be fully compliant (for later use)
p_screening_dr <- getScreeningDetectionRate(probabilities = p_dt, model_compliance = TRUE)

### SoC strategy
# obtain starting severity distributions - SoC strategy
p_dt_soc <- getStartDistSoc(probabilities = p_dt, severity_distribution = p_severity_diagnosed, visualize = F) 

#------------------------------------------------------------------------------#
####                       2 Markov model                            ####
#------------------------------------------------------------------------------#
# re-scale to cohort of 1000 patients
### AI strategy
v_cohort_ai <- lapply(p_dt_ai, function(x) x*1000)

### SoC strategy
v_cohort_soc <- lapply(p_dt_soc, function(x) x*1000)

# markov traces
### AI Strategy
a_trace_ai<- getMarkovTrace(scenario = "ai",
                            cohort = v_cohort_ai,
                            screening_detection_rate = p_screening_dr, 
                            df_mortality = df_mortality_clean, 
                            p_transition =  p_transition , 
                            age_init = 50,
                            incidences = df_incidence_clean,
                            interval = 5, 
                            max_repititions = 5)                            

### SoC strategy
# standard of care  
a_trace_soc <- getMarkovTrace(scenario = "soc",
                            cohort = v_cohort_ai,
                            screening_detection_rate = NULL, 
                            df_mortality = df_mortality_clean, 
                            p_transition =  p_transition , 
                            age_init = 50,
                            incidences = df_incidence_clean,
                            interval = 0, 
                            max_repititions = 0)                            


#------------------------------------------------------------------------------#
####                       3 Utilities                           ####
#------------------------------------------------------------------------------#
# AI strategy
qalys_ai <- getQALYs(a_trace = a_trace_ai, 
                     v_utilities = v_utilities, 
                     age_decrement = v_utilities_age_decrement, # annual utility decrement
                     n_cycle_length = n_cycle_length,
                     discount_rate = 0.015,
                     age_init = round(n_mean_age),
                     age_max = age_max) # obtain utilities
#SoC strategy
qalys_soc <- getQALYs(a_trace = a_trace_soc, 
                      v_utilities = v_utilities, 
                      age_decrement = v_utilities_age_decrement, 
                      n_cycle_length = n_cycle_length,
                      discount_rate =0.015,
                     age_init = round(n_mean_age),
                     age_max = age_max) # obtain utilities

#------------------------------------------------------------------------------#
####                   4a Costs decision tree screening costs                    ####
#------------------------------------------------------------------------------#
ai_screening_costs <-   getScreeningCosts(a_trace_ai_soc = a_trace_ai_soc, # patients non-compliant with AI screening
                                          a_trace_ai_low_risk = a_trace_ai_low_risk, # patients with negaive AI result
                                          a_trace_ai_high_risk = a_trace_ai_high_risk, # patients non-compliant with referral 
                                          a_trace_ai_compliant = a_trace_ai_compliant, # patients compliant with screening and referral
                                          screening_cost = v_cost_dt) # obtain screening costs

#------------------------------------------------------------------------------#
####                   4b Costs medicine                    ####
#------------------------------------------------------------------------------
## ai scenario
ai_medicine_costs <- getMedicineCosts(a_trace = a_trace_ai, # cohort trace of the patients non-compliant with AI screening
                                      medicine_cost = v_cost_medicine,
                                      medicine_utilisation = df_utilisation_medicine) # obtain medicine costs

# soc scenario
soc_medicine_costs <- getMedicineCosts(a_trace = a_trace_soc, # cohort trace of the patients non-compliant with AI screening
                                        medicine_cost = v_cost_medicine,
                                        medicine_utilisation = df_utilisation_medicine) # obtain medicine costs

#------------------------------------------------------------------------------#
####                   4c Costs diagnostics                    ####
#------------------------------------------------------------------------------
ai_diagnostic_costs <- getDiagnosticCosts(trace = a_trace_ai, # cohort trace of the patients non-compliant with AI screening
                                          diagnostics_cost = v_cost_utilisation_diagnostics) # obtain diagnostic costs

soc_diagnostic_costs <- getDiagnosticCosts(trace = a_trace_soc, # cohort trace of the patients non-compliant with AI screening
                                           diagnostics_cost = v_cost_utilisation_diagnostics) # obtain diagnostic costs

#------------------------------------------------------------------------------#
####                   4d Costs intervention                    ####
#------------------------------------------------------------------------------
ai_intervention_costs <- getInterventionCosts(trace = a_trace_ai, # cohort trace of the patients non-compliant with AI screening
                                             intervention_cost = v_cost_utilisation_intervention) # obtain intervention costs

soc_intervention_costs <- getInterventionCosts(trace = a_trace_soc, # cohort trace of the patients non-compliant with AI screening
                                               intervention_cost = v_cost_utilisation_intervention) # obtain intervention costs

#------------------------------------------------------------------------------#
####                 4e Costs burden of disease visually impaired & blind   ####
#------------------------------------------------------------------------------
## ai scenario
ai_visually_impaired <- getVisuallyImpairedCosts(costs = v_cost_visually_impaired, trace = a_trace_ai)
ai_blind <- getBlindCosts(costs = v_cost_visually_impaired, trace = a_trace_ai)

#soc scenario
soc_visually_impaired  <- getVisuallyImpairedCosts(costs = v_cost_visually_impaired, trace = a_trace_soc)
soc_blind <- getBlindCosts(costs = v_cost_visually_impaired, trace = a_trace_soc)

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




