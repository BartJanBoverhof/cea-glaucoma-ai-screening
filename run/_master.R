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

# non-fixed paramaters (PSA)
p_dt <- p_dt # Isaac: for all these probabilities we need to provide an uncertainty range. These will be sampled in the PSA separately using rbeta(...)
p_severity_undiagnosed <- p_severity_undiagnosed # Isaac: 'same' as p_dt but since this is a vector of linked probabilities we'll use rdirichlet(...)
p_severity_diagnosed <- p_severity_diagnosed # Isaac: same as undiagnosed
p_severity_low_risk <- p_severity_low_risk
p_transition <- p_transition # Isaac: we need to check this one carefully to avoid illogical values when considering all transitions together
v_utilities <- v_utilities # Isaac: unclear why healthy and obs get a value = 1. Maybe this is corrected later in the code, but it should be equivalent to the general population utility. # Then each utility will be sampled independently from a beta distribution: we may need to check for illogical values here too. 
v_utilities_age_decrement <- v_utilities_age_decrement


#------------------------------------------------------------------------------#
####                       0 Obtain Cohort                            ####
#------------------------------------------------------------------------------#
age_categories <- c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years") # modelled age categories
t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) # obtain cohorts
r_male_female <- getMaleFemaleRatio(df_mortality = df_mortality)

#------------------------------------------------------------------------------#
####                       1 Decision Tree                            ####
#------------------------------------------------------------------------------#
df_incidence_clean <- calculateIncidence(v_incidences, age_start = 50, age_max = 100) # calculate incidence
df_mortality_clean <- calculateMortality(df_mortality, age_start = 50, age_max = 100) # calculate all cause mortality

### AI strategy
p_dt_ai_soc <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = "soc", visualize = F, model_compliance = TRUE) # soc arm
p_dt_ai_low_risk <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_low_risk, strategy = "low_risk", visualize = F, model_compliance = TRUE) # low risk arm
p_dt_ai_high_risk <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = "high_risk", visualize = F, model_compliance = TRUE) # high risk arm
p_dt_ai_compliant <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = "compliant", visualize = F, model_compliance = TRUE) # compliant arm

p_dt_ai <- CombineDT(p_dt_ai_soc, p_dt_ai_low_risk, p_dt_ai_high_risk, p_dt_ai_compliant) # combine all arms
p_screening_dr <- getScreeningDetectionRate(probabilities = p_dt, model_compliance = TRUE) # obtain screening detection rate

# scale cohorts
v_cohort_ai_50_55 <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort["50 to 55 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_ai_55_60 <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort["55 to 60 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_ai_60_65 <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort["60 to 65 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_ai_65_70 <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort["65 to 70 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_ai_70_75 <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort["70 to 75 years"]) * 1000)) # re-scale to cohort of 1000 patients
sum(unlist(v_cohort_ai_50_55)) + sum(unlist(v_cohort_ai_55_60)) + sum(unlist(v_cohort_ai_60_65)) + sum(unlist(v_cohort_ai_65_70)) + sum(unlist(v_cohort_ai_70_75)) # check

### SoC strategy
p_dt_soc <- getStartDistSoc(probabilities = p_dt, severity_distribution = p_severity_diagnosed, visualize = F) # obtain severity distribution 

# scale cohorts
v_cohort_soc_50_55 <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort["50 to 55 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_soc_55_60 <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort["55 to 60 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_soc_60_65 <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort["60 to 65 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_soc_65_70 <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort["65 to 70 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_soc_70_75 <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort["70 to 75 years"]) * 1000)) # re-scale to cohort of 1000 patients
sum(unlist(v_cohort_soc_50_55)) + sum(unlist(v_cohort_soc_55_60)) + sum(unlist(v_cohort_soc_60_65)) + sum(unlist(v_cohort_soc_65_70)) + sum(unlist(v_cohort_soc_70_75)) # check 

#------------------------------------------------------------------------------#
####                       2 Markov model                            ####
#------------------------------------------------------------------------------#
### AI Strategy
a_trace_ai_5055 <- getMarkovTrace(scenario = "ai",
                                  cohort = v_cohort_ai_50_55,
                                  screening_detection_rate = p_screening_dr, 
                                  df_mortality = df_mortality_clean, 
                                  p_transition =  p_transition , 
                                  age_init = 52,
                                  incidences = df_incidence_clean,
                                  interval = 5, 
                                  max_repititions = 4)  

patients_ai_5055 <- sum(a_trace_ai_5055[1,]) # number of patients in the subcohort
a_trace_ai_5055_utility <- traceCorrectionUtil(a_trace_ai_5055, v_utilities_age_decrement, age_init = 52) # trace corrected for utilities (discount & age)

a_trace_ai_5560 <- getMarkovTrace(scenario = "ai",
                                  cohort = v_cohort_ai_55_60,
                                  screening_detection_rate = p_screening_dr, 
                                  df_mortality = df_mortality_clean, 
                                  p_transition =  p_transition , 
                                  age_init = 57,
                                  incidences = df_incidence_clean,
                                  interval = 5, 
                                  max_repititions = 3)

patients_ai_5560 <- sum(a_trace_ai_5560[1,]) # number of patients in the subcohort
a_trace_ai_5560_utility <- traceCorrectionUtil(a_trace_ai_5560, v_utilities_age_decrement, age_init = 57) # trace corrected for utilities (discount & age)                                                  

a_trace_ai_6065 <- getMarkovTrace(scenario = "ai",
                                  cohort = v_cohort_ai_60_65,
                                  screening_detection_rate = p_screening_dr, 
                                  df_mortality = df_mortality_clean, 
                                  p_transition =  p_transition , 
                                  age_init = 62,
                                  incidences = df_incidence_clean,
                                  interval = 5, 
                                  max_repititions = 2)

patients_ai_6065 <- sum(a_trace_ai_6065[1,]) # number of patients in the subcohort
a_trace_ai_6065_utility <- traceCorrectionUtil(a_trace_ai_6065, v_utilities_age_decrement, age_init = 62) # trace corrected for utilities (discount & age)


a_trace_ai_6570 <- getMarkovTrace(scenario = "ai",
                                  cohort = v_cohort_ai_65_70,
                                  screening_detection_rate = p_screening_dr, 
                                  df_mortality = df_mortality_clean, 
                                  p_transition =  p_transition , 
                                  age_init = 67,
                                  incidences = df_incidence_clean,
                                  interval = 5, 
                                  max_repititions = 1)

patients_ai_6570 <- sum(a_trace_ai_6570[1,]) # number of patients in the subcohort
a_trace_ai_6570_utility <- traceCorrectionUtil(a_trace_ai_6570, v_utilities_age_decrement, age_init = 67) # trace corrected for utilities (discount & age)

a_trace_ai_7075 <- getMarkovTrace(scenario = "ai",  
                                  cohort = v_cohort_ai_70_75,
                                  screening_detection_rate = p_screening_dr, 
                                  df_mortality = df_mortality_clean, 
                                  p_transition =  p_transition , 
                                  age_init = 72,
                                  incidences = df_incidence_clean,
                                  interval = 5, 
                                  max_repititions = 0)           

patients_ai_7075 <- sum(a_trace_ai_7075[1,]) # number of patients in the subcohort
a_trace_ai_7075_utility <- traceCorrectionUtil(a_trace_ai_7075, v_utilities_age_decrement, age_init = 72) # trace corrected for utilities (discount & age)

# create summed traces
a_trace_ai_uncorrected <- a_trace_ai_5055 + # uncorrected trace (for reference)
  padArray(pad = a_trace_ai_5560, pad_to = a_trace_ai_5055) + 
  padArray(pad = a_trace_ai_6065, pad_to = a_trace_ai_5055) + 
  padArray(pad = a_trace_ai_6570, pad_to = a_trace_ai_5055) + 
  padArray(pad = a_trace_ai_7075, pad_to = a_trace_ai_5055) 

a_trace_ai_utillity <- a_trace_ai_5055_utility + # corrected trace
  padArray(pad = a_trace_ai_5560_utility, pad_to = a_trace_ai_5055_utility) + 
  padArray(pad = a_trace_ai_6065_utility, pad_to = a_trace_ai_5055_utility) + 
  padArray(pad = a_trace_ai_6570_utility, pad_to = a_trace_ai_5055_utility) + 
  padArray(pad = a_trace_ai_7075_utility, pad_to = a_trace_ai_5055_utility)

### SoC strategy
a_trace_soc_5055 <- getMarkovTrace(scenario = "soc",
                                   cohort = v_cohort_soc_50_55,
                                   screening_detection_rate = 0, 
                                   df_mortality = df_mortality_clean, 
                                   p_transition =  p_transition , 
                                   age_init = 52,
                                   incidences = df_incidence_clean,
                                   interval = 0, 
                                   max_repititions = 0)                         

patients_soc_5055 <- sum(a_trace_soc_5055[1,]) # number of patients in the subcohort
a_trace_soc_5055_utility <- traceCorrectionUtil(a_trace_soc_5055, v_utilities_age_decrement, age_init = 52) # trace corrected for utilities (discount & age)

a_trace_soc_5560 <- getMarkovTrace(scenario = "soc",
                                    cohort = v_cohort_soc_55_60,
                                    screening_detection_rate = 0, 
                                    df_mortality = df_mortality_clean, 
                                    p_transition =  p_transition , 
                                    age_init = 57,
                                    incidences = df_incidence_clean,
                                    interval = 0, 
                                    max_repititions = 0)

patients_soc_5560 <- sum(a_trace_soc_5560[1,]) # number of patients in the subcohort
a_trace_soc_5560_utility <- traceCorrectionUtil(a_trace_soc_5560, v_utilities_age_decrement, age_init = 57) # trace corrected for utilities (discount & age)

a_trace_soc_6065 <- getMarkovTrace(scenario = "soc",
                                    cohort = v_cohort_soc_60_65,
                                    screening_detection_rate = 0, 
                                    df_mortality = df_mortality_clean, 
                                    p_transition =  p_transition , 
                                    age_init = 62,
                                    incidences = df_incidence_clean,
                                    interval = 0, 
                                    max_repititions = 0)

patients_soc_6065 <- sum(a_trace_soc_6065[1,]) # number of patients in the subcohort
a_trace_soc_6065_utility <- traceCorrectionUtil(a_trace_soc_6065, v_utilities_age_decrement, age_init = 62) # trace corrected for utilities (discount & age)

a_trace_soc_6570 <- getMarkovTrace(scenario = "soc",
                                    cohort = v_cohort_soc_65_70,
                                    screening_detection_rate = 0, 
                                    df_mortality = df_mortality_clean, 
                                    p_transition =  p_transition , 
                                    age_init = 67,
                                    incidences = df_incidence_clean,
                                    interval = 0, 
                                    max_repititions = 0)

patients_soc_6570 <- sum(a_trace_soc_6570[1,]) # number of patients in the subcohort
a_trace_soc_6570_utility <- traceCorrectionUtil(a_trace_soc_6570, v_utilities_age_decrement, age_init = 67) # trace corrected for utilities (discount & age)

a_trace_soc_7075 <- getMarkovTrace(scenario = "soc", 
                                    cohort = v_cohort_soc_70_75,
                                    screening_detection_rate = 0, 
                                    df_mortality = df_mortality_clean, 
                                    p_transition =  p_transition , 
                                    age_init = 72,
                                    incidences = df_incidence_clean,
                                    interval = 0, 
                                    max_repititions = 0)                                  

patients_soc_7075 <- sum(a_trace_soc_7075[1,]) # number of patients in the subcohort
a_trace_soc_7075_utility <- traceCorrectionUtil(a_trace_soc_7075, v_utilities_age_decrement, age_init = 72) # trace corrected for utilities (discount & age)

# create summed trace
a_trace_soc_uncorrected <- a_trace_soc_5055 + # uncorrected trace (for reference)
  padArray(pad = a_trace_soc_5560, pad_to = a_trace_soc_5055) + 
  padArray(pad = a_trace_soc_6065, pad_to = a_trace_soc_5055) + 
  padArray(pad = a_trace_soc_6570, pad_to = a_trace_soc_5055) + 
  padArray(pad = a_trace_soc_7075, pad_to = a_trace_soc_5055)

a_trace_soc_utillity <- a_trace_soc_5055_utility + # corrected trace
  padArray(pad = a_trace_soc_5560_utility, pad_to = a_trace_soc_5055_utility) + 
  padArray(pad = a_trace_soc_6065_utility, pad_to = a_trace_soc_5055_utility) + 
  padArray(pad = a_trace_soc_6570_utility, pad_to = a_trace_soc_5055_utility) + 
  padArray(pad = a_trace_soc_7075_utility, pad_to = a_trace_soc_5055_utility)


#------------------------------------------------------------------------------#
####                       3 Utilities                           ####
#------------------------------------------------------------------------------#
sum(a_trace_ai_5055[1,]) + sum(a_trace_ai_5560[1,]) + sum(a_trace_ai_6065[1,]) + sum(a_trace_ai_6570[1,]) + sum(a_trace_ai_7075[1,]) # check whether the traces sum up to 1000
sum(a_trace_soc_5055[1,]) + sum(a_trace_soc_5560[1,]) + sum(a_trace_soc_6065[1,]) + sum(a_trace_soc_6570[1,]) + sum(a_trace_soc_7075[1,]) # check whether the traces sum up to 1000

# AI strategy
qalys_ai <- getQALYs(a_trace = a_trace_ai, 
                     v_utilities = v_utilities, 
                     n_cycle_length = 1,
                     age_init = 52) 

#SoC strategy
qalys_soc <- getQALYs(a_trace = a_trace_soc, 
                      v_utilities = v_utilities, 
                      n_cycle_length = 1,
                      age_init = 52,
                      age_max = 100) # obtain utilities

#------------------------------------------------------------------------------#
####                   4a Costs decision tree screening costs                    ####
#------------------------------------------------------------------------------#
ai_screening_costs <- getScreeningCosts(a_trace_ai_soc = a_trace_ai_soc, # patients non-compliant with AI screening
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




