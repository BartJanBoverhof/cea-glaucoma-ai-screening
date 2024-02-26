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
load("data/4_p_transition.RData")
load("data/5a_v_utilities.RData")
load("data/5b_v_utilities_age_decrement.RData")
load("data/6a_v_incidences_of.RData")
load("data/6b_v_incidences_screening.RData")
load("data/6c_v_prevalence.RData")
load("data/7a_df_utilisation_medicine.RData")
load("data/8a_v_cost_dt.RData")
load("data/8b_v_cost_medicine.RData")
load("data/8c_v_cost_utilisation_diagnostics.RData")
load("data/8d_v_cost_utilisation_intervention.RData")
load("data/8f_v_cost_burden_disease.RData")

# non-fixed paramaters (PSA)
p_dt <- p_dt # Isaac: for all these probabilities we need to provide an uncertainty range. These will be sampled in the PSA separately using rbeta(...)
p_severity_undiagnosed <- p_severity_undiagnosed # Isaac: 'same' as p_dt but since this is a vector of linked probabilities we'll use rdirichlet(...)
p_transition <- p_transition # Isaac: we need to check this one carefully to avoid illogical values when considering all transitions together
v_utilities <- v_utilities # Isaac: unclear why healthy and obs get a value = 1. Maybe this is corrected later in the code, but it should be equivalent to the general population utility. # Then each utility will be sampled independently from a beta distribution: we may need to check for illogical values here too. 
v_utilities_age_decrement <- v_utilities_age_decrement


#------------------------------------------------------------------------------#
####                       0 Obtain Cohort                            ####
#------------------------------------------------------------------------------#
age_categories <- c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years") # modelled age categories
t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)
r_male_female <- getMaleFemaleRatio(df_mortality = df_mortality) ### (function returns proportion of patients that are male)

#------------------------------------------------------------------------------#
####                       1 Decision Tree                            ####
#------------------------------------------------------------------------------#
v_incidence_of <- calculateIncidence(incidences = v_incidences_of, age_start = 50, age_max = 100) ### (function returns incidence per age year)
v_incidence_screening <- calculateIncidence(incidences = v_incidences_screening, age_start = 50, age_max = 100) ### (function returns incidence per age year)
incidences <- list(v_incidence_of = v_incidence_of, v_incidence_screening = v_incidence_screening)
v_mortality <- calculateMortality(df_mortality = df_mortality, age_start = 50, age_max = 100) ### (function returns mortality per age year)

################## AI STRATEGY
### functions return probability of patients in each health state, seperately for each arm of the decision tree

#
strategies <- c("soc", "low_risk", "high_risk", "compliant")
p_dt_ai <- list()

for (strategy in strategies) {
  p_dt_ai[[strategy]] <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = strategy, visualize = F, model_compliance = FALSE)
}

p_dt_ai <- CombineDT(traces = p_dt_ai) ### function combines all arms of the decision tree into single starting distribution per health state


p_screening <- getScreeningProbabilities(probabilities = p_dt, model_compliance = FALSE) ### function returns list of probabilities related to each screening arm (for later use)

v_cohort_ai_50_55 <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort["50 to 55 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_ai_55_60 <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort["55 to 60 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_ai_60_65 <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort["60 to 65 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_ai_65_70 <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort["65 to 70 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_ai_70_75 <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort["70 to 75 years"]) * 1000)) # re-scale to cohort of 1000 patients
sum(unlist(v_cohort_ai_50_55)) + sum(unlist(v_cohort_ai_55_60)) + sum(unlist(v_cohort_ai_60_65)) + sum(unlist(v_cohort_ai_65_70)) + sum(unlist(v_cohort_ai_70_75)) # check if sum of all subcohorts is 1000

################## SOC STRATEGY
p_dt_soc <- getStartDistSoc(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, visualize = F) # obtain severity distribution 

v_cohort_soc_50_55 <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort["50 to 55 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_soc_55_60 <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort["55 to 60 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_soc_60_65 <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort["60 to 65 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_soc_65_70 <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort["65 to 70 years"]) * 1000)) # re-scale to cohort of 1000 patients
v_cohort_soc_70_75 <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort["70 to 75 years"]) * 1000)) # re-scale to cohort of 1000 patients
sum(unlist(v_cohort_soc_50_55)) + sum(unlist(v_cohort_soc_55_60)) + sum(unlist(v_cohort_soc_60_65)) + sum(unlist(v_cohort_soc_65_70)) + sum(unlist(v_cohort_soc_70_75)) # check if sum of all subcohorts is 1000

#------------------------------------------------------------------------------#
####                       2 Markov model                            ####
#------------------------------------------------------------------------------#
# prerequisites
age_inits <- c(52, 57, 62, 67, 72) # initial age for each age category

################## AI STRATEGY
a_trace_ai_5055 <- getMarkovTrace(scenario = "ai", ### (function returns list of (corrected) markov traces for the age category 50-55 years)
                                  cohort = v_cohort_ai_50_55,
                                  screening_detection_rate = p_screening$p_fully_compliant, 
                                  df_mortality = v_mortality, 
                                  p_transition =  p_transition , 
                                  age_init = age_inits[1],
                                  incidences = incidences,
                                  interval = 5, 
                                  max_repititions = 4)  

a_trace_ai_5560 <- getMarkovTrace(scenario = "ai", ### (function returns list of (corrected) markov traces for the age category 55-60 years)
                                  cohort = v_cohort_ai_55_60,
                                  screening_detection_rate = p_screening$p_fully_compliant, 
                                  df_mortality = v_mortality, 
                                  p_transition =  p_transition , 
                                  age_init = age_inits[2],
                                  incidences = incidences,
                                  interval = 5, 
                                  max_repititions = 3)

a_trace_ai_6065 <- getMarkovTrace(scenario = "ai", ### (function returns list of (corrected) markov traces for the age category 60-65 years)
                                  cohort = v_cohort_ai_60_65,
                                  screening_detection_rate = p_screening$p_fully_compliant, 
                                  df_mortality = v_mortality, 
                                  p_transition =  p_transition , 
                                  age_init = age_inits[3],
                                  incidences = incidences,
                                  interval = 5, 
                                  max_repititions = 2)

a_trace_ai_6570 <- getMarkovTrace(scenario = "ai", ### (function returns list of (corrected) markov traces for the age category 65-70 years)
                                  cohort = v_cohort_ai_65_70,
                                  screening_detection_rate = p_screening$p_fully_compliant, 
                                  df_mortality = v_mortality, 
                                  p_transition =  p_transition , 
                                  age_init = age_inits[4],
                                  incidences = incidences,
                                  interval = 5, 
                                  max_repititions = 1)

a_trace_ai_7075 <- getMarkovTrace(scenario = "ai", ### (function returns list of (corrected) markov traces for the age category 70-75 years) 
                                  cohort = v_cohort_ai_70_75,
                                  screening_detection_rate = p_screening$p_fully_compliant, 
                                  df_mortality = v_mortality, 
                                  p_transition =  p_transition , 
                                  age_init = age_inits[5],
                                  incidences = incidences,
                                  interval = 5, 
                                  max_repititions = 0)           
# create summed traces
a_trace_ai_uncorrected <- a_trace_ai_5055$trace + # uncorrected trace (for reference)
  padArray(pad = a_trace_ai_5560$trace, pad_to = a_trace_ai_5055$trace) + 
  padArray(pad = a_trace_ai_6065$trace, pad_to = a_trace_ai_5055$trace) + 
  padArray(pad = a_trace_ai_6570$trace, pad_to = a_trace_ai_5055$trace) + 
  padArray(pad = a_trace_ai_7075$trace, pad_to = a_trace_ai_5055$trace) 

a_trace_ai_utillity <- a_trace_ai_5055$trace_utility + # corrected trace (for utility)
  padArray(pad = a_trace_ai_5560$trace_utility, pad_to = a_trace_ai_5055$trace_utility) + 
  padArray(pad = a_trace_ai_6065$trace_utility, pad_to = a_trace_ai_5055$trace_utility) + 
  padArray(pad = a_trace_ai_6570$trace_utility, pad_to = a_trace_ai_5055$trace_utility) + 
  padArray(pad = a_trace_ai_7075$trace_utility, pad_to = a_trace_ai_5055$trace_utility)

a_trace_ai_cost <- a_trace_ai_5055$trace_cost + # corrected trace (for costs)
  padArray(pad = a_trace_ai_5560$trace_cost, pad_to = a_trace_ai_5055$trace_cost) +
  padArray(pad = a_trace_ai_6065$trace_cost, pad_to = a_trace_ai_5055$trace_cost) +
  padArray(pad = a_trace_ai_6570$trace_cost, pad_to = a_trace_ai_5055$trace_cost) +
  padArray(pad = a_trace_ai_7075$trace_cost, pad_to = a_trace_ai_5055$trace_cost)

# create list with all traces
list_traces_ai_costs <- list(a_trace_5055 = a_trace_ai_5055$trace_cost, a_trace_5560 = a_trace_ai_5560$trace_cost, a_trace_6065 = a_trace_ai_6065$trace_cost, a_trace_6570 = a_trace_ai_6570$trace_cost, a_trace_7075 = a_trace_ai_7075$trace_cost)

################## SOC STRATEGY
a_trace_soc_5055 <- getMarkovTrace(scenario = "soc", ### (function returns list of (corrected) markov traces for the age category 50-55 years)
                                   cohort = v_cohort_soc_50_55,
                                   screening_detection_rate = 0, 
                                   df_mortality = v_mortality, 
                                   p_transition =  p_transition , 
                                   age_init = age_inits[1],
                                   incidences = incidences,
                                   interval = 0, 
                                   max_repititions = 0)                         

a_trace_soc_5560 <- getMarkovTrace(scenario = "soc", ### (function returns list of (corrected) markov traces for the age category 55-60 years)
                                    cohort = v_cohort_soc_55_60,
                                    screening_detection_rate = 0, 
                                    df_mortality = v_mortality, 
                                    p_transition =  p_transition , 
                                    age_init = age_inits[2],
                                    incidences = incidences,
                                    interval = 0, 
                                    max_repititions = 0)

a_trace_soc_6065 <- getMarkovTrace(scenario = "soc", ### (function returns list of (corrected) markov traces for the age category 60-65 years)
                                    cohort = v_cohort_soc_60_65,
                                    screening_detection_rate = 0, 
                                    df_mortality = v_mortality, 
                                    p_transition =  p_transition , 
                                    age_init = age_inits[3],
                                    incidences = incidences,
                                    interval = 0, 
                                    max_repititions = 0)

a_trace_soc_6570 <- getMarkovTrace(scenario = "soc", ### (function returns list of (corrected) markov traces for the age category 65-70 years)
                                    cohort = v_cohort_soc_65_70,
                                    screening_detection_rate = 0, 
                                    df_mortality = v_mortality, 
                                    p_transition =  p_transition , 
                                    age_init = age_inits[4],
                                    incidences = incidences,
                                    interval = 0, 
                                    max_repititions = 0)

a_trace_soc_7075 <- getMarkovTrace(scenario = "soc", ### (function returns list of (corrected) markov traces for the age category 70-75 years)
                                    cohort = v_cohort_soc_70_75,
                                    screening_detection_rate = 0, 
                                    df_mortality = v_mortality, 
                                    p_transition =  p_transition , 
                                    age_init = age_inits[5],
                                    incidences = incidences,
                                    interval = 0, 
                                    max_repititions = 0)                                  

# create summed trace
a_trace_soc_uncorrected <- a_trace_soc_5055$trace + ### uncorrected trace (for reference)
  padArray(pad = a_trace_soc_5560$trace, pad_to = a_trace_soc_5055$trace) + 
  padArray(pad = a_trace_soc_6065$trace, pad_to = a_trace_soc_5055$trace) + 
  padArray(pad = a_trace_soc_6570$trace, pad_to = a_trace_soc_5055$trace) + 
  padArray(pad = a_trace_soc_7075$trace, pad_to = a_trace_soc_5055$trace)

a_trace_soc_utillity <- a_trace_soc_5055$trace_utility + ### corrected trace (for utility)
  padArray(pad = a_trace_soc_5560$trace_utility, pad_to = a_trace_soc_5055$trace_utility) + 
  padArray(pad = a_trace_soc_6065$trace_utility, pad_to = a_trace_soc_5055$trace_utility) + 
  padArray(pad = a_trace_soc_6570$trace_utility, pad_to = a_trace_soc_5055$trace_utility) + 
  padArray(pad = a_trace_soc_7075$trace_utility, pad_to = a_trace_soc_5055$trace_utility)

a_trace_soc_cost <- a_trace_soc_5055$trace_cost + ### corrected trace (for costs)
  padArray(pad = a_trace_soc_5560$trace_cost, pad_to = a_trace_soc_5055$trace_cost) +
  padArray(pad = a_trace_soc_6065$trace_cost, pad_to = a_trace_soc_5055$trace_cost) +
  padArray(pad = a_trace_soc_6570$trace_cost, pad_to = a_trace_soc_5055$trace_cost) +
  padArray(pad = a_trace_soc_7075$trace_cost, pad_to = a_trace_soc_5055$trace_cost)

# create list with all traces
list_traces_soc_costs <- list(a_trace_5055 = a_trace_soc_5055$trace_cost, a_trace_5560 = a_trace_soc_5560$trace_cost, a_trace_6065 = a_trace_soc_6065$trace_cost, a_trace_6570 = a_trace_soc_6570$trace_cost, a_trace_7075 = a_trace_soc_7075$trace_cost)

sum(a_trace_ai_5055$trace[1,]) + sum(a_trace_ai_5560$trace[1,]) + sum(a_trace_ai_6065$trace[1,]) + sum(a_trace_ai_6570$trace[1,]) + sum(a_trace_ai_7075$trace[1,]) # check whether the traces sum up to 1000
sum(a_trace_soc_5055$trace[1,]) + sum(a_trace_soc_5560$trace[1,]) + sum(a_trace_soc_6065$trace[1,]) + sum(a_trace_soc_6570$trace[1,]) + sum(a_trace_soc_7075$trace[1,]) # check whether the traces sum up to 1000

################################################################
########################### RESTULTS ###########################
################################################################
ai_time_spent <- getTimeSpent(a_trace = a_trace_ai_uncorrected) ### (function returns time spent in each health state))
soc_time_spent <- getTimeSpent(a_trace = a_trace_soc_uncorrected) ### (function returns time spent in each health state))
blindness_prevented <- getBlindnessPrevented(a_trace_ai = a_trace_ai_uncorrected, a_trace_soc = a_trace_soc_uncorrected) ### (function returns blindness prevented)
ai_screening_descriptives <- getScreenignDescriptives(trace = a_trace_ai_uncorrected, ### function returns screening costs per screening repition
                                                     screening_probabilities = p_screening,
                                                     screening_cost = v_cost_dt, # obtain screening costs
                                                     interval = 5, 
                                                     max_repititions = 4,
                                                     total_cohort = t_total_cohort) # screening repition reflects the amount of repitions IN ADDITION to the screening before the markov model

#------------------------------------------------------------------------------#
####                       3 Utilities                           ####
#------------------------------------------------------------------------------#
ai_total_qaly <- getQALYs(a_trace = a_trace_ai_utillity, v_utilities = v_utilities) ### (function returns average QALY per patient)
soc_total_qaly <- getQALYs(a_trace = a_trace_soc_utillity, v_utilities = v_utilities) ### (returns average QALY per patient)

#------------------------------------------------------------------------------#
####                   4a Costs decision tree screening costs                    ####
#------------------------------------------------------------------------------#
ai_screening_costs <- getScreeningCosts(trace = a_trace_ai_cost, ### function returns screening costs per screening repition
                                        screening_probabilities = p_screening,
                                        screening_cost = v_cost_dt, # obtain screening costs
                                        interval = 5, 
                                        max_repititions = 4,
                                        total_cohort = t_total_cohort) # screening repition reflects the amount of repitions IN ADDITION to the screening before the markov model 


#------------------------------------------------------------------------------#
####                   4b Costs medicine                    ####
#------------------------------------------------------------------------------
ai_medicine_costs <- getMedicineCosts(a_trace = a_trace_ai_cost, ### function returns the total medicine costs
                                      medicine_cost = v_cost_medicine,
                                      medicine_utilisation = df_utilisation_medicine) # obtain medicine costs

soc_medicine_costs <- getMedicineCosts(a_trace = a_trace_soc_cost, # cohort trace of the patients non-compliant with AI screening
                                        medicine_cost = v_cost_medicine,
                                        medicine_utilisation = df_utilisation_medicine) # obtain medicine costs

#------------------------------------------------------------------------------#
####                   4c Costs diagnostics                    ####
#------------------------------------------------------------------------------
ai_diagnostic_costs <- getDiagnosticCosts(trace = a_trace_ai_cost, # cohort trace of the patients non-compliant with AI screening
                                          diagnostics_cost = v_cost_utilisation_diagnostics) # obtain diagnostic costs

soc_diagnostic_costs <- getDiagnosticCosts(trace = a_trace_soc_cost, # cohort trace of the patients non-compliant with AI screening
                                           diagnostics_cost = v_cost_utilisation_diagnostics) # obtain diagnostic costs

#------------------------------------------------------------------------------#
####                   4d Costs intervention                    ####
#------------------------------------------------------------------------------
ai_intervention_costs <- getInterventionCosts(trace = a_trace_ai_cost, # cohort trace of the patients non-compliant with AI screening
                                             intervention_cost = v_cost_utilisation_intervention) # obtain intervention costs

soc_intervention_costs <- getInterventionCosts(trace = a_trace_soc_cost, # cohort trace of the patients non-compliant with AI screening
                                               intervention_cost = v_cost_utilisation_intervention) # obtain intervention costs

#------------------------------------------------------------------------------#
####                 4e Costs burden of disease visually impaired & blind   ####
#------------------------------------------------------------------------------
ai_burden <- getCostsBurdenOfDisease(costs = v_cost_burden_disease, trace = a_trace_ai_cost, societal_perspective = TRUE)
ai_productivity <- getProductivityCosts(costs = v_cost_burden_disease, traces = list_traces_ai_costs, age_inits = age_inits)

#soc scenario
soc_burden <- getCostsBurdenOfDisease(costs = v_cost_burden_disease, trace = a_trace_soc_cost, societal_perspective = TRUE)
soc_productivity <- getProductivityCosts(costs = v_cost_burden_disease, traces = list_traces_soc_costs, age_inits = age_inits)

#------------------------------------------------------------------------------#
####                         4f Total costs                                 ####
#------------------------------------------------------------------------------#
ai_total_costs <- ai_screening_costs$cost_pp + ai_medicine_costs$cost_pp + ai_diagnostic_costs$cost_pp + ai_intervention_costs$price_pp + ai_burden$price_pp + ai_productivity$price_pp
soc_total_costs <- soc_medicine_costs$cost_pp + soc_diagnostic_costs$cost_pp + soc_intervention_costs$price_pp + soc_burden$price_pp + soc_productivity$price_pp

#------------------------------------------------------------------------------#
####                       04 Visualization         ####
#------------------------------------------------------------------------------#
# plot trace output
n_cycles <- nrow(a_trace_ai_uncorrected) -1
v_names_states <- colnames(a_trace_ai_uncorrected)
plot_trace(a_trace_ai_uncorrected)
plot_trace(a_trace_soc_uncorrected)

#------------------------------------------------------------------------------#
####                       05 Cost-effectiveness analysis (CEA)         ####
#------------------------------------------------------------------------------#
## Incremental cost-effectiveness ratios (ICERs) 
icer <- (ai_total_costs - soc_total_costs) / (ai_total_qaly - soc_total_qaly)





