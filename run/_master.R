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
# Isaac: as far as I remember, the average age of the patient population was around 65 years old, right?
# If that's the case, the numbers provided in t_total_cohort do not match with that average age, since these proportions suggest 
# that the most frequent age is 50-55 followed by 55-60. The proportions decrease, so I cannot see how the average age of ~65 can be 
# obtained with these numbers.
r_male_female <- getMaleFemaleRatio(df_mortality = df_mortality) ### (function returns proportion of patients that are male) 

# Isaac: I understand why you prefer to run age-dependent cohorts separately. 
# However, this approach has several issues associated as discussed.
# I wonder for example if the proportion of patients that are male should also be age-dependent. In fact, the same could be said about 
# all parameters in the model, such as p_dt, p_severity_undiagnosed, p_transition or even the utilities. 
# For the utilities, I know we consider age in terms of age-related decrement. However, the values at baseline were obtained 
# for a cohort of a certain age. It could also be argued that these baseline values could also be age-dependent.
# The reason I insist on this issue is because I believe it might be criticised by some reviewers. It's totally fine to have
# different age cohorts, but then each cohort parameters' should be cohort specific (in theory) and I wonder whether that's
# actually the case. Clear justification has to be provided for the assumptins made. 

#------------------------------------------------------------------------------#
####                       1 Decision Tree                            ####
#------------------------------------------------------------------------------#
v_incidence_of <- calculateIncidence(incidences = v_incidences_of, age_start = 50, age_max = 100) ### (function returns incidence per age year)
v_incidence_screening <- calculateIncidence(incidences = v_incidences_screening, age_start = 50, age_max = 100) ### (function returns incidence per age year)
incidences <- list(v_incidence_of = v_incidence_of, v_incidence_screening = v_incidence_screening)
v_mortality <- calculateMortality(df_mortality = df_mortality, age_start = 50, age_max = 100) ### (function returns mortality per age year)

################## AI STRATEGY
### functions return probability of patients in each health state, seperately for each arm of the decision tree
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

# Isaac:  same as above: what we do here is to re-scale the cohorts based on the age distribution, but I'm not sure if that's correct.
# I would think that the older patients the higher the probability of being severe, but right now the opposite happens. 
# I think this happens for two reasons: 1. p_dt_soc is not age dependent and 2. t_total_cohort seems incorrect. 

#------------------------------------------------------------------------------#
####                       2 Markov model                            ####
#------------------------------------------------------------------------------#
# prerequisites
age_inits <- c(52, 57, 62, 67, 72) # initial age for each age category 
# Isaac: I understand you have chosen the middle point of each category, right? I think it's OK to start with 50 (not in the middle).

################## AI STRATEGY
#Isaac: we need to explain the input parameters of these functions (low priority right now)
# We should also add what the function is returning. I see the traces (LYs, costs and utilities and total patients)
# Question here: are these results discounted? I cannot see the option of discounting by the way and I think this was implemented in aprevious version of the code.
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

# Isaac: why is this called uncorrected? Have you checked that all rows sum to 1000 or that does not need to happen?
# Also, unclear why we need to sum all these when traces will be of different length for each cohort.
# The same applies for the two objects below.
# For a_trace_ai_utillity, please check that a_trace_ai_utillity < a_trace_ai_uncorrected, and that they are 
# equal if all utilities = 1.

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

# create list with all traces #Isaac: what is the purpose of this object and why it contains only costs?
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
# Isaac: these two functions are based on the objects above where you combined traces of different lengths.
# As discussed before, I'm not sure why that step is needed. To validate the results, it'd be easier to calculate
# total QALYs for each cohort separately and then take the weighted average.
# It could also be a good exercise to check whether these calculations are correct, since they should be equal I suppose.
# I've also noticed that changing age_init does not change the results of ai_total_qaly or soc_total_qaly. Is that correct?
                             
ai_total_qaly <- getQALYs(a_trace = a_trace_ai_utillity, v_utilities = v_utilities) ### (function returns average QALY per patient)
soc_total_qaly <- getQALYs(a_trace = a_trace_soc_utillity, v_utilities = v_utilities) ### (returns average QALY per patient)


#------------------------------------------------------------------------------#
####                   4a Costs decision tree screening costs                    ####
#------------------------------------------------------------------------------#

# Isaac: I also don't understand this one since this should be different for each cohort. So the question is why the max is set to 4.
# Again, why don't we get results per age cohort separately and then calculate the weighted average?
# Is the cost of one screening sum(v_cost_dt)? This is equal to 188€. If ai_screening_costs = 557€, that means that on average there 
# are 3 AI screenings per patient per lifetime. Do we consider that a valid result? My point is that we need to link that with the 
# average age of the patient population. If this is 67 years, then 3 AI screenings do not seem correct. However, in the model now
# I suppose the average age should be a bit above 60 years then. We need to consider whether that's valid or not.

ai_screening_costs <- getScreeningCosts(trace = a_trace_ai_cost, ### function returns screening costs per screening repetition
                                        screening_probabilities = p_screening,
                                        screening_cost = v_cost_dt, # obtain screening costs
                                        interval = 5, 
                                        max_repititions = 4,
                                        total_cohort = t_total_cohort) # screening repition reflects the amount of repitions IN ADDITION to the screening before the markov model 
                                        max_repititions = 4) # screening repetition reflects the amount of repetitions IN ADDITION to the screening before the markov model 

#------------------------------------------------------------------------------#
####                   4b Costs medicine                    ####
#------------------------------------------------------------------------------

# Isaac: same comments apply here and I suppose below too. One of the advantages of having separating age cohorts is the 
# ability to show them separately, which is lost when these are combined in this way.  

ai_medicine_costs <- getMedicineCosts(a_trace = a_trace_ai_cost, ### function returns the total medicine costs
                                      medicine_cost = v_cost_medicine,
                                      medicine_utilisation = df_utilisation_medicine) # obtain medicine costs

soc_medicine_costs <- getMedicineCosts(a_trace = a_trace_soc_cost, # cohort trace of the patients non-compliant with AI screening
                                        medicine_cost = v_cost_medicine,
                                        medicine_utilisation = df_utilisation_medicine) # obtain medicine costs

#------------------------------------------------------------------------------#
####                   4c Costs diagnostics                    ####
#------------------------------------------------------------------------------

# Isaac: please briefly define what these costs are

ai_diagnostic_costs <- getDiagnosticCosts(trace = a_trace_ai_cost, # cohort trace of the patients non-compliant with AI screening
                                          diagnostics_cost = v_cost_utilisation_diagnostics) # obtain diagnostic costs

soc_diagnostic_costs <- getDiagnosticCosts(trace = a_trace_soc_cost, # cohort trace of the patients non-compliant with AI screening
                                           diagnostics_cost = v_cost_utilisation_diagnostics) # obtain diagnostic costs

#------------------------------------------------------------------------------#
####                   4d Costs intervention                    ####
#------------------------------------------------------------------------------

# Isaac: please briefly define what these costs are

ai_intervention_costs <- getInterventionCosts(trace = a_trace_ai_cost, # cohort trace of the patients non-compliant with AI screening
                                             intervention_cost = v_cost_utilisation_intervention) # obtain intervention costs

soc_intervention_costs <- getInterventionCosts(trace = a_trace_soc_cost, # cohort trace of the patients non-compliant with AI screening
                                               intervention_cost = v_cost_utilisation_intervention) # obtain intervention costs

#------------------------------------------------------------------------------#
####                 4e Costs burden of disease visually impaired & blind   ####
#------------------------------------------------------------------------------

# Isaac: similar comments as above. In addition: societal_perspective = TRUE -> this is an option that we should have in another 
# part of the code, so that we can easily change this once allowing to select societal or health care perspective.
# I see for example that for burden you used a_trace_ai_cost but for productivity you used list_traces_ai_costs. 
# I understand that list_traces_ai_costs is used for productivity because it's easier to assign 0 costs to some cohorts.
# But this also has the advantage of getting results separately per cohort. As mentioned above, I find the approach of combining
# traces of different length confusing and difficult to validate. I'd propose to use something like list_traces_ai_costs all the time
# and have results separated per cohort (we will need to present these anyway!) and then take the weighted average for the total cohort results.
# Also noticed that productivity costs change significantly if age_inits is changed as well.

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

# Isaac: The plots of the traces are incorrect. As discussed yesterday, these are not actual traces since the row sums do not 
# equal to 1000: rowSums(a_trace_ai_uncorrected). This is problematic and it has to do with combining traces of different length.
# I've also noticed that the number of patients in No glaucoma and in Observation are identical between AI and SoC. Is that a valid result?

#------------------------------------------------------------------------------#
####                       05 Cost-effectiveness analysis (CEA)         ####
#------------------------------------------------------------------------------#
## Incremental cost-effectiveness ratios (ICERs) 
icer <- (ai_total_costs - soc_total_costs) / (ai_total_qaly - soc_total_qaly) # Isaac: at this moment, I don't think this ICER is representative of the actual results.

# Isaac: My overall suggestion would be to re-program (in fact it is a matter of moving some code around, not an actual re-programming) the decision tree and Markov model in this file so that they are run for 
# one age cohort only, which the user can choose at the beginning. Then the models calculate the results for that specific cohort.
# To calculate the overall population results, the model will be run separately for each cohort and a weighted average of the results will be calculated. This should not be complicated since the age cohort is already an input of the decision tree and Markov model functions. You can do this I suppose with either a for loop or with lapply() as you have done above.