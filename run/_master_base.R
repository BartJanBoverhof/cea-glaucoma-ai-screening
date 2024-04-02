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
source("R/0_run.R", echo = TRUE) #Load general functions
source("R/1_model_pipeline_functions.R", echo = TRUE) #Load cohort model input functions
source("R/2_decision_tree_functions.R", echo = TRUE) #Load decision model functions
source("R/3_markov_model_functions.R", echo = TRUE) #Load decision model functions
source("R/4_costs_and_utilities_functions.R", echo = TRUE) #Load utility functions
source("R/5_sensitivity_and_scenario.R", echo = TRUE) #Load visualization functions
source("R/6_visualisation_functions.R", echo = TRUE) #Load visualization functions


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
####                       0 Define Cohorts                            ####
#------------------------------------------------------------------------------#
age_categories <- c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years") # age categories to be screened
t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)

#------------------------------------------------------------------------------#
####                       1 Run model                            ####
#------------------------------------------------------------------------------#
age50_55 <- run(cohort = age_categories[1])
age55_60 <- run(cohort = age_categories[2])
age60_65 <- run(cohort = age_categories[3])
age65_70 <- run(cohort = age_categories[4])
age70_75 <- run(cohort = age_categories[5])

# inspect trace
a_trace_soc_uncorrected <- age50_55$ai_trace + ### uncorrected trace (for reference)
    padArray(pad = age55_60$ai_trace, pad_to = age50_55$ai_trace) + 
    padArray(pad = age60_65$ai_trace, pad_to = age50_55$ai_trace) +
    padArray(pad = age65_70$ai_trace, pad_to = age50_55$ai_trace) +
    padArray(pad = age70_75$ai_trace, pad_to = age50_55$ai_trace)

# sum of the first row
sum(a_trace_soc_uncorrected[1,])

(a_trace_soc_uncorrected)

# total costs and QALYs per patient 
ai_costs_pp <- (sum(unlist(age50_55$ai_costs)) + 
                sum(unlist(age55_60$ai_costs)) + 
                sum(unlist(age60_65$ai_costs)) + 
                sum(unlist(age65_70$ai_costs)) + 
                sum(unlist(age70_75$ai_costs))) / 1000

soc_costs_pp <- (sum(unlist(age50_55$soc_costs)) + 
                sum(unlist(age55_60$soc_costs)) + 
                sum(unlist(age60_65$soc_costs)) + 
                sum(unlist(age65_70$soc_costs)) + 
                sum(unlist(age70_75$soc_costs))) / 1000

ai_qaly_pp <- (age50_55$ai_qaly + age55_60$ai_qaly + age60_65$ai_qaly + age65_70$ai_qaly + age70_75$ai_qaly) / 1000
soc_qaly_pp <- (age50_55$soc_qaly + age55_60$soc_qaly + age60_65$soc_qaly + age65_70$soc_qaly + age70_75$soc_qaly) / 1000

# icer
icer <- (ai_costs_pp - soc_costs_pp) / (ai_qaly_pp - soc_qaly_pp) 


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
icer <- (ai_costs_pp - soc_costs_pp) / (ai_qaly_pp - soc_qaly_pp) # Isaac: at this moment, I don't think this ICER is representative of the actual results.


#------------------------------------------------------------------------------#
####                       06 Deterministic sensitivity analysis         ####
#------------------------------------------------------------------------------# 
# parameters to vary in sensitivity analysis
parameters <-  list(ai_screening_costs = ai_screening_costs$cost_pp, 
                    ai_medicine_costs = ai_medicine_costs$cost_pp, 
                    ai_diagnostic_costs = ai_diagnostic_costs$cost_pp, 
                    ai_intervention_costs = ai_intervention_costs$price_pp, 
                    ai_burden = ai_burden$price_pp, 
                    ai_productivity = ai_productivity$price_pp,
                    qaly = ai_total_qaly
                    )


tornadoPlot(parameters = parameters, titleName = "Tornado diagram", outcomeName = "")


