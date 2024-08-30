#------------------------------------------------------------------------------#
####                       0 Prerequisites                           ####
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
load("data/5b_v_utilities_gp.RData")
load("data/6a_v_incidences_of.RData")
load("data/6b_v_incidences_screening.RData")
load("data/6c_v_prevalence.RData")
load("data/7a_df_utilisation_medicine.RData")
load("data/8a_v_cost_dt.RData")
load("data/8b_v_cost_medicine.RData")
load("data/8c_v_cost_utilisation_diagnostics.RData")
load("data/8d_v_cost_utilisation_intervention.RData")
load("data/8f_v_cost_burden_disease.RData")
load("data/9_sensitivity_se.RData")

#------------------------------------------------------------------------------#
####                       1 Define Cohorts                            ####
#------------------------------------------------------------------------------#
age_categories <- c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years") # age categories to be screened
t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)

#------------------------------------------------------------------------------#
####                       2 Run model                           ####
#------------------------------------------------------------------------------#
# varying parameters
p_dt <- p_dt
p_severity_undiagnosed <- p_severity_undiagnosed
p_transition <- p_transition
v_utilities <- v_utilities
v_utilities_gp <- v_utilities_gp
screening_interval <- 5
discount <- TRUE
pension_age <- 67
v_dsa_se <- v_sensitivity_se

# run base case
strategy <- "base"
base <- callModel(descriptives = TRUE, perspective= "societal")

strategy <- "dsa"

# deterministic sensitivity analysis
if (strategy == "dsa") {
    dsa_se <- list()
    dsa_se$transition_untreated <- runDSA(parameter = "transition_untreated")
    dsa_se$transition_treated <- runDSA(parameter = "transition_treated")
    dsa_se$sensitivity <- runDSA(parameter = "sensitivity")
    dsa_se$specificity <- runDSA(parameter = "specificity")
    dsa_se$prevalence <- runDSA(parameter = "prevalence")
    dsa_se$incidences_of <- runDSA(parameter = "incidences_of")
    dsa_se$incidences_screening <- runDSA(parameter = "incidences_screening")
    dsa_se$utilities <- runDSA(parameter = "utilities")
    dsa_se$costs_screening <- runDSA(parameter = "costs_screening")
    dsa_se$costs_medicine <- runDSA(parameter = "costs_medicine")
    dsa_se$costs_diagnostics <- runDSA(parameter = "costs_diagnostics")
    dsa_se$costs_intervention <- runDSA(parameter = "costs_intervention")
    dsa_se$costs_burden_disease <- runDSA(parameter = "costs_burden_disease")
    dsa_se$costs_productivity <- runDSA(parameter = "costs_productivity")
    
    dsa_20 <- list()
    dsa_20$transition_untreated <- runDSA(parameter = "transition_untreated", use_se = FALSE)
    dsa_20$transition_treated <- runDSA(parameter = "transition_treated", use_se = FALSE)
    dsa_20$sensitivity <- runDSA(parameter = "sensitivity", use_se = FALSE)
    dsa_20$specificity <- runDSA(parameter = "specificity", use_se = FALSE)
    dsa_20$prevalence <- runDSA(parameter = "prevalence", use_se = FALSE)
    dsa_20$incidences_of <- runDSA(parameter = "incidences_of", use_se = FALSE)
    dsa_20$incidences_screening <- runDSA(parameter = "incidences_screening", use_se = FALSE)
    dsa_20$utilities<- runDSA(parameter = "utilities", use_se = FALSE)
    dsa_20$costs_screening <- runDSA(parameter = "costs_screening", use_se = FALSE)
    dsa_20$costs_medicine <- runDSA(parameter = "costs_medicine", use_se = FALSE)
    dsa_20$costs_diagnostics <- runDSA(parameter = "costs_diagnostics", use_se = FALSE)
    dsa_20$costs_intervention <- runDSA(parameter = "costs_intervention", use_se = FALSE)
    dsa_20$costs_burden_disease <- runDSA(parameter = "costs_burden_disease", use_se = FALSE)
    dsa_20$costs_productivity <- runDSA(parameter = "costs_productivity", use_se = FALSE)
}

#------------------------------------------------------------------------------#
####                       06 Deterministic sensitivity analysis         ####
#------------------------------------------------------------------------------#
paramNames <-  c(   "Transition probabilities untreated",
                    "Transition probabilities treated",
                    "AI sensitivity",
                    "AI specificity",
                    "Prevalence",
                    "Incidences diagnosed ",
                    "Incidences undiagnosed ",
                    "Utilities",
                    "Screening costs",
                    "Medicine costs",
                    "Monitoring costs",
                    "Laser and surgery costs",
                    "Visual imparement costs",
                    "Productivity costs"
                    )

data_se <- matrix(c(base, dsa_se$transition_untreated[1], dsa_se$transition_untreated[2],
                base, dsa_se$transition_treated[1], dsa_se$transition_treated[2],
                base, dsa_se$sensitivity[1], dsa_se$sensitivity[2],
                base, dsa_se$specificity[1], dsa_se$specificity[2],
                base, dsa_se$prevalence[1], dsa_se$prevalence[2],
                base, dsa_se$incidences_of[1], dsa_se$incidences_of[2],
                base, dsa_se$incidences_screening[1], dsa_se$incidences_screening[2],
                base, dsa_se$utilities[1], dsa_se$utilities[2], 
                base, dsa_se$costs_screening[1], dsa_se$costs_screening[2],
                base, dsa_se$costs_medicine[1], dsa_se$costs_medicine[2],
                base, dsa_se$costs_diagnostics[1], dsa_se$costs_diagnostics[2],
                base, dsa_se$costs_intervention[1], dsa_se$costs_intervention[2],
                base, dsa_se$costs_burden_disease[1], dsa_se$costs_burden_disease[2],
                base, dsa_se$costs_productivity[1], dsa_se$costs_productivity[2]),
                nrow = length(paramNames), ncol = 3, byrow = TRUE)

data_20 <- matrix(c(base, dsa_20$transition_untreated[1], dsa_20$transition_untreated[2],
                base, dsa_20$transition_treated[1], dsa_20$transition_treated[2],
                base, dsa_20$sensitivity[1], dsa_20$sensitivity[2],
                base, dsa_20$specificity[1], dsa_20$specificity[2],
                base, dsa_20$prevalence[1], dsa_20$prevalence[2],
                base, dsa_20$incidences_of[1], dsa_20$incidences_of[2],
                base, dsa_20$incidences_screening[1], dsa_20$incidences_screening[2],
                base, dsa_20$utilities[1], dsa_20$utilities[2],
                base, dsa_20$costs_screening[1], dsa_20$costs_screening[2],
                base, dsa_20$costs_medicine[1], dsa_20$costs_medicine[2],
                base, dsa_20$costs_diagnostics[1], dsa_20$costs_diagnostics[2],
                base, dsa_20$costs_intervention[1], dsa_20$costs_intervention[2],
                base, dsa_20$costs_burden_disease[1], dsa_20$costs_burden_disease[2],
                base, dsa_20$costs_productivity[1], dsa_20$costs_productivity[2]),
                nrow = length(paramNames), ncol = 3, byrow = TRUE)

Parms = paramNames
Outcomes = data
titleName = "Tornado diagram"

#save plot
tornadoPlot(Parms = paramNames, Outcomes_se = data_se, Outcomes_20 = data_20, titleName = "Tornado diagram", outcomeName_se = "By standard errors", outcomeName_20 = "By 20% of the mean")



















