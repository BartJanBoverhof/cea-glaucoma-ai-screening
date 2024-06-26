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
load("data/9b_v_psa_sd.RData")

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
v_utilities_gp<- v_utilities_gp

# run base case
strategy <- "psa"

# probabalistic sensitivity analysis
runPSA()
 

#------------------------------------------------------------------------------#
####                       04 Visualization         ####
#------------------------------------------------------------------------------#
# plot trace output
#n_cycles <- nrow(a_trace_soc_uncorrected) -1
#v_names_states <- colnames(a_trace_soc_uncorrected)
#plot_trace(a_trace_soc_uncorrected)
#plot_trace(a_trace_soc_uncorrected)



#------------------------------------------------------------------------------#
####                       06 Deterministic sensitivity analysis         ####
#------------------------------------------------------------------------------#
paramNames <-  c(   "Transition probabilities untreated [-/+ 20%]",
                    "Transition probabilities treated [-/+ 20%]",
                    "AI sensitivity [-/+ 20% points] (max 1)",
                    "AI specificity [-/+ 20% points] (max 1)",
                    "Prevalence [-/+ 20%]",
                    "OF detection rate [-/+ 20%]",
                    "Incidences [-/+ 20%]",
                    "Utilities untreated [-/+ 10%]",
                    "Utilities treated [-/+ 10%]",
                    "Screening costs [-/+ 20%]",
                    "Medicine costs [-/+ 20%]",
                    "Diagnostics costs [-/+ 20%]",
                    "Laser and surgery costs [-/+ 20%]",
                    "Burden of disease costs [-/+ 20%]",
                    "Productivity costs [-/+ 20%]"
                    )

data <- matrix(c(base, dsa$transition_untreated[1], dsa$transition_untreated[2],
                base, dsa$transition_treated[1], dsa$transition_treated[2],
                base, dsa$sensitivity[1], dsa$sensitivity[2],
                base, dsa$specificity[1], dsa$specificity[2],
                base, dsa$prevalence[1], dsa$prevalence[2],
                base, dsa$incidences_of[1], dsa$incidences_of[2],
                base, dsa$incidences_screening[1], dsa$incidences_screening[2],
                #base, dsa$utilities_untreated[1], dsa$utilities_untreated[2],
                #base, dsa$utilities_treated[1], dsa$utilities_treated[2], 
                base, dsa$costs_screening[1], dsa$costs_screening[2],
                base, dsa$costs_medicine[1], dsa$costs_medicine[2],
                base, dsa$costs_diagnostics[1], dsa$costs_diagnostics[2],
                base, dsa$costs_intervention[1], dsa$costs_intervention[2],
                base, dsa$costs_burden_disease[1], dsa$costs_burden_disease[2],
                base, dsa$costs_productivity[1], dsa$costs_productivity[2]),
                nrow = length(paramNames), ncol = 3, byrow = TRUE)

Parms = paramNames
Outcomes = data
titleName = "Tornado diagram"

#save plot
ggsave("figures/tornado_plot.png", tornadoPlot(Parms = paramNames, Outcomes = data, titleName = "Tornado diagram", outcomeName = ""), width = 20, height = 15, units = "cm")



s

















