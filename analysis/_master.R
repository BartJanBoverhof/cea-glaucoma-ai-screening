rm(list = ls()) # to clean the workspace

# loading packages
p_load_gh("DARTH-git/darthtools")
if (!require('pacman')) install.packages('pacman'); library(pacman) # use this package to conveniently install other packages
p_load("dplyr", "tidyr", "reshape2", "devtools", "scales", "ellipse", "ggplot2", "ggrepel", "gridExtra", "lazyeval", "igraph", "truncnorm", "ggraph", "reshape2", "patchwork", "knitr", "stringr", "diagram", "dampack","DiagrammeR") # load (install if required) packages from CRAN
p_load_gh("DARTH-git/darthtools") # load (install if required) packages from GitHub

# importing Functions
source("R/1_model_pipeline_functions.R", echo = TRUE) #Load cohort model input functions
source("R/2_decision_tree_functions.R", echo = TRUE) #Load decision model functions
source("R/3_markov_model_functions.R", echo = TRUE) #Load decision model functions

# loading all required data objects
load("data/1_all_cause_mortality.RData")
load("data/2_dt_ai.RData")
load("data/3_severity_undiagnosed.RData")
load("data/4a_transition_prob_screen.RData")

# obtain cohorts
total_cohort <- get_cohort(all_cause_mortality, age_categories = c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years")) # step 1: get cohort
dt <- get_dt_probabilities(dt_ai, severity_undiagnosed, "Ai Screening", visualize = T) # step 2: get dt probabilities
cohort <- get_cohort_arm(total_cohort, dt) # step 3: obtain cohort per decision-tree-arm

# preliminaries
cycle_length <- 1
n_age_init <- 50
n_age_max <- 75
n_cycles <- (n_age_max - n_age_init)/cycle_length # time horizon, number of cycles

v_age_names  <- paste(rep(n_age_init:(n_age_max-1), each = 1/cycle_length), 
                      1:(1/cycle_length), 
                      sep = ".")

v_names_states <- c("H",  # healthy 
                    "Mild", # glaucoma sick 
                    "Mod", # glaucoma moderate
                    "Sev", # glaucoma severe
                    "Blind")  # blind 

n_states <- length(v_names_states)   # number of health states 

d_c <- 0.04         # annual discount rate for costs 
d_e <- 0.015        # annual discount rate for QALYs

# strategies
v_names_str <- c("SoC", # store the strategy names
                 "SoC_healthier",
                 "SoC_sicker",
                 "Screen") 
n_str       <- length(v_names_str)   # number of strategies

# within-cycle correction
v_wcc  <- gen_wcc(n_cycles = n_cycles, method = "Simpson1/3")

hr_S1  <- 3     # hazard ratio all states

### Effectiveness of treatment AB 
hr_S1S2_trtAB <- 0.6  # hazard ratio of becoming Sicker when Sick under treatment AB

# age-dependent mortality rates 
# extract age-specific all-cause mortality for ages in model time horizon
# <PUT IN FUNCTION LATER>
v_r_mort_by_age <- all_cause_mortality %>%
  filter(Attribute == "Total male and female") %>%
  filter(!(Age %in% c("Total of all ages", "40 to 45 years", "45 to 50 years"))) %>% 
  transmute(Age = Age, mortality = (`All causes of death` / `Average population`)) %>% 
  separate(Age, into = c("start_age", "end_age"), sep = " to ", remove = FALSE) %>%
  mutate(
    start_age = as.numeric(gsub("[^0-9]", "", start_age)),
    end_age = as.numeric(gsub("[^0-9]", "", end_age))
  ) %>% 
  rowwise() %>%
  mutate(year = list(seq(from = start_age, to = end_age - 1))) %>%
  unnest(year) %>%
  select(year, mortality)


