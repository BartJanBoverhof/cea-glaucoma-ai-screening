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
n_age_max <- 100
n_cycles <- (n_age_max - n_age_init)/cycle_length # time horizon, number of cycles

v_age_names  <- paste(rep(n_age_init:(n_age_max-1), each = 1/cycle_length), 
                      1:(1/cycle_length), 
                      sep = ".")

v_names_states <- c("Healthy",  # healthy 
                    "Mild", # glaucoma sick 
                    "Moderate", # glaucoma moderate
                    "Severe", # glaucoma severe
                    "Blind",
                    "Death")  # blind 

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

hr  <- 1     # hazard ratio all states

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

### State rewards #placeholders
#### Costs 
# <PUT IN FUNCTION LATER>
c_H     <- 2000  # annual cost of being healthy
c_S1    <- 4000  # annual cost of glaucoma mild
c_S2    <- 15000 # annual cost of glaucoma moderate
c_S3    <- 20000 # annual cost of glaucoma severe
c_B     <- 25000 # annual cost of  blind
#### Utilities 
u_H     <- 1     # annual utility of being healthy
u_S1    <- 0.75  # annual utility of glaucoma mild
u_S2    <- 0.5   # annual utility of glaucoma moderate
u_D     <- 0     # annual utility of glaucoma severe
u_trtAB <- 0.95  # annual utility when receiving treatment AB

### Discount weight for costs and effects 
v_dwc   <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
v_dwe   <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))

# Process model inputs 
#<CLEAN UP THIS LATER. The inital v_r_mort_by_age shouldnt be a tibble> 
## Age-specific transition rates to the Dead state for all cycles 
v_r_HDage  <- rep(v_r_mort_by_age$mortality, each = 1/cycle_length)
# Name age-specific mortality vector 
names(v_r_HDage) <- v_age_names

# compute mortality rates
v_r_S1Dage  <- v_r_HDage * hr # Age-specific mortality rate in all states
# transform rates to probabilities adjusting by cycle length (in case nescessary)
#p_HS1       <- rate_to_prob(r = r_HS1,  t = cycle_length) # constant annual probability of becoming Sick when Healthy conditional on surviving 
#p_S1H       <- rate_to_prob(r = r_S1H,  t = cycle_length) # constant annual probability of becoming Healthy when Sick conditional on surviving
#p_S1S2      <- rate_to_prob(r = r_S1S2, t = cycle_length) # constant annual probability of becoming Sicker when Sick conditional on surviving
#v_p_HDage   <- rate_to_prob(v_r_HDage,  t = cycle_length) # Age-specific mortality risk in the Healthy state 
#v_p_S1Dage  <- rate_to_prob(v_r_S1Dage, t = cycle_length) # Age-specific mortality risk in the Sick state
#v_p_S2Dage  <- rate_to_prob(v_r_S2Dage, t = cycle_length) # Age-specific mortality risk in the Sicker state

## Annual transition probability of becoming Sicker when Sick for treatment AB 
# Apply hazard ratio to rate to obtain transition rate of becoming Sicker when Sick for treatment AB
#r_S1S2_trtAB <- r_S1S2 * hr_S1S2_trtAB
# Transform rate to probability to become Sicker when Sick under treatment AB 
# adjusting by cycle length conditional on surviving
#p_S1S2_trtAB <- rate_to_prob(r = r_S1S2_trtAB, t = cycle_length)


# 04 Construct state-transition models
## 04.1 Initial state vector
v_r_mort_by_age

v_m_init <- c(H = severity_undiagnosed$undiagnosed_mild, S1 = 0, S2 = 0, D = 0) # initial state vector
v_m_init

## 04.2 Initialize cohort traces


### Initialize cohort trace under SoC 
m_M_SoC <- matrix(NA, 
                  nrow = (n_cycles + 1), ncol = n_states, 
                  dimnames = list(0:n_cycles, v_names_states))
# Store the initial state vector in the first row of the cohort trace
m_M_SoC[1, ] <- v_m_init

### Initialize cohort trace for strategy AB 
# Structure and initial states are the same as for SoC
m_M_strAB <- m_M_SoC # Strategy AB

## 04.3 Create transition probability matrices

```{r}
## Create transition probability arrays for strategy SoC 
### Initialize transition probability array for strategy SoC 
# All transitions to a non-death state are assumed to be conditional on survival
a_P_SoC <- array(0,
                 dim  = c(n_states, n_states, n_cycles),
                 dimnames = list(v_names_states, 
                                 v_names_states, 
                                 0:(n_cycles - 1)))
### Fill in array
## From H
a_P_SoC["H", "H", ]   <- (1 - v_p_HDage) * (1 - p_HS1)
a_P_SoC["H", "S1", ]  <- (1 - v_p_HDage) *      p_HS1
a_P_SoC["H", "D", ]   <-      v_p_HDage
## From S1
a_P_SoC["S1", "H", ]  <- (1 - v_p_S1Dage) *       p_S1H
a_P_SoC["S1", "S1", ] <- (1 - v_p_S1Dage) * (1 - (p_S1H + p_S1S2))
a_P_SoC["S1", "S2", ] <- (1 - v_p_S1Dage) *               p_S1S2
a_P_SoC["S1", "D", ]  <-      v_p_S1Dage
## From S2
a_P_SoC["S2", "S2", ] <- 1 - v_p_S2Dage
a_P_SoC["S2", "D", ]  <-     v_p_S2Dage
## From D
a_P_SoC["D", "D", ]   <- 1

### Initialize transition probability array for strategy AB 
a_P_strAB <- a_P_SoC
# Update only transition probabilities from S1 involving p_S1S2
a_P_strAB["S1", "S1", ] <- (1 - v_p_S1Dage) * (1 - (p_S1H + p_S1S2_trtAB))
a_P_strAB["S1", "S2", ] <- (1 - v_p_S1Dage) *               p_S1S2_trtAB

## Check if transition probability arrays are valid 
### Check that transition probabilities are [0, 1] 
check_transition_probability(a_P_SoC,   verbose = TRUE)
check_transition_probability(a_P_strAB, verbose = TRUE)
### Check that all rows for each slice of the array sum to 1 
check_sum_of_transition_array(a_P_SoC,   n_states = n_states, n_cycles = n_cycles, verbose = TRUE)
check_sum_of_transition_array(a_P_strAB, n_states = n_states, n_cycles = n_cycles, verbose = TRUE)
```


# 05 Run Markov model

```{r}
# Iterative solution of age-dependent cSTM
for(t in 1:n_cycles){
  ## Fill in cohort trace
  # For SoC
  m_M_SoC[t + 1, ]   <- m_M_SoC[t, ]   %*% a_P_SoC[, , t]
  # For strategy AB 
  m_M_strAB[t + 1, ] <- m_M_strAB[t, ] %*% a_P_strAB[, , t]
  
}

## Store the cohort traces in a list 
l_m_M <- list(SoC =  m_M_SoC,
              AB  =  m_M_strAB)
names(l_m_M) <- v_names_str


```

# 06 Plot Outputs

## 06.1 Plot the cohort trace for strategies SoC and AB

```{r}
plot_trace(m_M_SoC)
plot_trace(m_M_strAB)
```

# 07 State Rewards 

```{r}
## Scale by the cycle length 
# Vector of state utilities under strategy SoC
v_u_SoC    <- c(H  = u_H, 
                S1 = u_S1, 
                S2 = u_S2, 
                D  = u_D) * cycle_length
# Vector of state costs under strategy SoC
v_c_SoC    <- c(H  = c_H, 
                S1 = c_S1,
                S2 = c_S2, 
                D  = c_D) * cycle_length
# Vector of state utilities under strategy AB
v_u_strAB  <- c(H  = u_H, 
                S1 = u_trtAB, 
                S2 = u_S2, 
                D  = u_D) * cycle_length
# Vector of state costs under strategy AB
v_c_strAB  <- c(H  = c_H, 
                S1 = c_S1 + c_trtAB, 
                S2 = c_S2 + c_trtAB, 
                D  = c_D) * cycle_length

## Store state rewards 
# Store the vectors of state utilities for each strategy in a list 
l_u <- list(SoC = v_u_SoC,
            AB  = v_u_strAB)
# Store the vectors of state cost for each strategy in a list 
l_c <- list(SoC =  v_c_SoC,
            AB  =  v_c_strAB)

# assign strategy names to matching items in the lists
names(l_u) <- names(l_c) <- v_names_str
```

# 08 Compute expected outcomes 

```{r}
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
```

# 09 Cost-effectiveness analysis (CEA) 

```{r}
## Incremental cost-effectiveness ratios (ICERs) 
df_cea <- calculate_icers(cost       = v_tot_cost, 
                          effect     = v_tot_qaly,
                          strategies = v_names_str)
df_cea
```

```{r}
## CEA table in proper format 
table_cea <- format_table_cea(df_cea)
table_cea
```

```{r}
## CEA frontier 
plot(df_cea, label = "all", txtsize = 16) +
  expand_limits(x = max(table_cea$QALYs) + 0.1) +
  theme(legend.position = c(0.82, 0.3))
```




