# preliminaries
cycle_length <- 1
n_age_init <- 50
n_age_max <- 100
n_cycles <- (n_age_max - n_age_init)/cycle_length # time horizon, number of cycles

# labels of age vectors
v_age_names  <- paste(rep(n_age_init:(n_age_max-1), each = 1/cycle_length), 
                      1:(1/cycle_length), 
                      sep = ".")

# markov model states
v_names_states <- c("Healthy",       # names health states
                    "Mild", 
                    "Moderate", 
                    "Severe", 
                    "Blind", 
                    #"Observation",
                    "Death")

n_states <- length(v_names_states)   # number of health states 

# discount rates
d_c <- 0.04         # annual discount rate for costs 
d_e <- 0.015        # annual discount rate for QALYs

# strategies
v_names_str <- c("AI", # store the strategy names <FOR NOW JUST SCREEN & AI>
                 #"SoC_healthier",
                 #"SoC_sicker",
                 "SoC") 
n_str       <- length(v_names_str)   # number of strategies

# within-cycle correction
v_wcc  <- gen_wcc(n_cycles = n_cycles, method = "Simpson1/3")


# hazard ratio's SoC healthier & SoC sicker
hr_soc_healthier <- 1.2 # hazard ratio of glaucoma-related progression healthier population
hr_soc_sicker <- 0.8 # hazard ratio of glaucoma-related progression sicker population

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

# state rewards #PLACEHOLDERS
#### costs 
# <PUT IN FUNCTION LATER> <ALSO UTILITY DECRENEMENT DUE TO SCREENING? AND WHAT ABOUT OBSERVATION STATE?>
c_healthy     <- 2000  # annual cost of being healthy
c_mild        <- 4000  # annual cost of glaucoma mild
c_moderate    <- 15000 # annual cost of glaucoma moderate
c_severe      <- 20000 # annual cost of glaucoma severe
c_blind       <- 25000 # annual cost of blind
#c_observation <- foo   # annual cost of observation
c_death       <- 30000 # annual cost of death

#### utilities 
u_healthy     <- 1     # annual utility of being healthy
u_mild        <- 0.75  # annual utility of glaucoma mild
u_moderate    <- 0.6   # annual utility of glaucoma moderate
u_severe      <- 0.4     # annual utility of glaucoma severe
u_blind       <- 0.3 # annual utility of blind
#u_observation <- foo   # annual utlity of observation
u_death       <- 0 

### Discount weight for costs and effects 
v_dwc   <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
v_dwe   <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))

# Process model inputs 
#<CLEAN UP THIS LATER. The inital v_r_mort_by_age shouldnt be a tibble> 
## age-specific transition rates to the dead state for all cycles 
v_r_mortality  <- rep(v_r_mort_by_age$mortality, each = 1/cycle_length)
# Name age-specific mortality vector 
names(v_r_mortality) <- v_age_names

# compute mortality rates
#hr  <- 1     # hazard ratio all states: kept at 1 because we dont assume any difference in mortakity due to glaucoma
#v_r_S1Dage  <- v_r_HDage * hr_S1 # Age-specific mortality rate in the Sick state 
#v_r_S2Dage  <- v_r_HDage * hr_S2 # Age-specific mortality rate in the Sicker state # transform rates to probabilities adjusting by cycle length (in case nescessary)
#p_HS1       <- rate_to_prob(r = r_HS1,  t = cycle_length) # constant annual probability of becoming Sick when Healthy conditional on surviving 
#p_S1H       <- rate_to_prob(r = r_S1H,  t = cycle_length) # constant annual probability of becoming Healthy when Sick conditional on surviving
#p_S1S2      <- rate_to_prob(r = r_S1S2, t = cycle_length) # constant annual probability of becoming Sicker when Sick conditional on surviving
v_p_mortality  <- rate_to_prob(v_r_mortality,  t = cycle_length) # Age-specific mortality risk in all states

## Annual transition probability of becoming Sicker when Sick for treatment AB 
# Apply hazard ratio to rate to obtain transition rate of becoming Sicker when Sick for treatment AB
#r_S1S2_trtAB <- r_S1S2 * hr_S1S2_trtAB
# Transform rate to probability to become Sicker when Sick under treatment AB 
# adjusting by cycle length conditional on surviving
#p_S1S2_trtAB <- rate_to_prob(r = r_S1S2_trtAB, t = cycle_length)

# annual transition probabilities
p_prevalence <-  p_transition_ai$prevalence
p_mild_mod <-  p_transition_ai$p_mild_mod
p_mod_sev <-  p_transition_ai$p_mod_sev
p_sev_blind <-  p_transition_ai$p_sev_blind


# 04 Construct state-transition models
## 04.1 Initial state vector

# initial probabilities
v_m_init <- c(Healthy = p_dt_ai$p_path_fp, 
              Mild = p_dt_ai$p_path_mild, 
              Moderate = p_dt_ai$p_path_mod, 
              Severe = p_dt_ai$p_path_severe, 
              Blind = p_dt_ai$p_path_blind,
              #Observation = p_dt_ai$p_path_obs,
              Death = 0) 

## 04.2 Initialize cohort traces
### Initialize cohort trace under SoC 
m_trace_ai <- matrix(NA, 
                  nrow = (n_cycles + 1), ncol = n_states, 
                  dimnames = list(0:n_cycles, v_names_states))
# Store the initial state vector in the first row of the cohort trace
m_trace_ai[1, ] <- v_m_init

### Initialize cohort trace for AI screening strategy
# Structure and initial states are the same as for SoC
m_trace_soc <- m_trace_ai # strategy AI screening

## 04.3 Create transition probability matrices

## Create transition probability arrays for strategy SoC 
### Initialize transition probability array for strategy SoC 
# All transitions to a non-death state are assumed to be conditional on survival
a_p_ai <- array(0,
                 dim  = c(n_states, n_states, n_cycles),
                 dimnames = list(v_names_states, 
                                 v_names_states, 
                                 0:(n_cycles - 1)))
### fill in array
## from healthy
a_p_ai["Healthy", "Healthy", ]   <- (1 - v_p_mortality) * (1 - p_prevalence)
a_p_ai["Healthy", "Mild", ]  <- (1 - v_p_mortality) * p_prevalence
a_p_ai["Healthy", "Death", ]   <-      v_p_mortality
## from mild
a_p_ai["Mild", "Mild", ]  <- (1 - v_p_mortality) * (1 - p_mild_mod)
a_p_ai["Mild", "Moderate", ]  <- (1 - v_p_mortality) * p_mild_mod
a_p_ai["Mild", "Death", ]  <-      v_p_mortality
## from moderate
a_p_ai["Moderate", "Moderate", ]  <- (1 - v_p_mortality) * (1 - p_mod_sev)
a_p_ai["Moderate", "Severe", ]  <- (1 - v_p_mortality) * p_mod_sev
a_p_ai["Moderate", "Death", ]  <-      v_p_mortality
## from severe
a_p_ai["Severe", "Severe", ]  <- (1 - v_p_mortality) * (1 - p_sev_blind)
a_p_ai["Severe", "Blind", ]  <- (1 - v_p_mortality) * p_sev_blind
a_p_ai["Severe", "Death", ]  <-      v_p_mortality
## from blind
a_p_ai["Blind", "Blind", ]  <- (1 - v_p_mortality)
a_p_ai["Blind", "Death", ]  <-      v_p_mortality
## from death
a_p_ai["Death", "Death", ]   <- 1
## from death
#<FOO>
### Initialize transition probability array for soc 
a_p_soc <- a_p_ai
# update transition probabilities with soc values


## Check if transition probability arrays are valid 
### Check that transition probabilities are [0, 1] 
check_transition_probability(a_p_ai,   verbose = TRUE)
check_transition_probability(a_p_soc, verbose = TRUE)
### Check that all rows for each slice of the array sum to 1 
check_sum_of_transition_array(a_p_ai,   n_states = n_states, n_cycles = n_cycles, verbose = TRUE)
check_sum_of_transition_array(a_p_soc, n_states = n_states, n_cycles = n_cycles, verbose = TRUE)


# 05 Run Markov model

# iterative solution of age-dependent cSTM
for(t in 1:n_cycles){
  ## fill in cohort trace
  # for ai
  m_trace_ai[t + 1, ]   <- m_trace_ai[t, ]   %*% a_p_ai[, , t]
  # for strategy soc 
  m_trace_soc[t + 1, ] <- m_trace_soc[t, ] %*% a_p_soc[, , t]

}

## Store the cohort traces in a list 
l_m_M <- list(ai =  m_trace_ai,
              soc  =  m_trace_soc)
names(l_m_M) <- v_names_str


# 06 Plot Outputs

## 06.1 Plot the cohort trace for strategies SoC and AB

plot_trace(m_trace_ai)
plot_trace(m_trace_soc)

# 07 State Rewards 

## Scale by the cycle length 
# Vector of state utilities under strategy SoC
v_u_ai    <- c(Healthy  = u_healthy, 
                Mild = u_mild, 
                Moderate = u_moderate, 
                Severe  = u_severe,
                Blind = u_blind,
                #Observation = u_observation,
                Death = u_death) * cycle_length
# Vector of state costs under strategy SoC
v_c_ai    <- c(Healthy  = c_healthy, 
               Mild = c_mild, 
               Moderate = c_moderate, 
               Severe  = c_severe,
               Blind = c_blind,
               #Observation = u_observation,
               Death = c_death) * cycle_length

v_u_soc <- v_u_ai
v_c_soc <- v_c_ai

#<UTILITIES AND COSTS SOC>
## Store state rewards 
# Store the vectors of state utilities for each strategy in a list 
l_u <- list(ai  = v_u_ai,
            soc = v_u_soc)
# Store the vectors of state cost for each strategy in a list 
l_c <- list(ai  = v_c_ai,
            soc = v_c_soc)

# assign strategy names to matching items in the lists
names(l_u) <- names(l_c) <- v_names_str


# 08 Compute expected outcomes 
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


# 09 Cost-effectiveness analysis (CEA) 
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





