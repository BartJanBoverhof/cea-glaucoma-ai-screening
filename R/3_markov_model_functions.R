getMarkovTrace <- function(strategy, # strategy
                           cohort, # cohort
                           df_mortality, # mortality data
                           p_transition, # transition probabilities
                           age_init, # initial age
                           age_max, # maximum age
                           incidences # incidence
                           ){ 

  #------------------------------------------------------------------------------#
  ####                       01 Prerequisites                           ####
  #------------------------------------------------------------------------------#
  v_names_states <- c("Healthy", # markov model states
                    "Mild", 
                    "Moderate", 
                    "Severe", 
                    "Blind", 
                    "Observation",
                    "Death")
                    
  cycle_length <- 1 #cycle length
  
  n_cycles <- (age_max - age_init)/cycle_length # time horizon, number of cycles
  
  v_age_names  <- paste(rep(age_init:(age_max-1), each = 1/cycle_length),   # labels of age vectors
                        1:(1/cycle_length), 
                        sep = ".")  # labels of age vectors
  
  n_states <- length(v_names_states)   # number of health states 

  # drop age categories in mortality object before initial age
  v_mortality <- df_mortality[-c(1:(age_init-50))]

  # drop age categories in incidence object before initial age
  v_incidences <- incidences[-c(1:(age_init-50))]
  
  # transition probabilities
  p_mild_mod <- p_transition$p_mild_mod
  p_mod_sev <- p_transition$p_mod_sev
  p_sev_blind <- p_transition$p_sev_blind
  p_healthy_obs <- p_transition$healthy_obs
  p_obs_healthy <- p_transition$obs_healthy
  
  # put transition probability observation to 0 if strategy is soc, low_risk, or high_risk
  #if(strategy == "soc" | strategy == "low_risk" | strategy == "high_risk"){
  #  p_healthy_obs <- 0
  #  p_obs_healthy <- 0
  #}

  #------------------------------------------------------------------------------#
  ####                       02 Create matrices       ####
  #------------------------------------------------------------------------------#
  # initial distribution per health state for AI strategy (path probability DT)
  v_m_init <- c(healthy = cohort$p_path_no_glaucoma, 
                mild = cohort$p_path_mild, 
                moderate = cohort$p_path_mod, 
                severe = cohort$p_path_severe, 
                blind = cohort$p_path_blind,
                observation = cohort$p_path_obs,
                death = 0
                )
  # total number of people in sub-cohort
  n_cohort <- sum(v_m_init)

  # initialize cohort trace 
  m_trace <- matrix(NA, 
                    nrow = (n_cycles + 1), ncol = n_states, 
                    dimnames = list(0:n_cycles, v_names_states))
  
  # store the initial state vector in the first row of the cohort trace
  m_trace[1, ] <- v_m_init
  
  # create transition probability matrices
  a_matrices <- array(0,
              dim  = c(n_states, n_states, n_cycles),
              dimnames = list(v_names_states, 
                              v_names_states, 
                                  0:(n_cycles - 1)))
  
  #do a rowsum of the array
  #sum(a_matrices[,,30][2,])

  # fill array with transition probabilities
  # from healthy
  a_matrices["Healthy", "Healthy", ]          <- 1 - (((1-v_mortality) * v_incidences) + ((1-v_mortality) * p_healthy_obs) + v_mortality)
  a_matrices["Healthy", "Mild", ]             <-     (1-v_mortality) * v_incidences
  a_matrices["Healthy", "Observation", ]      <-     (1-v_mortality) * p_healthy_obs
  a_matrices["Healthy", "Death", ]            <-     v_mortality

    # from observation
  a_matrices["Observation", "Observation", ]  <- 1 - (((1-v_mortality) * v_incidences) + ((1-v_mortality) * p_obs_healthy) + v_mortality)
  a_matrices["Observation", "Healthy", ]      <- (1-v_mortality) * p_obs_healthy
  a_matrices["Observation", "Mild", ]         <- (1-v_mortality) * v_incidences
  a_matrices["Observation", "Death", ]        <- v_mortality

  # from mild
  a_matrices["Mild", "Mild", ]                <- 1 - ((1-v_mortality) * p_mild_mod + v_mortality)
  a_matrices["Mild", "Moderate", ]            <- (1-v_mortality) * p_mild_mod
  a_matrices["Mild", "Death", ]               <- v_mortality
  
  # from moderate
  a_matrices["Moderate", "Moderate", ]        <- 1 - ((1-v_mortality) * p_mod_sev + v_mortality)
  a_matrices["Moderate", "Severe", ]          <- (1-v_mortality) * p_mod_sev
  a_matrices["Moderate", "Death", ]           <- v_mortality
  
  # from severe
  a_matrices["Severe", "Severe", ]            <-  1 - ((1-v_mortality) * p_sev_blind + v_mortality)
  a_matrices["Severe", "Blind", ]             <- (1-v_mortality) * p_sev_blind
  a_matrices["Severe", "Death", ]             <- v_mortality
  
  # from blind
  a_matrices["Blind", "Blind", ]              <- 1 - v_mortality
  a_matrices["Blind", "Death", ]              <- v_mortality
  
  # from death
  a_matrices["Death", "Death", ]              <- 1
  #------------------------------------------------------------------------------#
  ####                       03 Run Markov Model          ####
  #------------------------------------------------------------------------------#
  
  # run Markov model
  for(t in 1:n_cycles){
    # fill in cohort trace
    m_trace[t + 1, ]   <- m_trace[t, ]   %*% a_matrices[, , t]
  }
  
  #------------------------------------------------------------------------------#
  ####                       04 Error handling / validation             ####
  #------------------------------------------------------------------------------#
  # check that transition probabilities are [0, 1] 
  check_transition_probability(a_matrices,   verbose = TRUE)
  
  # check that all rows for each slice of the array sum to 1 
  check_sum_of_transition_array(a_matrices,   n_states = n_states, n_cycles = n_cycles, verbose = TRUE)
  
  # append column to m_trace that checks if all rows in the cohort trace sum up to 100% of the cohort
  #m_trace <- cbind(m_trace, rowSums(m_trace))
  #colnames(m_trace)[ncol(m_trace)] <- "Total"
  
  return(cohort_trace = m_trace)
}


