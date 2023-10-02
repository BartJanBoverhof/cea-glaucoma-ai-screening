getMarkovTrace <- function(strat, # strategy
                           df_mortality, # mortality data
                           p_transition, # transition probabilities
                           discount_costs, # discount rate costs
                           discount_qalys # discount rate qalys
                           ){ 

    #------------------------------------------------------------------------------#
  ####                       01 Prerequisites                           ####
  #------------------------------------------------------------------------------#
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

  
  # Transition probabilities
  p_prevalence <- p_transition$prevalence
  p_mild_mod <- p_transition$p_mild_mod
  p_mod_sev <- p_transition$p_mod_sev
  p_sev_blind <- p_transition$p_sev_blind
  
  # hazard ratio's for soc_healthier and soc_sicker strategies
  hr_soc_healthier <- 0.8 # hazard ratio of glaucoma-related progression healthier population
  hr_soc_sicker <- 1.2 # hazard ratio of glaucoma-related progression sicker population
  
  # if strategy is SoC_healthier or SoC_sicker use the hazard ratio's to alter the transition probabilities defined above
  if (strat == "SoC_healthier"){
    p_mild_mod <- p_mild_mod * hr_soc_healthier
    p_mod_sev <- p_mod_sev * hr_soc_healthier
    p_sev_blind <- p_sev_blind * hr_soc_healthier
  } else if (strat == "SoC_sicker"){
    p_mild_mod <- p_mild_mod * hr_soc_sicker
    p_mod_sev <- p_mod_sev * hr_soc_sicker
    p_sev_blind <- p_sev_blind * hr_soc_sicker
  }
  
  #------------------------------------------------------------------------------#
  ####                       02 Create matrices       ####
  #------------------------------------------------------------------------------#

  # initial distribution per health state (path probability DT)
  v_m_init <- c(Healthy = p_dt_ai$p_path_fp, 
                Mild = p_dt_ai$p_path_mild, 
                Moderate = p_dt_ai$p_path_mod, 
                Severe = p_dt_ai$p_path_severe, 
                Blind = p_dt_ai$p_path_blind,
                #Observation = p_dt_ai$p_path_obs,
                Death = 0) 
  
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
  # fill array
  # from healthy
  a_matrices["Healthy", "Healthy", ]   <- (1 - df_mortality_clean) * (1 - p_prevalence)
  a_matrices["Healthy", "Mild", ]  <- (1 - df_mortality_clean) * p_prevalence
  a_matrices["Healthy", "Death", ]   <-      df_mortality_clean
  
  # from mild
  a_matrices["Mild", "Mild", ]  <- (1 - df_mortality_clean) * (1 - p_mild_mod)
  a_matrices["Mild", "Moderate", ]  <- (1 - df_mortality_clean) * p_mild_mod
  a_matrices["Mild", "Death", ]  <-      df_mortality_clean
  
  # from moderate
  a_matrices["Moderate", "Moderate", ]  <- (1 - df_mortality_clean) * (1 - p_mod_sev)
  a_matrices["Moderate", "Severe", ]  <- (1 - df_mortality_clean) * p_mod_sev
  a_matrices["Moderate", "Death", ]  <-      df_mortality_clean
  
  # from severe
  a_matrices["Severe", "Severe", ]  <- (1 - df_mortality_clean) * (1 - p_sev_blind)
  a_matrices["Severe", "Blind", ]  <- (1 - df_mortality_clean) * p_sev_blind
  a_matrices["Severe", "Death", ]  <-      df_mortality_clean
  
  # from blind
  a_matrices["Blind", "Blind", ]  <- (1 - df_mortality_clean)
  a_matrices["Blind", "Death", ]  <-      df_mortality_clean
  
  # from death
  a_matrices["Death", "Death", ]   <- 1
  # from death
  
  # check that transition probabilities are [0, 1] 
  check_transition_probability(a_matrices,   verbose = TRUE)

  # check that all rows for each slice of the array sum to 1 
  check_sum_of_transition_array(a_matrices,   n_states = n_states, n_cycles = n_cycles, verbose = TRUE)

  #------------------------------------------------------------------------------#
  ####                       03 Run Markov Model          ####
  #------------------------------------------------------------------------------#
  
  # run Markov model
  for(t in 1:n_cycles){
    # fill in cohort trace
    m_trace[t + 1, ]   <- m_trace[t, ]   %*% a_matrices[, , t]
  }
  
  # check if cohort trace sums to 1
  m_trace <- cbind(m_trace, rowSums(m_trace, na.rm = TRUE))
  colnames(m_trace)[ncol(m_trace)] <- "Sum"
  
  return(list(cohort_trace = trace))
}





