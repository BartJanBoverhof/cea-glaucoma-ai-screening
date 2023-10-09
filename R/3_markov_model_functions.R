getMarkovTrace <- function(strategy, # strategy
                           cohort, # cohort
                           df_mortality, # mortality data
                           p_transition, # transition probabilities
                           age_init, # initial age
                           age_max, # maximum age
                           names_states # names of states
                           ){ 

  #------------------------------------------------------------------------------#
  ####                       01 Prerequisites                           ####
  #------------------------------------------------------------------------------#
  cycle_length <- 1 #cycle length
  
  n_cycles <- (age_max - age_init)/cycle_length # time horizon, number of cycles
  
  v_age_names  <- paste(rep(age_init:(age_max-1), each = 1/cycle_length),   # labels of age vectors
                        1:(1/cycle_length), 
                        sep = ".")  # labels of age vectors
  
  n_states <- length(names_states)   # number of health states 

  # drop age categories before initial age
  df_mortality <- df_mortality[-c(1:(age_init-50))]

  # transition probabilities
  p_incidence <- p_transition$incidence
  p_mild_mod <- p_transition$p_mild_mod
  p_mod_sev <- p_transition$p_mod_sev
  p_sev_blind <- p_transition$p_sev_blind
  
  # hazard ratio's for soc_healthier and soc_sicker strategies
  hr_soc_healthier <- 0.8 # hazard ratio of glaucoma-related progression healthier population
  hr_soc_sicker <- 1.2 # hazard ratio of glaucoma-related progression sicker population
  
  # if strategy is SoC_healthier or SoC_sicker use the hazard ratio's to alter the transition probabilities defined above
  if (strategy == "SoC_healthier"){
    p_mild_mod <- p_mild_mod * hr_soc_healthier
    p_mod_sev <- p_mod_sev * hr_soc_healthier
    p_sev_blind <- p_sev_blind * hr_soc_healthier
  } else if (strategy == "SoC_sicker"){
    p_mild_mod <- p_mild_mod * hr_soc_sicker
    p_mod_sev <- p_mod_sev * hr_soc_sicker
    p_sev_blind <- p_sev_blind * hr_soc_sicker
  }
  
  #------------------------------------------------------------------------------#
  ####                       02 Create matrices       ####
  #------------------------------------------------------------------------------#

  if (strategy == "AI"){
    # initial distribution per health state (path probability DT)
    v_m_init <- c(healthy = cohort$p_path_fp, 
                  mild = cohort$p_path_mild, 
                  moderate = cohort$p_path_mod, 
                  severe = cohort$p_path_severe, 
                  blind = cohort$p_path_blind,
                  #observation = cohort$observation,
                  death = 0
                  )
    # total number of people in sub-cohort
    n_cohort <- sum(v_m_init)
    
    # still incorrect, think about how to do this
    } else if (strategy == "SoC_healthier"){
    # initial distribution per health state (path probability DT)
    v_m_init <- c(healthy = cohort$false_pos, 
                  mild = cohort$mild, 
                  moderate = cohort$moderate, 
                  severe = cohort$severe, 
                  blind = cohort$blind 
                  #observation = cohort$observation,
                  )
  } else if (strategy == "SoC_sicker"){
    # initial distribution per health state (path probability DT)
    v_m_init <- c(healthy = cohort$false_pos, 
                  mild = cohort$mild, 
                  moderate = cohort$moderate, 
                  severe = cohort$severe, 
                  blind = cohort$blind 
                  #observation = cohort$observation,
                  )
  }
  
  # initialize cohort trace 
  m_trace <- matrix(NA, 
                    nrow = (n_cycles + 1), ncol = n_states, 
                    dimnames = list(0:n_cycles, names_states))
  
  # store the initial state vector in the first row of the cohort trace
  m_trace[1, ] <- v_m_init
  

  # create transition probability matrices
  a_matrices <- array(0,
              dim  = c(n_states, n_states, n_cycles),
              dimnames = list(names_states, 
                              names_states, 
                                  0:(n_cycles - 1)))
  # fill array
  # from healthy
  a_matrices["Healthy", "Healthy", ]   <- (1 - df_mortality) * (1 - p_incidence)
  a_matrices["Healthy", "Mild", ]  <- (1 - df_mortality) * p_incidence
  a_matrices["Healthy", "Death", ]   <-      df_mortality
  
  # from mild
  a_matrices["Mild", "Mild", ]  <- (1 - df_mortality) * (1 - p_mild_mod)
  a_matrices["Mild", "Moderate", ]  <- (1 - df_mortality) * p_mild_mod
  a_matrices["Mild", "Death", ]  <-      df_mortality
  
  # from moderate
  a_matrices["Moderate", "Moderate", ]  <- (1 - df_mortality) * (1 - p_mod_sev)
  a_matrices["Moderate", "Severe", ]  <- (1 - df_mortality) * p_mod_sev
  a_matrices["Moderate", "Death", ]  <-      df_mortality
  
  # from severe
  a_matrices["Severe", "Severe", ]  <- (1 - df_mortality) * (1 - p_sev_blind)
  a_matrices["Severe", "Blind", ]  <- (1 - df_mortality) * p_sev_blind
  a_matrices["Severe", "Death", ]  <-      df_mortality
  
  # from blind
  a_matrices["Blind", "Blind", ]  <- (1 - df_mortality)
  a_matrices["Blind", "Death", ]  <-      df_mortality
  
  # from death
  a_matrices["Death", "Death", ]   <- 1

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
  m_trace <- cbind(m_trace, rowSums(m_trace))
  colnames(m_trace)[ncol(m_trace)] <- "Total"
  
  return(cohort_trace = m_trace)
}




