getMarkovTrace <- function(scenario, # scenario
                            cohort, # cohort
                            screening_detection_rate, # probabilities of the AI for repeated screening
                            df_mortality, # mortality data
                            p_transition, # transition probabilities
                            age_init, # initial age
                            incidences, # incidence
                            interval, # years interval
                            max_repititions # maximum number of repititions
                           ){ 

  #------------------------------------------------------------------------------#
  ####                       01 Prerequisites                           ####
  #------------------------------------------------------------------------------#
  v_names_states <- c("No glaucoma", # markov model states
                    "Mild treated", 
                    "Moderate treated", 
                    "Severe treated", 
                    "Mild untreated",
                    "Moderate untreated",
                    "Severe untreated",
                    "Blind", 
                    "Observation",
                    "Death")

  age_max <- 100 # time horizon               
  cycle_length <- 1 #cycle length
  n_cycles <- (age_max - age_init)/cycle_length # time horizon, number of cycles
  
  v_age_names  <- paste(rep(age_init:(age_max-1), each = 1/cycle_length),   # labels of age vectors
                        1:(1/cycle_length), 
                        sep = ".")  # labels of age vectors
  
  n_states <- length(v_names_states)   # number of health states 
  sequence <- seq(from = interval, by = interval, length.out = max_repititions) # generate sequence of number on which repeated screening is conducted
  
  # drop age categories before initial age
  v_mortality <- df_mortality[(age_init-n_cycles):(age_max-n_cycles)]
  v_incidences <- incidences[(age_init-n_cycles):(age_max-n_cycles)]
  
  # transition probabilities
  p_healthy_obs <- p_transition$healthy_obs
  p_obs_healthy <- p_transition$obs_healthy

  p_mild_mod_treated <- p_transition$p_mild_mod_treated
  p_mod_sev_treated <- p_transition$p_mod_sev_treated
  p_sev_blind_treated <- p_transition$p_sev_blind_treated

  p_mild_mod_untreated <- p_transition$p_mild_mod_untreated
  p_mod_sev_untreated <- p_transition$p_mod_sev_untreated
  p_sev_blind_untreated <- p_transition$p_sev_blind_untreated

  #------------------------------------------------------------------------------#
  ####                       02 Create matrices       ####
  #------------------------------------------------------------------------------#
  # initial distribution per health state 
  v_m_init <- c(no_glaucoma = cohort$p_path_no_glaucoma, 
                mild_diagnosed = cohort$p_path_mild_diagnosed,
                moderate_diagnosed = cohort$p_path_mod_diagnosed,
                severe_diagnosed = cohort$p_path_severe_diagnosed,
                mild_undiagnosed = cohort$p_path_mild_undiagnosed,
                moderate_undiagnosed = cohort$p_path_mod_undiagnosed,
                severe_undiagnosed = cohort$p_path_severe_undiagnosed,
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
  a_matrices["No glaucoma", "No glaucoma", ]          <- 1 - (((1-v_mortality) * v_incidences) + ((1-v_mortality) * p_healthy_obs) + v_mortality)
  a_matrices["No glaucoma", "Mild untreated", ]             <-     (1-v_mortality) * v_incidences #This is not right. It shoul be changed later.
  a_matrices["No glaucoma", "Observation", ]      <-     (1-v_mortality) * p_healthy_obs
  a_matrices["No glaucoma", "Death", ]            <-     v_mortality

  # from mild treated
  a_matrices["Mild treated", "Mild treated", ]                <- 1 - ((1-v_mortality) * p_mild_mod_treated + v_mortality)
  a_matrices["Mild treated", "Moderate treated", ]            <- (1-v_mortality) * p_mild_mod_treated
  a_matrices["Mild treated", "Death", ]               <- v_mortality
  
  # from moderate treated
  a_matrices["Moderate treated", "Moderate treated", ]        <- 1 - ((1-v_mortality) * p_mod_sev_treated + v_mortality)
  a_matrices["Moderate treated", "Severe treated", ]          <- (1-v_mortality) * p_mod_sev_treated
  a_matrices["Moderate treated", "Death", ]           <- v_mortality
  
  # from severe treated
  a_matrices["Severe treated", "Severe treated", ]            <-  1 - ((1-v_mortality) * p_sev_blind_treated + v_mortality)
  a_matrices["Severe treated", "Blind", ]             <- (1-v_mortality) * p_sev_blind_treated
  a_matrices["Severe treated", "Death", ]             <- v_mortality

   # from mild untreated
  a_matrices["Mild untreated", "Mild untreated", ]                <- 1 - ((1-v_mortality) * p_mild_mod_untreated + (1-v_mortality) * v_incidences + v_mortality)
  a_matrices["Mild untreated", "Moderate untreated", ]            <- (1-v_mortality) * p_mild_mod_untreated
  a_matrices["Mild untreated", "Mild treated", ]               <- (1-v_mortality) * v_incidences
  a_matrices["Mild untreated", "Death", ]               <- v_mortality
  
  # from moderate untreated
  a_matrices["Moderate untreated", "Moderate untreated", ]        <- 1 - ((1-v_mortality) * p_mod_sev_untreated + (1-v_mortality) * v_incidences + v_mortality)
  a_matrices["Moderate untreated", "Severe untreated", ]          <- (1-v_mortality) * p_mod_sev_untreated
  a_matrices["Moderate untreated", "Moderate treated", ]           <- (1-v_mortality) * v_incidences
  a_matrices["Moderate untreated", "Death", ]           <- v_mortality
  
  # from severe untreated
  a_matrices["Severe untreated", "Severe untreated", ]            <-  1 - ((1-v_mortality) * p_sev_blind_untreated + (1-v_mortality) * v_incidences + v_mortality)
  a_matrices["Severe untreated", "Blind", ]             <- (1-v_mortality) * p_sev_blind_untreated
  a_matrices["Severe untreated", "Severe treated", ]             <- (1-v_mortality) * v_incidences
  a_matrices["Severe untreated", "Death", ]             <- v_mortality

  # from observation
  a_matrices["Observation", "Observation", ]  <- 1 - (((1-v_mortality) * v_incidences) + ((1-v_mortality) * p_obs_healthy) + v_mortality)
  a_matrices["Observation", "No glaucoma", ]      <- (1-v_mortality) * p_obs_healthy
  a_matrices["Observation", "Mild treated", ]         <- (1-v_mortality) * v_incidences
  a_matrices["Observation", "Death", ]        <- v_mortality
  
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

    if (t %in% sequence && scenario == "ai"){ # patients that are undiagnosed move to the diagnosed ones 
      m_trace[t + 1, "Mild treated"] <- m_trace[t + 1, "Mild treated"] + (m_trace[t + 1, "Mild untreated"] * screening_detection_rate)
      m_trace[t + 1, "Moderate treated"] <- m_trace[t + 1, "Moderate treated"] + (m_trace[t + 1, "Moderate untreated"] * screening_detection_rate)
      m_trace[t + 1, "Severe treated"] <- m_trace[t + 1, "Severe treated"] + (m_trace[t + 1, "Severe untreated"] * screening_detection_rate)

      m_trace[t + 1, "Mild untreated"] <- m_trace[t + 1, "Mild untreated"] * (1 - screening_detection_rate)
      m_trace[t + 1, "Moderate untreated"] <- m_trace[t + 1, "Moderate untreated"] * (1 - screening_detection_rate)
      m_trace[t + 1, "Severe untreated"] <- m_trace[t + 1, "Severe untreated"] * (1 - screening_detection_rate)

    }
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