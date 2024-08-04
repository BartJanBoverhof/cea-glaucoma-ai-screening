getMarkovTrace <- function(scenario, # scenario
                            cohort, # cohort
                            p_screening, # probabilities of the AI for repeated screening
                            df_mortality, # mortality data
                            p_transition, # transition probabilities
                            age_init, # initial age
                            incidences, # incidence
                            severity, # severity
                            interval, # screening interval (years)
                            max_repititions # maximum number of screening repititions
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

  p_transition <- get("p_transition", envir = parent.frame())

  age_max <- 100 # time horizon               
  cycle_length <- 1 #cycle length
  
  v_utilities <- get("v_utilities", envir = parent.frame())
  discount <- get("discount", envir = parent.frame())

  n_cycles <- (age_max - age_init)/cycle_length # time horizon, number of cycles
  
  #v_age_names  <- paste(rep(age_init:(age_max-1), each = 1/cycle_length),   # labels of age vectors
  #                      1:(1/cycle_length), 
  #                      sep = ".")  # labels of age vectors
  
  n_states <- length(v_names_states)   # number of health states 
  sequence <- seq(from = interval, by = interval, length.out = max_repititions) # generate sequence of number on which repeated screening is conducted
  
  # drop age categories before and after initial age
  v_mortality <- df_mortality[(age_init-49):(age_max-50)]
  v_incidences_of <- incidences$v_incidence_of[(age_init-49):(age_max-50)]
  v_incidences_screening <- incidences$v_incidence_screening[(age_init-49):(age_max-50)]

  # transition probabilities
  p_healthy_obs <- p_transition$healthy_obs
  p_obs_healthy <- p_transition$obs_healthy

  p_mild_mod_treated <- p_transition$p_mild_mod_treated
  p_mod_sev_treated <- p_transition$p_mod_sev_treated
  p_sev_blind_treated <- p_transition$p_sev_blind_treated

  p_mild_mod_untreated <- p_transition$p_mild_mod_untreated
  p_mod_sev_untreated <- p_transition$p_mod_sev_untreated
  p_sev_blind_untreated <- p_transition$p_sev_blind_untreated

  # screening detection rates for repeated screening
  if (scenario == "ai"){
    detection_rate_flat <- p_screening$detection_rate_flat 
    detection_rate_missed <- p_screening$detection_rate_missed

    # severity stages used for the repeated screening
    severity_mild <- severity$mild
    severity_mod <- severity$moderate
    severity_severe <- severity$severe
    severity_blind <- severity$blind
  }

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

  # fill array with transition probabilities
  # from healthy
  a_matrices["No glaucoma", "No glaucoma", ]          <- 1 - (((1-v_mortality) * v_incidences_screening) + ((1-v_mortality) * p_healthy_obs) + v_mortality)
  a_matrices["No glaucoma", "Mild untreated", ]             <-     (1-v_mortality) * v_incidences_screening 
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
  a_matrices["Mild untreated", "Mild untreated", ]                <- 1 - ((1-v_mortality) * p_mild_mod_untreated + (1-v_mortality) * v_incidences_of + v_mortality)
  a_matrices["Mild untreated", "Moderate untreated", ]            <- (1-v_mortality) * p_mild_mod_untreated
  a_matrices["Mild untreated", "Mild treated", ]               <- (1-v_mortality) * v_incidences_of
  a_matrices["Mild untreated", "Death", ]               <- v_mortality
  
  # from moderate untreated
  a_matrices["Moderate untreated", "Moderate untreated", ]        <- 1 - ((1-v_mortality) * p_mod_sev_untreated + (1-v_mortality) * v_incidences_of + v_mortality)
  a_matrices["Moderate untreated", "Severe untreated", ]          <- (1-v_mortality) * p_mod_sev_untreated
  a_matrices["Moderate untreated", "Moderate treated", ]           <- (1-v_mortality) * v_incidences_of
  a_matrices["Moderate untreated", "Death", ]           <- v_mortality
  
  # from severe untreated
  a_matrices["Severe untreated", "Severe untreated", ]            <-  1 - ((1-v_mortality) * p_sev_blind_untreated + (1-v_mortality) * v_incidences_of + v_mortality)
  a_matrices["Severe untreated", "Blind", ]             <- (1-v_mortality) * p_sev_blind_untreated
  a_matrices["Severe untreated", "Severe treated", ]             <- (1-v_mortality) * v_incidences_of
  a_matrices["Severe untreated", "Death", ]             <- v_mortality

  # from observation
  a_matrices["Observation", "Observation", ]  <- 1 - (((1-v_mortality) * v_incidences_screening) + ((1-v_mortality) * p_obs_healthy) + v_mortality)
  a_matrices["Observation", "No glaucoma", ]      <- (1-v_mortality) * p_obs_healthy
  a_matrices["Observation", "Mild treated", ]         <- (1-v_mortality) * v_incidences_screening
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
    m_trace[t + 1, ]   <- m_trace[t, ]  %*% a_matrices[, , t]

    # modeling repeated screening
    if (t %in% sequence && scenario == "ai"){ # patients that are undiagnosed move to the diagnosed ones 
      
      # re-allocate participants to the treated states
      m_trace[t + 1, "Mild treated"] <- m_trace[t + 1, "Mild treated"] + (m_trace[t + 1, "Mild untreated"] * detection_rate_flat)
      m_trace[t + 1, "Moderate treated"] <- m_trace[t + 1, "Moderate treated"] + (m_trace[t + 1, "Moderate untreated"] * detection_rate_flat)
      m_trace[t + 1, "Severe treated"] <- m_trace[t + 1, "Severe treated"] + (m_trace[t + 1, "Severe untreated"] * detection_rate_flat)

      # re-define the untreated states
      m_trace[t + 1, "Mild untreated"] <- m_trace[t + 1, "Mild untreated"] * (1 - detection_rate_flat)
      m_trace[t + 1, "Moderate untreated"] <- m_trace[t + 1, "Moderate untreated"] * (1 - detection_rate_flat)
      m_trace[t + 1, "Severe untreated"] <- m_trace[t + 1, "Severe untreated"] * (1 - detection_rate_flat)

      # re-allocate participants from the "no glaucoma" and "observation" states
      # is this necessary??
     #m_trace[t + 1, "Mild treated"] <- m_trace[t + 1, "Mild treated"] + ((m_trace[t + 1, "No glaucoma"] + m_trace[t + 1, "Observation"]) * detection_rate_missed) * severity_mild
     # m_trace[t + 1, "Moderate treated"] <- m_trace[t + 1, "Moderate treated"] + ((m_trace[t + 1, "No glaucoma"] + m_trace[t + 1, "Observation"]) * detection_rate_missed) * severity_mod
     # m_trace[t + 1, "Severe treated"] <- m_trace[t + 1, "Severe treated"] + ((m_trace[t + 1, "No glaucoma"] + m_trace[t + 1, "Observation"]) * detection_rate_missed) * severity_severe
     # m_trace[t + 1, "Blind"] <- m_trace[t + 1, "Blind"] + ((m_trace[t + 1, "No glaucoma"] + m_trace[t + 1, "Observation"]) * detection_rate_missed) * severity_blind

      # re-allocate particiipants to the "no glaucoma" and "observation" states
      #m_trace[t + 1, "No glaucoma"] <- m_trace[t + 1, "No glaucoma"] * (1 - detection_rate_missed)
      #m_trace[t + 1, "Observation"] <- m_trace[t + 1, "Observation"] * (1 - detection_rate_missed)


    }
  }
  
  #------------------------------------------------------------------------------#
  ####                       04 Error handling / validation             ####
  #------------------------------------------------------------------------------#
  # Check 1: Check if all values are within the [0, 1] range
  check_values <- function(array) {
    # Flatten the array to a vector for simplicity
    flattened_array <- as.vector(array)
    
    # Check for any values outside the [0, 1] range
    if (any(flattened_array < 0 | flattened_array > 1)) {
      foo <- print("Some values are outside the range [0, 1].")
    } else {
      foo <- "All values are within the range [0, 1]."
    }
    return(foo)
  }

  check1 <- check_values(a_matrices)

  if (check1 == "Some values are outside the range [0, 1].") {
    valid_trans[[psa_iteration]] <- c(psa_iteration, check1)
  }
  
  # Check 2: Check if all rows for each slice of the array sum to 1
  check_slices <- function(array) {
    slice_sums <- apply(array, 3, function(slice) round(rowSums(slice),2))
    
    if (any(as.vector(slice_sums) != 1)) {
      foo <- print("Some horizontal slices do not sum to 1.")
    } else {
      foo <- "All horizontal slices sum to 1."
    }
    return(foo)
  }

  # Step 3: Apply the check
  check2 <- check_slices(a_matrices)

  if (check2 == "Some horizontal slices do not sum to 1.") {
    valid_array[[psa_iteration]] <- c(psa_iteration, check2)
  }

  #------------------------------------------------------------------------------#
  ####                       05 Creating corrected traces             ####
  #------------------------------------------------------------------------------#
  patients <- sum(m_trace[1,]) # number of patients in the subcohort
  trace_utility <- traceCorrectionUtil(m_trace, v_utilities_gp, age_init = age_init, utilities = v_utilities, discounting = discount) # trace corrected for utilities (discount & age)
  trace_cost <- discountTraceCosts(m_trace, discounting = discount) # trace corrected for cost discount

  return(list(trace = m_trace, patients = patients, trace_utility = trace_utility, trace_cost = trace_cost))
}
