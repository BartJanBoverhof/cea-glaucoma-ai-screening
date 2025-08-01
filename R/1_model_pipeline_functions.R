# Function Name: getCohort
# Description: Returns age-weighted distribution for a specified sex.
getCohort <- function(data, 
                       age_categories = c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years"), 
                       sex = "Total male and female"
                       ){

  total_population <- data %>%
    dplyr::filter(Age == "Total of all ages") %>%
    dplyr::filter(Attribute == sex) %>%
    dplyr::summarise(total_population = sum(`Average population`, na.rm = TRUE))
  
  total_cohort <- data %>%
    dplyr::filter(Age %in% age_categories) %>%
    dplyr::filter(Attribute %in% sex) %>%
    dplyr::summarise(total_population = sum(`Average population`, na.rm = TRUE))
  
  age_weighted <- data %>%
    dplyr::filter(Attribute == sex) %>%
    dplyr::filter(Age != "Total of all ages") %>%
    dplyr::filter(Age %in% age_categories) %>%
    dplyr::mutate(age_weighted = `Average population` / total_cohort$total_population)

  vector <- as.vector(age_weighted$age_weighted)
  names(vector) <- age_weighted$Age
  
  return(vector)
}


# function to calculate mortality
# Function Name: calculateMortality
# Description: Prepares mortality rates per cycle for given age range.
calculateMortality <- function(df_mortality, # mortality data
                               age_start, # start age of the cohort
                               age_max, # maximum age of the cohort
                               cycle_length = 1 # cycle length in years
                               ){
  start_age <- age_start
  end_age <- age_max

  # filter, transform and reshape the mortality data

  v_r_mort_by_age <- df_mortality %>%
    filter(Attribute == "Total male and female") %>%
    filter(!(Age %in% c("Total of all ages", "40 to 45 years", "45 to 50 years"))) %>% 
    filter(start_age <= as.numeric(gsub("[^0-9]", "", Age))) %>%  # Filter by start_age
    transmute(Age = Age, mortality = (`All causes of death` / `Average population`)) %>% # divide by five to bring it to a yearly rate
    separate(Age, into = c("start_age", "end_age"), sep = " to ", remove = FALSE) %>%
    mutate(
      start_age = as.numeric(gsub("[^0-9]", "", start_age)),
      end_age = as.numeric(gsub("[^0-9]", "", end_age))
    ) %>%
    rowwise() %>%
    mutate(year = list(seq(from = start_age, to = end_age - 1))) %>%
    unnest(year) %>%
    select(year, mortality)
  
  # remove rows that are lower than start_age and higher than end_age
  v_r_mort_by_age <- v_r_mort_by_age %>%
    filter(year >= start_age & year <= end_age)
  
  # age-specific transition rates to the dead state for all cycles 
  v_r_mortality  <- rep(v_r_mort_by_age$mortality, each = 1/cycle_length)
  # name the ages in the vector
  names(v_r_mortality) <- v_r_mort_by_age$year

  #v_p_mortality  <- rate_to_prob(v_r_mortality,  t = cycle_length) # Age-specific mortality risk in all states

  return(v_r_mortality)
}

# function to calculate incidence
# Function Name: calculateIncidence
# Description: Interpolates incidence data for use in the model.
calculateIncidence <- function(incidences,
                               age_start,
                               age_max,
                               cycle_length =1){

  v_incidences_modified <- incidences %>%  
    separate(age, into = c("start_age", "end_age"), sep = "-", remove = FALSE) %>%
    mutate(
          start_age = as.numeric(gsub("[^0-9]", "", start_age)),
          end_age = as.numeric(gsub("[^0-9]", "", end_age))
        ) %>%
        rowwise() %>%
        mutate(year = list(seq(from = start_age, to = end_age ))) %>%
        unnest(year) %>%
        select(year, incidence)

  # remove rows that are lower than start_age and higher than end_age
  v_incidences_modified_by_age <- v_incidences_modified %>%
    filter(year >= age_start & year <= age_max)

  # age-specific transition rates to the dead state for all cycles 
  v_incidences_modified_by_age  <- rep(v_incidences_modified_by_age$incidence, each = 1/cycle_length)  
  
  # generate a sequence of ages 
  v_age <- seq(from = age_start, to = age_max-1, by = 1/cycle_length)

  # name the ages in the vector
  names(v_incidences_modified_by_age) <- v_age

  return (v_incidences_modified_by_age)
}


# Function Name: getScreeningProbabilities
# Description: Generates screening-related probabilities based on test performance and compliance.
getScreeningProbabilities <- function(probabilities, model_compliance, p_prevalence){

  if (model_compliance == FALSE){
    p_screen_compliance <-  1     # screening compliance
    p_referral_compliance <- 1   # referral compliance
  } else if (model_compliance == TRUE) {
    p_screen_compliance <-  probabilities$screen_comp # screening compliance
    p_referral_compliance <- probabilities$ref_comp   # referral compliance
  }

# calculate required probabilities
ai_tp <- probabilities$ai_sens * p_prevalence  # true positives
ai_fp <- (1-probabilities$ai_spec) * (1-p_prevalence) # false positives
ai_tn <- probabilities$ai_spec * (1-p_prevalence) # true negatives
ai_fn <- (1-probabilities$ai_sens) * p_prevalence # false negatives

ai_positive <- ai_tp + ai_fp # positive test result
ai_negative <- ai_tn + ai_fn # negative test result

ai_ppv <- ai_tp / ai_positive # positive predictive value
ai_npv <- ai_tn / ai_negative # negative predictive value

p_invited <- 1 # everyone is invited to screening
p_ai_screened <- p_screen_compliance * p_invited # probability of being screened
p_followed_up <- p_screen_compliance * p_referral_compliance * ai_positive

detection_rate_flat <- p_screen_compliance * p_referral_compliance * probabilities$ai_sens
#detection_rate_missed <- p_screen_compliance * p_referral_compliance * (1- probabilities$ai_sens) * ai_tp # detection rate in the population that are previously missed (used in markov model)
#p_path_no_glaucoma <- p_screen_compliance * ai_positive * p_referral_compliance * ai_ppv

return(list(p_invited = p_invited, p_ai_screened = p_ai_screened, p_followed_up = p_followed_up, detection_rate_flat = detection_rate_flat))
}

# Function Name: padArray
# Description: Pads a matrix with zero rows to match another matrix size.
padArray <- function(pad, pad_to) {
  if(nrow(pad) < nrow(pad_to)) {
    rows_to_add <- nrow(pad_to) - nrow(pad)
    padding <- matrix(0, nrow = rows_to_add, ncol = ncol(pad))
    pad <- rbind(pad, padding)
  }
  return(pad)
}

# Function Name: traceCorrectionUtil
# Description: Applies age-based utility adjustment and discounting to a Markov trace.
traceCorrectionUtil <- function(a_trace, utility_gp, age_init, utilities, discounting) {
  
  # Age correction
  # max age
  age_max <- nrow(a_trace) + age_init -1
  
  #select categories age age_init to age_max
  age_correction <- subset(utility_gp, age >= age_init & age <= age_max) %>% select(utility)

  # multiply each collumn in the trace with the age correction collumn
  a_trace_corrected <- sweep(a_trace, 1, age_correction$utility, "*")   # age correction

  # drop last element of list utilities
  utilities <- utilities[1:(length(utilities)-1)]

  # multiply each collumn in the trace with each item in the list utilities
  a_trace_corrected <- sweep(a_trace_corrected, 2, as.numeric(utilities), "*") # discounting

  # discounting
  if (discounting == TRUE) {
    exponents <- seq(from = 0, to = -(nrow(a_trace_corrected)-1), length.out = nrow(a_trace_corrected))
    sequence <- (1 + 0.015) ^ exponents
    a_trace_discount <-  a_trace_corrected * sequence  
  } else {
    a_trace_discount <- a_trace_corrected
  }

  # half-cycle correction
  num_cycles <- nrow(a_trace_discount)-1
  corrected_trace <- a_trace_discount

  # loop over trace
  for (i in 2:num_cycles) {
    corrected_trace[i,] <- (a_trace_discount[i,] + a_trace_discount[i-1,]) * 0.5
  }
  
  corrected_trace <- corrected_trace[1:num_cycles,]

  return(corrected_trace)
}

# Function Name: discountTraceCosts
# Description: Applies cost discounting and half-cycle correction to a Markov trace.
discountTraceCosts <- function(a_trace, discounting) {
  
  if (discounting == TRUE) {
    # discounting
    exponents <- seq(from = 0, to = -(nrow(a_trace)-1), length.out = nrow(a_trace))
    sequence <- (1 + 0.03) ^ exponents
    a_trace_discount <-  a_trace * sequence
  } else {
    a_trace_discount <- a_trace
  }
  
  # half-cycle correction
  num_cycles <- nrow(a_trace_discount)-1
  corrected_trace <- a_trace_discount

  # loop over trace
  for (i in 2:num_cycles) {
    corrected_trace[i,] <- (a_trace_discount[i,] + a_trace_discount[i-1,]) * 0.5
  }
  
  corrected_trace <- corrected_trace[1:num_cycles,]

  return(corrected_trace)
}

# Function Name: sampleBeta
# Description: Draws a random sample from a beta distribution using mean and standard error.
sampleBeta <- function(mu, se) {
  
  # defining alpha & beta 
  alpha <- mu * ((mu * (1 - mu) / se^2) - 1)
  beta <- alpha * (1 - mu) / mu
  
  # sample from beta distribution
  sample <- mapply(function(a, b) rbeta(1, a, b), alpha, beta)
  
  return(sample)
}

# Function Name: sampleGamma
# Description: Draws a random sample from a gamma distribution using mean and standard error.
sampleGamma <- function(mu, se) {
  
  # defining alpha & beta
  alpha <- (mu^2) / (se^2)
  beta <- (se^2) / mu

  # sample from gamma distribution
  sample <- mapply(function(a, b) rgamma(1, shape = a, scale =b), alpha, beta)
  return(sample)
}

