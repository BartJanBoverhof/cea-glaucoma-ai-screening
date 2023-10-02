# data loading
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
  
  return(total_cohort)
}

# cohort per decision tree arm
getCohortArm <- function(total_cohort, dt){
  
  return(list(
    mild = dt$p_path_mild * total_cohort$total_population,
    moderate = dt$p_path_mod * total_cohort$total_population,
    severe = dt$p_path_severe * total_cohort$total_population,
    blind = dt$p_path_blind * total_cohort$total_population,
    observation = dt$p_path_obs * total_cohort$total_population,
    false_pos = dt$p_path_fp * total_cohort$total_population,
    soc = dt$p_path_soc * total_cohort$total_population,
    soc_healthier = dt$p_path_soc_healthier * total_cohort$total_population,
    soc_sicker = dt$p_path_soc_sicker * total_cohort$total_population
  ))
}

# function to calculate mortality
CalculateMortality <- function(df_mortality, # mortality data
                               start_age) { # start age of the cohort, should be above 50 years
  # Filter, transform and reshape the mortality data
  v_r_mort_by_age <- df_mortality %>%
    filter(Attribute == "Total male and female") %>%
    filter(!(Age %in% c("Total of all ages", "40 to 45 years", "45 to 50 years"))) %>% 
    filter(start_age <= as.numeric(gsub("[^0-9]", "", Age))) %>%  # Filter by start_age
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
  
  # remove rows that are lower than start_age
  v_r_mort_by_age <- v_r_mort_by_age %>%
    filter(year >= start_age)
  
  
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

  
  return(v_p_mortality)
}

