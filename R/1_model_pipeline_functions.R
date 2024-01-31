# function to load and prepare mortality data
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
calculateIncidence <- function(incidences,
                               age_start,
                               age_max,
                               cycle_length =1){
  v_incidences_modified <- incidences %>%  
    separate(Age, into = c("start_age", "end_age"), sep = "-", remove = FALSE) %>%
    mutate(
          start_age = as.numeric(gsub("[^0-9]", "", start_age)),
          end_age = as.numeric(gsub("[^0-9]", "", end_age))
        ) %>%
        rowwise() %>%
        mutate(year = list(seq(from = start_age, to = end_age - 1))) %>%
        unnest(year) %>%
        select(year, Incidence)

  # remove rows that are lower than start_age and higher than end_age
  v_incidences_modified_by_age <- v_incidences_modified %>%
    filter(year >= age_start & year <= age_max)

  # age-specific transition rates to the dead state for all cycles 
  v_incidences_modified_by_age  <- rep(v_incidences_modified_by_age$Incidence, each = 1/cycle_length)  
  
  # generate a sequence of ages 
  v_age <- seq(from = age_start, to = age_max-1, by = 1/cycle_length)

  # name the ages in the vector
  names(v_incidences_modified_by_age) <- v_age

  return (v_incidences_modified_by_age)
}


# function to calculate average age of the cohort
getMeanAge <- function(df_mortality, # mortality data
                             age_start, # start age of the cohort
                             age_max # maximum age of the cohort
                             ){
  start_age <- age_start
  end_age <- age_max

  df_mean_age <- df_mortality %>% #to calculate the midpoint age
    filter(Attribute == "Total male and female") %>%
    filter(!(Age %in% c("Total of all ages", "40 to 45 years", "45 to 50 years"))) %>% 
    filter(start_age <= as.numeric(gsub("[^0-9]", "", Age))) %>%  # Filter by start_age
    separate(Age, into = c("lower_bound", "upper_bound"), sep = " to ") %>%
    mutate(
      lower_bound = as.numeric(gsub("[^0-9]", "", lower_bound)),
      upper_bound = as.numeric(gsub("[^0-9]", "", gsub(" years", "", upper_bound))),  # Remove ' years' then keep only numbers
      midpoint_age = (lower_bound + upper_bound) / 2) %>%
    filter(lower_bound >= start_age & upper_bound <= end_age)
    
   
  weighted_sum <- sum(df_mean_age$midpoint_age * df_mean_age$`Average population`) # total years per in the cohort
  total_population <- sum(df_mean_age$`Average population`) # total population in the cohort
  mean_age <- weighted_sum / total_population # mean age of the cohort
  
  return(mean_age)
}
