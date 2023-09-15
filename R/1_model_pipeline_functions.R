# data loading
get_cohort <- function(data, 
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
get_cohort_arm <- function(total_cohort, dt){
  
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
