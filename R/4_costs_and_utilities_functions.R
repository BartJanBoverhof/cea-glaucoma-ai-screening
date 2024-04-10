getTimeSpent <- function(a_trace){
  # get the time spent in each state
  time_spent <- colSums(a_trace)
  return(time_spent)
}

getBlindnessPrevented <- function(a_trace_ai, a_trace_soc){
  
  # get the number of blindness prevented
  blind_ai <- sum(a_trace_ai[,"Blind"])
  blind_soc <- sum(a_trace_soc[,"Blind"])
  blind_prevented <- blind_soc - blind_ai

  return(blind_prevented)
}

getQALYs <- function(a_trace, # cohort trace
                      v_utilities){ # vector of utilities

  n_cycle_length <- 1

  # vector of utilities
  v_u <- c(no_glaucoma = v_utilities$healthy, 
            mild_diagnosed = v_utilities$mild_treated,
            moderate_diagnosed = v_utilities$mod_treated,
            severe_diagnosed = v_utilities$severe_treated,
            mild_undiagnosed = v_utilities$mild_untreated,
            moderate_undiagnosed = v_utilities$mod_untreated,
            severe_undiagnosed = v_utilities$severe_untreated,
            blind = v_utilities$blind,
            observation = v_utilities$obs,
            death = v_utilities$death) * n_cycle_length

  v_qaly <- a_trace %*% v_u # multiply utilities with cohort trace

  #return qaly's 
  return(sum(v_qaly))
}

getScreenignDescriptives <- function(trace,
                                    screening_probabilities, # probabilities to end up in the different DT arms
                                    screening_cost, # screening cost
                                    interval, # screening interval (years)
                                    max_repititions # maximum number of screening repititions
                                    ){
                                    
  row_indices <- seq(from = 1, by = interval, length.out = max_repititions+1)  
  column_names <- c("No glaucoma", "Mild untreated", "Moderate untreated", "Severe untreated", "Observation") # people to sum in the 
  screened_patients <- list() # create an empty list
  p_fully_compliant <- screening_probabilities$p_fully_compliant

  for (i in seq_along(row_indices)) {
    
    if (row_indices[i] == 1){ # if dealing with the first row
      patients <- sum(trace[i,]) # if the first cycle, take the first rowsum

    } else {
       patients <- sum(trace[row_indices[i], column_names]) # for other cycles, only screen non-diagnosed patients
    }                
    
    # determining screens
    screened_patients[i] <- (patients * p_fully_compliant)
    }
  
  # screening summary
  total_screens <- (sum(unlist(screened_patients)))

  return(total_screens)
}
                                                
getScreeningCosts <- function(trace, 
                              screening_probabilities, # probabilities to end up in the different DT arms
                              screening_cost, # screening cost
                              interval, # screening interval (years)
                              max_repititions # maximum number of screening repititions
                              ){
  # save objects
  screening_invitation <- screening_cost$screening_invitation
  fundus_photo <- screening_cost$fundus_photo
  ai_costs <- screening_cost$ai_costs
  ophthalmologist <- screening_cost$ophthalmologist

  p_soc <- screening_probabilities$p_soc
  p_low_risk <- screening_probabilities$p_low_risk
  p_high_risk <- screening_probabilities$p_high_risk
  p_fully_compliant <- screening_probabilities$p_fully_compliant

  # creating indices for calculating screening costs
  row_indices <- seq(from = 1, by = interval, length.out = max_repititions+1)  
  column_names <- c("No glaucoma", "Mild untreated", "Moderate untreated", "Severe untreated", "Observation") # people to sum in the 
  screening_cost <- list() # create an empty list

  for (i in seq_along(row_indices)) {
    
    if (row_indices[i] == 1){ # if dealing with the first row
      patients <- sum(trace[i,]) # if the first cycle, take the first rowsum
    } else {
       patients <- sum(trace[row_indices[i], column_names]) # for other cycles, only screen non-diagnosed patients
    }

    # determining  costs
    cost_soc <- (patients * p_soc) * screening_invitation 
    cost_low_risk <- (patients * p_low_risk) * (screening_invitation + fundus_photo + ai_costs)
    cost_high_risk <- (patients * p_high_risk) * (screening_invitation + fundus_photo + ai_costs)
    cost_compliant <- (patients * p_fully_compliant) * (screening_invitation + fundus_photo + ai_costs + ophthalmologist)
    screening_cost[i] <- cost_soc + cost_low_risk + cost_high_risk + cost_compliant # add cost to the list
  }

  cost <- unlist(screening_cost)

  # return costs
  return(sum(cost))
}
 
getMedicineCosts <- function(a_trace, # cohort trace
                             medicine_costs, # medicine costs
                             medicine_utilisation){ # medicine utilisation

  # save objects of medicine costs
  price_latanopros <- medicine_costs$latanopros
  price_timolol <- medicine_costs$timolol
  price_dorzolamide <- medicine_costs$dorzolamide
  price_brimonidine <- medicine_costs$brimonidine
  price_pilocarpine <- medicine_costs$pilocarpine
  price_mannitol <- medicine_costs$mannitol
  price_diamox <- medicine_costs$diamox

  # total number of patient years in each state
  patients_mild <- sum(a_trace[,"Mild treated"])
  patients_moderate <- sum(a_trace[,"Moderate treated"])
  patients_severe <- sum(a_trace[,"Severe treated"])
  patients_blind <- sum(a_trace[,"Blind"])

  v_patients <- c(mild = patients_mild,
                  moderate = patients_moderate,
                  severe = patients_severe,
                  blind = patients_blind)

  # multiply the total patients with medicine utilisation
  patients_latanopros <- medicine_utilisation$latanopros * v_patients
  patients_timolol <- medicine_utilisation$timolol * v_patients
  patients_dorzolamide <- medicine_utilisation$dorzolamide * v_patients
  patients_brimonidine <- medicine_utilisation$brimonidine * v_patients
  patients_pilocarpine <- medicine_utilisation$pilocarpine * v_patients
  patients_mannitol <- medicine_utilisation$mannitol * v_patients
  patients_diamox <- medicine_utilisation$diamox * v_patients
  
  # total costs total for each medicine
  cost_latanopros <- (patients_latanopros) * medicine_costs$latanopros
  cost_timolol <- (patients_timolol) * medicine_costs$timolol
  cost_dorzolamide <- (patients_dorzolamide) * medicine_costs$dorzolamide
  cost_brimonidine <- (patients_brimonidine) * medicine_costs$brimonidine
  cost_pilocarpine <- (patients_pilocarpine) * medicine_costs$pilocarpine
  cost_mannitol <- (patients_mannitol) * medicine_costs$mannitol
  cost_diamox <- (patients_diamox) * medicine_costs$diamox

  # total yearly costs
  costs <- cost_latanopros + cost_timolol + cost_dorzolamide + cost_brimonidine + cost_pilocarpine + cost_mannitol + cost_diamox
  
  # return costs
  return(sum(costs))
}

getDiagnosticCosts <- function(trace, diagnostics_cost) {
  
  # total patients
  patients_observation <- sum(trace[,"Observation"]) #observation state
  patients_mild <- sum(trace[,"Mild treated"]) 
  patients_moderate <- sum(trace[,"Moderate treated"])
  patients_severe <- sum(trace[,"Severe treated"])
  patients_blind <- sum(trace[,"Blind"])
  
  # calculating weighted price
  # observation state
  costs_obs <- diagnostics_cost %>%
    filter(str_detect(item, "_obs$")) %>%
    transmute(real_price = as.numeric(average_best) * price) %>%
    transmute(weighted_price = real_price * patients_observation)
  
  # mild state
  costs_mild <- diagnostics_cost %>%
    filter(str_detect(item, "_mild$")) %>%
    transmute(real_price = as.numeric(average_best) * price) %>%
    transmute(weighted_price = real_price * patients_mild) 

  # moderate state
  costs_mod <- diagnostics_cost %>%
    filter(str_detect(item, "_mod$")) %>%
    transmute(real_price = as.numeric(average_best) * price) %>%
    transmute(weighted_price = real_price * patients_moderate)

  # severe state
  costs_sev <- diagnostics_cost %>%
    filter(str_detect(item, "_sev$")) %>%
    transmute(real_price = as.numeric(average_best) * price) %>%
    transmute(weighted_price = real_price * patients_severe)

  # blind state
  costs_blind <- diagnostics_cost %>%
    filter(str_detect(item, "_blind$")) %>%
    transmute(real_price = as.numeric(average_best) * price) %>%
    transmute(weighted_price = real_price * patients_blind)     
  
  # Return the result
  return(sum(costs_obs, costs_mild, costs_mod, costs_sev, costs_blind))
}

getInterventionCosts <- function(trace, intervention_cost){
    
    # total patients
    patients_observation <- sum(trace[,"Observation"]) #observation state
    patients_mild <- sum(trace[,"Mild treated"]) 
    patients_moderate <- sum(trace[,"Moderate treated"])
    patients_severe <- sum(trace[,"Severe treated"])
    patients_blind <- sum(trace[,"Blind"])
    
    # calculating weighted price
    # observation state
    costs_obs <- intervention_cost %>%
      filter(str_detect(item, "_obs$")) %>%
      transmute(real_price = as.numeric(average_best) * price) %>%
      transmute(weighted_price = real_price * patients_observation)

    # mild state
    costs_mild <- intervention_cost %>%
      filter(str_detect(item, "_mild")) %>%
      transmute(real_price = as.numeric(average_best) * price) %>%
      transmute(weighted_price = real_price * patients_mild)

    # moderate state
    costs_mod <- intervention_cost %>%
      filter(str_detect(item, "_mod")) %>%
      transmute(real_price = as.numeric(average_best) * price) %>%
      transmute(weighted_price = real_price * patients_moderate)

    # severe state
    costs_sev <- intervention_cost %>%
      filter(str_detect(item, "_sev")) %>%
      transmute(real_price = as.numeric(average_best) * price) %>%
      transmute(weighted_price = real_price * patients_severe)

    # blind state
    costs_blind <- intervention_cost %>%
      filter(str_detect(item, "_blind")) %>%
      transmute(real_price = as.numeric(average_best) * price) %>%
      transmute(weighted_price = real_price * patients_blind)

    return(sum(costs_obs, costs_mild, costs_mod, costs_sev, costs_blind))
  }

getCostsBurdenOfDisease <- function(costs, trace, societal_perspective) {
  
  # filter on visually impaired (vf) costs 
  costs_vi_mod <- costs %>%
    filter(str_detect(type, "vi_mod$")) 
  
  # filter on blind (blind) costs
  costs_vi_sev <- costs %>%
    filter(str_detect(type, "vi_sev$"))
  
  if (societal_perspective == FALSE) {
    # set all costs to 0
    costs_vi$informal_care_household <- 0
    costs_vi$informal_care_personal <- 0
    costs_vi$informal_care_communication <- 0
    costs_vi$informal_care_companionship <- 0
    costs_vi$transportation <- 0
  }
  
  # direct medical costs
  direct_med_vi_mod <- costs_vi_mod$general_practitioner + costs_vi_mod$other + costs_vi_mod$inpatient_services + costs_vi_mod$physician + costs_vi_mod$mobility_training + costs_vi_mod$practical_skills + costs_vi_mod$blind_aids + costs_vi_mod$communication_aids + costs_vi_mod$vision_aids + costs_vi_mod$measuring_devices # direct medical costs visually impaired
  
  direct_med_vi_sev <- costs_vi_sev$general_practitioner + costs_vi_sev$other + costs_vi_sev$inpatient_services + costs_vi_sev$physician + costs_vi_sev$mobility_training + costs_vi_sev$practical_skills + costs_vi_sev$blind_aids + costs_vi_sev$communication_aids + costs_vi_sev$vision_aids + costs_vi_sev$measuring_devices # direct medical costs blind

  # direct non-medical costs
  direct_nonmed_vi_mod <- costs_vi_mod$transportation + costs_vi_mod$home_care_household + costs_vi_mod$home_care_household + costs_vi_mod$home_care_personal + costs_vi_mod$informal_care_household + costs_vi_mod$informal_care_personal + costs_vi_mod$informal_care_communication + costs_vi_mod$informal_care_companionship # direct non-medical costs visually impaired
  
  direct_nonmed_vi_sev <- costs_vi_sev$transportation + costs_vi_sev$home_care_household + costs_vi_sev$home_care_household + costs_vi_sev$home_care_personal + costs_vi_sev$informal_care_household + costs_vi_sev$informal_care_personal + costs_vi_sev$informal_care_communication + costs_vi_sev$informal_care_companionship # direct non-medical costs blind
  
  # total number of patients
  patients_severe_treated <- sum(trace[,"Severe treated"]) 
  patients_severe_untreated <- sum(trace[,"Severe untreated"])
  patients_blind <- sum(trace[,"Blind"])
  
  # total costs
  costs_severe_treated <- ((direct_med_vi_mod + direct_nonmed_vi_mod) * patients_severe_treated) 
  costs_severe_untreated <- ((direct_med_vi_mod + direct_nonmed_vi_mod) * patients_severe_untreated) 
  costs_blind <- ((direct_med_vi_sev + direct_nonmed_vi_sev) * patients_blind)
  
  # Return the result
  return(sum(costs_severe_treated, costs_severe_untreated, costs_blind))
}

getProductivityCosts <- function(costs, trace, age_init) {
  
  pension_age <- 67 #rounded
  average_age <- age_init + 2

  # filter on visually impaired (vf) costs 
  costs_vi_mod <- costs %>%
    filter(str_detect(type, "vi_mod$")) 
  
  # filter on blind (blind) costs
  costs_vi_sev <- costs %>%
    filter(str_detect(type, "vi_sev$"))

  # calculate total number of patients for each entry in trace
  patients_severe_treated <- sum(trace[,"Severe treated"][1: (pension_age - average_age)])
  patients_severe_untreated <- sum(trace[,"Severe untreated"][1: (pension_age - average_age)])
  patients_blind <- sum(trace[,"Blind"][1: (pension_age - average_age)])  
  
  # total costs
  costs_severe_treated <- ((costs_vi_mod$productivity_absent + costs_vi_mod$productivity_disability) * sum(patients_severe_treated))
  costs_severe_untreated <- ((costs_vi_mod$productivity_absent + costs_vi_mod$productivity_disability) * sum(patients_severe_untreated)) 
  costs_blind <- ((costs_vi_sev$productivity_absent + costs_vi_sev$productivity_disability) * sum(patients_blind)) 
  
  return(sum(costs_severe_treated, costs_severe_untreated, costs_blind))
}
