## Isaac: as previously mentioned, this code lacks comments. I propose using a standardized approach. I've used ChatGPT for example to provide comments using the following format. Please feel free to adjust.

# Name of the function: getTimeSpent
# Purpose: Calculate the total time spent in each state
# Inputs:  a_trace - a matrix or data frame with the proportion of time spent in each health state
# Outputs: A vector with the sum of each column of the matrix or data frame
# Do half cycle correction here!
getTimeSpent <- function(a_trace){
  time_spent <- colSums(a_trace)
  return(time_spent)
  }

# Name of the function: getBlindnessPrevented
# Purpose of the code: Calculate the number of blindness cases prevented by AI compared to SoC
# Inputs: a_trace_ai - AI intervention trace, a_trace_soc - standard of care trace
# Outputs: Number of blindness cases prevented
# Do half cycle correction here!
getBlindnessPrevented <- function(a_trace_ai, a_trace_soc){
  blind_ai <- sum(a_trace_ai[,"Blind"]) # Isaac: we already got this from getTimeSpent. Maybe we dont need to input the trace and get the sum again. Seems faster, but not sure how much time it can save. Low priority.
  blind_soc <- sum(a_trace_soc[,"Blind"])
  blind_prevented <- blind_soc - blind_ai
  return(blind_prevented)
}


# Name of the function: getQALYs
# Purpose of the code: Calculate the total QALYs (Quality-Adjusted Life Years) for a given Markov trace
# Inputs: a_trace - cohort trace matrix, v_utilities - vector of utility values for different health states (we need to explaiun why these are vectors and not single values: age effect already included?)
# Outputs: Total QALYs (consider returning QALYs per health state as well - I think htat would be v_qaly)
getQALYs <- function(a_trace){ # vector of utilities

  #return qaly's 
  return(sum(a_trace))
}

# Isaac: I stopped here since these functions are more complex
getScreenignDescriptives <- function(trace, 
                              screening_probabilities, # probabilities to end up in the different DT arms
                              interval, # screening interval (years)
                              max_repititions # maximum number of screening repititions
                              ){
  # save objects
  p_invited <- screening_probabilities$p_invited
  p_ai_screened <- screening_probabilities$p_ai_screened
  p_followed_up <- screening_probabilities$p_followed_up

  # creating indices for calculating screening costs
  row_indices <- seq(from = 1, by = interval, length.out = max_repititions+1)  
  column_names <- c("No glaucoma", "Mild untreated", "Moderate untreated", "Severe untreated", "Observation", "Blind") # people to sum in the 
  
  invited_people <- list() # create an empty list
  screened_people  <- list()
  followed_up_people  <- list()

  for (i in seq_along(row_indices)) {
    
    if (row_indices[i] == 1){ # if the first cycle, take the first rowsum
      patients <- sum(trace[i,])
    } else {
       patients <- sum(trace[row_indices[i]-1, column_names]) # for other cycles, only screen non-diagnosed patients
    }

    # determining  costs
    invited_people[i] <- (patients * p_invited)
    screened_people[i] <- (patients * p_ai_screened)
    followed_up_people[i] <- (patients * p_followed_up)
  }

  invited_people <- sum(unlist(invited_people))
  screened_people <- sum(unlist(screened_people))
  followed_up_people <- sum(unlist(followed_up_people))
  
  # return costs
  return(list(invited_people = invited_people, screened_people = screened_people, followed_up_people = followed_up_people))
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

  p_invited <- screening_probabilities$p_invited
  p_ai_screened <- screening_probabilities$p_ai_screened
  p_followed_up <- screening_probabilities$p_followed_up

  # creating indices for calculating screening costs
  row_indices <- seq(from = 1, by = interval, length.out = max_repititions+1)  
  column_names <- c("No glaucoma", "Mild untreated", "Moderate untreated", "Severe untreated", "Observation", "Blind") # people to sum in the 
  screening_cost <- list() # create an empty list

  for (i in seq_along(row_indices)) {
    
    if (row_indices[i] == 1){ # if the first cycle, take the first rowsum
      patients <- sum(trace[i,])
    } else {
       patients <- sum(trace[row_indices[i]-1, column_names]) # for other cycles, only screen non-diagnosed patients
    }

    # determining  costs
    cost_invitation <- (patients * p_invited) * screening_invitation 
    cost_ai_screened <- (patients * p_ai_screened) * (fundus_photo + ai_costs)
    cost_followed_up <- (patients * p_followed_up) * ophthalmologist
    screening_cost[i] <- cost_invitation + cost_ai_screened + cost_followed_up # add cost to the list
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

  # total costs
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

getInterventionCosts <- function(trace, intervention_cost, p_transition, v_incidence_of, v_incidence_screening, age_init, interval, max_repititions, strategy){
    

    ### DETERMINE PATIENTS THAT RECEIVE TREATMENT ###
    age_max <- 100
    
    # drop age categories before and after initial age
    v_incidence_of <- v_incidence_of[(age_init-49):(age_max-50)]
    v_incidence_screening <- v_incidence_screening[(age_init-49):(age_max-50)]

    # mild patients
    mild_obs <- sum(trace[c(2:nrow(trace)),"Observation"] * v_incidence_screening)
    mild_untreated <- sum(trace[c(2:nrow(trace)),"Mild untreated"] * v_incidence_of)
    mild_treated <- list()
    
    if (strategy == "ai") # if strategy is ai, also include the screened participants
      for (rep in 1:max_repititions) { # for loop to calculate the amount of people that become untreated after screening
        mild_treated[rep] <- trace[(rep*interval),"Mild untreated"] - trace[(rep*interval)+1,"Mild untreated"]
      }
    
    # calculate all mild patients that receive treatment
    mild_patients <- sum(mild_obs, mild_untreated, unlist(mild_treated), trace[1,"Mild treated"])

    trace[,"Mild treated"]

    # moderate patients 
    mod_mild_treated <- sum(trace[c(2:nrow(trace)),"Mild treated"] * p_transition$p_mild_mod_treated)
    mod_mod_untreated <- sum(trace[c(2:nrow(trace)),"Moderate untreated"] * v_incidence_of)
    mod_treated <- list()

    if (strategy == "ai") # if strategy is ai, also include the screened participants
      for (rep in 1:max_repititions) { # for loop to calculate the amount of people that become untreated after screening
        mod_treated[rep] <- trace[(rep*interval),"Moderate untreated"] - trace[(rep*interval)+1,"Moderate untreated"]
      }

    # calculate all moderate patients that receive treatment
    mod_patients <- sum(mod_mild_treated, mod_mod_untreated, unlist(mod_treated), trace[1,"Moderate treated"])
    
    
    # severe patients
    sev_mod_treated <- sum(trace[c(2:nrow(trace)),"Moderate treated"] * p_transition$p_mod_sev_treated)
    sev_sev_untreated <- sum(trace[c(2:nrow(trace)),"Severe untreated"] * v_incidence_of)
    sev_treated <- list()

    if (strategy == "ai") # if strategy is ai, also include the screened participants
      for (rep in 1:max_repititions) { # for loop to calculate the amount of people that become untreated after screening
        sev_treated[rep] <- trace[(rep*interval),"Severe untreated"] - trace[(rep*interval)+1,"Severe untreated"]
      }
    
    # calculate all severe patients that receive treatment
    sev_patients <- sum(sev_mod_treated, sev_sev_untreated, unlist(sev_treated), trace[1,"Severe treated"])

    # blind patients
    blind_sev_treated <- sum(trace[c(2:nrow(trace)),"Severe treated"] * p_transition$p_sev_blind_treated)
    blind_blind_untreated <- sum(trace[c(2:nrow(trace)),"Blind"] * v_incidence_of)

    # calculate all blind patients that receive treatment
    blind_patients <- sum(blind_sev_treated, blind_blind_untreated, trace[1,"Blind"])
    
    # determine the share of patients that receive laser treatment 
    share_laser_mild <- intervention_cost %>%filter(str_detect(item, "laser_mild$")) %>% transmute(average_best) %>% as.numeric() * mild_patients
    share_laser_mod <- intervention_cost %>%filter(str_detect(item, "laser_mod$")) %>% transmute(average_best) %>% as.numeric() * mod_patients
    share_laser_sev <- intervention_cost %>%filter(str_detect(item, "laser_sev$")) %>% transmute(average_best) %>% as.numeric() * sev_patients
    share_laser_blind <- intervention_cost %>%filter(str_detect(item, "laser_vi$")) %>% transmute(average_best) %>% as.numeric() * blind_patients

    # determine the share of patients that receive surgery
    share_surgery_mild <- intervention_cost %>%filter(str_detect(item, "surgery_mild$")) %>% transmute(average_best) %>% as.numeric() * mild_patients
    share_surgery_mod <- intervention_cost %>%filter(str_detect(item, "surgery_mod$")) %>% transmute(average_best) %>% as.numeric() * mod_patients
    share_surgery_sev <- intervention_cost %>%filter(str_detect(item, "surgery_sev$")) %>% transmute(average_best) %>% as.numeric() * sev_patients
    share_surgery_blind <- intervention_cost %>%filter(str_detect(item, "surgery_vi$")) %>% transmute(average_best) %>% as.numeric() * blind_patients

    ### DETERMINE COSTS
    # determine the costs of laser treatment
    costs_laser_mild <- intervention_cost %>% filter(str_detect(item, "laser_treatment")) %>% transmute(price) * share_laser_mild
    costs_laser_mod <- intervention_cost %>% filter(str_detect(item, "laser_treatment")) %>% transmute(price) * share_laser_mod
    costs_laser_sev <- intervention_cost %>% filter(str_detect(item, "laser_treatment")) %>% transmute(price) * share_laser_sev
    costs_laser_blind <- intervention_cost %>% filter(str_detect(item, "laser_treatment")) %>% transmute(price) * share_laser_blind

    # determine the costs of surgery
    costs_surgery_mild <- intervention_cost %>% filter(str_detect(item, "surgery_treatment")) %>% transmute(price) * share_surgery_mild
    costs_surgery_mod <- intervention_cost %>% filter(str_detect(item, "surgery_treatment")) %>% transmute(price) * share_surgery_mod
    costs_surgery_sev <- intervention_cost %>% filter(str_detect(item, "surgery_treatment")) %>% transmute(price) * share_surgery_sev
    costs_surgery_blind <- intervention_cost %>% filter(str_detect(item, "surgery_treatment")) %>% transmute(price) * share_surgery_blind

    total_laser <- sum(costs_laser_mild, costs_laser_mod, costs_laser_sev, costs_laser_blind)
    total_surgery <- sum(costs_surgery_mild, costs_surgery_mod, costs_surgery_sev, costs_surgery_blind)

    return(sum(total_laser, total_surgery))
  }

getCostsBurdenOfDisease <- function(costs, trace, societal_perspective) {
  
  # filter on visually impaired (vf) costs 
  #costs_vi_mod <- costs %>%
  #  filter(str_detect(type, "vi_mod$")) 
  
  # filter on blind (blind) costs
  costs_vi_sev <- costs 
  
  if (societal_perspective == FALSE) {
    # set all costs to 0
    costs_vi$informal_care_household <- 0
    costs_vi$informal_care_personal <- 0
    costs_vi$informal_care_communication <- 0
    costs_vi$informal_care_companionship <- 0
    costs_vi$transportation <- 0
  }
  
  # direct medical costs
  #direct_med_vi_mod <- costs_vi_mod$general_practitioner + costs_vi_mod$other + costs_vi_mod$inpatient_services + costs_vi_mod$physician + costs_vi_mod$mobility_training + costs_vi_mod$practical_skills + costs_vi_mod$blind_aids + costs_vi_mod$communication_aids + costs_vi_mod$vision_aids + costs_vi_mod$measuring_devices # direct medical costs visually impaired
  
  direct_med_vi_sev <- costs_vi_sev$general_practitioner + costs_vi_sev$other + costs_vi_sev$inpatient_services + costs_vi_sev$physician + costs_vi_sev$mobility_training + costs_vi_sev$practical_skills + costs_vi_sev$blind_aids + costs_vi_sev$communication_aids + costs_vi_sev$vision_aids + costs_vi_sev$measuring_devices # direct medical costs blind

  # direct non-medical costs
  #direct_nonmed_vi_mod <- costs_vi_mod$transportation + costs_vi_mod$home_care_household + costs_vi_mod$home_care_household + costs_vi_mod$home_care_personal + costs_vi_mod$informal_care_household + costs_vi_mod$informal_care_personal + costs_vi_mod$informal_care_communication + costs_vi_mod$informal_care_companionship # direct non-medical costs visually impaired
  direct_nonmed_vi_sev <- costs_vi_sev$transportation + costs_vi_sev$home_care_household + costs_vi_sev$home_care_household + costs_vi_sev$home_care_personal + costs_vi_sev$informal_care_household + costs_vi_sev$informal_care_personal + costs_vi_sev$informal_care_communication + costs_vi_sev$informal_care_companionship # direct non-medical costs blind
  
  # total number of patients
  #patients_severe_treated <- sum(trace[,"Severe treated"]) 
  #patients_severe_untreated <- sum(trace[,"Severe untreated"])
  patients_blind <- sum(trace[,"Blind"])
  
  # total costs
  #costs_severe_treated <- ((direct_med_vi_mod + direct_nonmed_vi_mod) * patients_severe_treated) 
  #costs_severe_untreated <- ((direct_med_vi_mod + direct_nonmed_vi_mod) * patients_severe_untreated) 
  costs_blind <- ((direct_med_vi_sev + direct_nonmed_vi_sev) * patients_blind)
  
  # Return the result
  return(sum(costs_blind))
  #  return(sum(costs_severe_treated, costs_severe_untreated, costs_blind))
}

getProductivityCosts <- function(costs, trace, age_init) {
    
  # filter on blind (blind) costs
  costs_vi_sev <- costs 

  # calculate total number of patients for each entry in trace
  patients_blind <- sum(trace[,"Blind"][1: (pension_age - age_init)])  
  
  # total costs
  costs_blind <- ((costs_vi_sev$productivity_absent + costs_vi_sev$productivity_disability) * sum(patients_blind)) 
  
  return(sum(costs_blind))
}
