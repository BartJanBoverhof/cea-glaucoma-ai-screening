getQALYs <- function(a_trace, # cohort trace
                        v_utilities = v_utilities, # vector of utilities  
                        n_cycle_length = 1, # cycle length
                        age_init){

  # vector of utilities
  v_u <- c(no_glaucoma = v_utilities$healthy, 
            mild_diagnosed = v_utilities$mild,
            moderate_diagnosed = v_utilities$mod,
            severe_diagnosed = v_utilities$severe,
            mild_undiagnosed = v_utilities$mild,
            moderate_undiagnosed = v_utilities$mod,
            severe_undiagnosed = v_utilities$severe,
            blind = v_utilities$blind,
            observation = v_utilities$obs,
            death = v_utilities$death) * n_cycle_length

  v_qaly <- a_trace %*% v_u # multiply utilities with cohort trace

  #return qaly's (per patient)
  return(sum(v_qaly)/1000)
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
    cost_soc <- ( patients * p_soc) * screening_invitation 
    cost_low_risk <- (patients * p_low_risk) * (screening_invitation + fundus_photo + ai_costs)
    cost_high_risk <- (patients * p_high_risk) * (screening_invitation + fundus_photo + ai_costs)
    cost_compliant <- (patients * p_fully_compliant) * (screening_invitation + fundus_photo + ai_costs + ophthalmologist)
    screening_cost[i] <- cost_soc + cost_low_risk + cost_high_risk + cost_compliant # add cost to the list
  }
 
  # return costs per patient 
  return(sum(unlist(screening_cost)) / 1000)
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
  cost_latanopros <- sum(patients_latanopros) * medicine_costs$latanopros
  cost_timolol <- sum(patients_timolol) * medicine_costs$timolol
  cost_dorzolamide <- sum(patients_dorzolamide) * medicine_costs$dorzolamide
  cost_brimonidine <- sum(patients_brimonidine) * medicine_costs$brimonidine
  cost_pilocarpine <- sum(patients_pilocarpine) * medicine_costs$pilocarpine
  cost_mannitol <- sum(patients_mannitol) * medicine_costs$mannitol
  cost_diamox <- sum(patients_diamox) * medicine_costs$diamox

  # total yearly costs
  costs_total <- sum(cost_latanopros, cost_timolol, cost_dorzolamide, cost_brimonidine, cost_pilocarpine, cost_mannitol, cost_diamox)

  # return costs
  return(costs_total / 1000)
}


getCostsBurdenOfDisease <- function(costs, trace, societal_perspective) {
  
  # filter on visually impaired (vf) costs 
  costs_vi <- costs %>%
    filter(str_detect(type, "vi$")) 
  
  # filter on blind (blind) costs
  costs_blind <- costs %>%
    filter(str_detect(type, "blind$"))
  
  if (societal_perspective == FALSE) {
    # set all costs to 0
    costs_vi$informal_care_household <- 0
    costs_vi$informal_care_personal <- 0
    costs_vi$informal_care_communication <- 0
    costs_vi$informal_care_companionship <- 0
    costs_vi$transportation <- 0
  }
  
  # direct medical costs
  direct_med_vi <- costs_vi$general_practitioner + costs_vi$inpatient_services + costs_vi$physician + costs_vi$mobility_training + costs_vi$practical_skills + costs_vi$blind_aids + costs_vi$communication_aids + costs_vi$vision_aids + costs_vi$measuring_devices # direct medical costs visually impaired
  direct_med_blind <- costs_blind$general_practitioner + costs_blind$inpatient_services + costs_blind$physician + costs_blind$mobility_training + costs_blind$practical_skills + costs_blind$blind_aids + costs_blind$communication_aids + costs_blind$vision_aids + costs_blind$measuring_devices # direct medical costs blind

  # direct non-medical costs
  direct_nonmed_vi <- costs_vi$transportation + costs_vi$home_care_household + costs_vi$home_care_household + costs_vi$home_care_personal + costs_vi$informal_care_household + costs_vi$informal_care_personal + costs_vi$informal_care_communication + costs_vi$informal_care_companionship # direct non-medical costs visually impaired
  direct_nonmed_blind <- costs_blind$transportation + costs_blind$home_care_household + costs_blind$home_care_household + costs_blind$home_care_personal + costs_blind$informal_care_household + costs_blind$informal_care_personal + costs_blind$informal_care_communication + costs_blind$informal_care_companionship # direct non-medical costs blind
  
  # total number of patients
  patients_severe_treated <- sum(trace[,"Severe treated"]) 
  patients_severe_untreated <- sum(trace[,"Severe untreated"])
  patients_blind <- sum(trace[,"Blind"])
  
  # total costs
  costs_severe_treated <- ((direct_med_vi + direct_nonmed_vi) * patients_severe_treated) / 1000
  costs_severe_untreated <- ((direct_med_vi + direct_nonmed_vi) * patients_severe_untreated) / 1000
  costs_blind <- ((direct_med_blind + direct_nonmed_blind ) * patients_blind) / 1000
  total <- sum(costs_severe_treated, costs_severe_untreated, costs_blind)
  
  # Return the result
  return(list(costs_severe_treated = costs_severe_treated, costs_severe_untreated = costs_severe_untreated, costs_blind = costs_blind, total = total))
}

getProductivityCosts <- function(costs, traces, age_inits) {
  
  # filter on visually impaired (vf) costs 
  costs_vi <- costs %>%
    filter(str_detect(type, "vi$")) 
  
  # filter on blind (blind) costs
  costs_blind <- costs %>%
    filter(str_detect(type, "blind$"))
  
  # initialize variables  
  pension_age <- 67 # dutch pension age
  age_inits <- age_inits[age_inits < pension_age] # removing ages from the list that are higher than pension age
  patients_severe_treated <- rep(0, length(age_inits))
  patients_severe_untreated <- rep(0, length(age_inits))
  patients_blind <- rep(0, length(age_inits))

  # calculate total number of patients for each entry in traces
  for (i in 1:length(age_inits)) {
    trace <- traces[[i]]
    patients_severe_treated[i] <- sum(trace[,"Severe treated"][1: (pension_age - age_inits[i])])
    patients_severe_untreated[i] <- sum(trace[,"Severe untreated"][1: (pension_age - age_inits[i])])
    patients_blind[i] <- sum(trace[,"Blind"][1: (pension_age - age_inits[i])])
  }
  
  # total costs
  costs_severe_treated <- ((costs_vi$productivity_absent + costs_vi$productivity_disability) * sum(patients_severe_treated)) / 1000 
  costs_severe_untreated <- ((costs_vi$productivity_absent + costs_vi$productivity_disability) * sum(patients_severe_untreated)) / 1000
  costs_blind <- ((costs_blind$productivity_absent + costs_blind$productivity_disability) * sum(patients_blind)) / 1000
  total <- sum(costs_severe_treated, costs_severe_untreated, costs_blind)
  
  return(list(costs_severe_treated = costs_severe_treated, costs_severe_untreated = costs_severe_untreated, costs_blind = costs_blind, total = total))
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

  # discount

  # Return the result
  return(sum(costs_obs, costs_mild, costs_mod, costs_sev, costs_blind)/1000)
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

    # discount

    # return the result
    return(sum(costs_obs, costs_mild, costs_mod, costs_sev, costs_blind)/1000)
  }
