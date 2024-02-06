getQALYs <- function(a_trace, # cohort trace
                        v_utilities = v_utilities, # vector of utilities  
                        age_decrement, # annual discount rate for utliities
                        n_cycle_length = 1, # cycle length
                        discount_rate,
                        age_init,
                        age_max){

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

  # assuming utility_age_decrement is a data.frame and "age" is one of its columns
  age_decrement_vector <- subset(age_decrement, age >= age_init & age <= age_max)
  age_decrement_vector <- as.vector(age_decrement_vector[,"dutch_utility_decrement"]) # vector of age utility decrements 50-100 

  # age correction
  a_trace_corrected <- sweep(a_trace, 1, age_decrement_vector$dutch_utility_decrement, "*")

  # multiply utilities with cohort trace
  v_qaly <- a_trace_corrected %*% v_u # sum the utilities of all states for each cycle

  # discount
  discount_vector <- 1 - discount_rate * (0:(nrow(a_trace_corrected) - 1))
  v_qaly_discounted <-  v_qaly * discount_vector
  #a_trace_correctedd <- sweep(a_trace_corrected, 1, discount_vector, "*")

  #return the vector of qaly's
  return(sum(v_qaly_discounted))
  
}

getScreeningCosts <- function(a_trace_ai_soc,  # cohort trace of the patients non-compliant with AI screening
                              a_trace_ai_low_risk, # cohort trace of the patients with negaive AI result
                              a_trace_ai_high_risk, # cohort trace of the patients non-compliant with referral 
                              a_trace_ai_compliant, # cohort trace of the patients compliant with screening and referral
                              screening_cost # screening cost
                              ){
  # save objects of screening costs
  screening_invitation <- screening_cost$screening_invitation
  fundus_photo <- screening_cost$fundus_photo
  ai_costs <- screening_cost$ai_costs
  ophthalmologist <- screening_cost$ophthalmologist
  
  ### noncompliant with screening
  # total number of patients non-compliant with screening
  patients_soc <- sum(a_trace_ai_soc[1,])

  # costs total. Non-compliant screening patients receive only invitation costs.
  costs_soc <- screening_invitation * patients_soc


  ### negative result
  # total number of patients with negative AI result
  patients_low_risk <- sum(a_trace_ai_low_risk[1,])

  # costs total. Negative AI result patients receive only invitation costs.
  costs_low_risk <- screening_invitation * patients_low_risk


  ### non-compliant with referral
 # total number of patients non-compliant with referral
  patients_high_risk <- sum(a_trace_ai_high_risk[1,])
  
# costs total. Non-compliant referral patients receive invitation costs, AI costs and fundus photo costs.
  costs_high_risk <- screening_invitation * patients_high_risk + ai_costs * patients_high_risk + fundus_photo * patients_high_risk

  ### compliant
  # total number of patients compliant with screening and referral
  patients_compliant <- sum(a_trace_ai_compliant[1,])

  # costs total. Compliant patients receive invitation costs, AI costs, fundus photo costs and ophthalmologist costs.
  costs_compliant <- screening_invitation * patients_compliant + ai_costs * patients_compliant + fundus_photo * patients_compliant + ophthalmologist * patients_compliant

  # total costs
  costs_total <- sum(costs_soc, costs_low_risk, costs_high_risk, costs_compliant)

  # return costs
  return(costs_total)
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

  # total number of patients in each state
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
  return(costs_total)
}

getVisuallyImpairedCosts <- function(v_cost_visually_impaired, a_trace) {
  
  # direct medical costs
  direct_med_physician <- v_cost_visually_impaired$direct_med_physician
  direct_med_inpatient <- v_cost_visually_impaired$direct_med_inpatient
  direct_med_non_physician <- v_cost_visually_impaired$direct_med_non_physician
  direct_med_devices <- v_cost_visually_impaired$direct_med_devices

  # direct non-medical costs
  direct_nonmed_home <- v_cost_visually_impaired$direct_nonmed_home
  direct_nonmed_informal <- v_cost_visually_impaired$direct_nonmed_informal
  direct_nonmed_transportation <- v_cost_visually_impaired$direct_nonmed_transportation

  # productivity costs
  productivity_absent <- v_cost_visually_impaired$productivity_absent
  productivity_disability <- v_cost_visually_impaired$productivity_disability

  # total number of patients
  patients_severe <- sum(a_trace[,"Severe treated"]) + sum(a_trace[,"Severe untreated"]) 

  # total costs
  direct_med <- (direct_med_physician + direct_med_inpatient + direct_med_non_physician + direct_med_devices) * patients_severe
  direct_nonmed <- (direct_nonmed_home + direct_nonmed_informal + direct_nonmed_transportation) * patients_severe
  productivity <- (productivity_absent + productivity_disability) * patients_severe
  total <- sum(direct_med, direct_nonmed, productivity)

  # Return the result
  return(total)
}

getVisuallyImpairedCosts <- function(costs, trace) {
  
  # direct medical costs
  direct_med_physician <- costs$direct_med_physician
  direct_med_inpatient <- costs$direct_med_inpatient
  direct_med_non_physician <- costs$direct_med_non_physician
  direct_med_devices <- costs$direct_med_devices

  # direct non-medical costs
  direct_nonmed_home <- costs$direct_nonmed_home
  direct_nonmed_informal <- costs$direct_nonmed_informal
  direct_nonmed_transportation <- costs$direct_nonmed_transportation

  # productivity costs
  productivity_absent <- v_cost_visually_impaired$productivity_absent
  productivity_disability <- v_cost_visually_impaired$productivity_disability

  # total number of patients
  patients <- sum(trace[,"Severe treated"]) + sum(trace[,"Severe untreated"]) 

  # total costs
  direct_med <- (direct_med_physician + direct_med_inpatient + direct_med_non_physician + direct_med_devices) * patients
  direct_nonmed <- (direct_nonmed_home + direct_nonmed_informal + direct_nonmed_transportation) * patients
  productivity <- (productivity_absent + productivity_disability) * patients
  total <- sum(direct_med, direct_nonmed, productivity)

  # Return the result
  return(total)
}

getBlindCosts <- function(costs, trace) {
  
  # direct medical costs
  direct_med_physician <- costs$direct_med_physician
  direct_med_inpatient <- costs$direct_med_inpatient
  direct_med_non_physician <- costs$direct_med_non_physician
  direct_med_devices <- costs$direct_med_devices

  # direct non-medical costs
  direct_nonmed_home <- costs$direct_nonmed_home
  direct_nonmed_informal <- costs$direct_nonmed_informal
  direct_nonmed_transportation <- costs$direct_nonmed_transportation

  # productivity costs
  productivity_absent <- v_cost_visually_impaired$productivity_absent
  productivity_disability <- v_cost_visually_impaired$productivity_disability

  # total number of patients
  patients <- sum(trace[,"Blind"])

  # total costs
  direct_med <- (direct_med_physician + direct_med_inpatient + direct_med_non_physician + direct_med_devices) * patients
  direct_nonmed <- (direct_nonmed_home + direct_nonmed_informal + direct_nonmed_transportation) * patients
  productivity <- (productivity_absent + productivity_disability) * patients
  total <- sum(direct_med, direct_nonmed, productivity)

  # Return the result
  return(total)
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

    # discount

    # return the result
    return(sum(costs_obs, costs_mild, costs_mod, costs_sev, costs_blind))
  }
