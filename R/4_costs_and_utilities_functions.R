getQALYs <- function(a_trace, # cohort trace
                        v_utilities = v_utilities, # vector of utilities  
                        age_decrement, # annual discount rate for utliities
                        n_cycle_length = 1, # cycle length
                        discount_rate){
  
  # obtain the utilities for each state
  u_no_glaucoma <- v_utilities$healthy
  u_mild <- v_utilities$mild
  u_moderate <- v_utilities$mod
  u_severe <- v_utilities$severe
  u_blind <- v_utilities$blind
  u_observation <- v_utilities$obs
  u_death <- v_utilities$death

  # discount factor
  v_u <- c(no_glaucoma = u_no_glaucoma, 
            mild_diagnosed = u_mild,
            moderate_diagnosed = u_moderate,
            severe_diagnosed = u_severe,
            mild_undiagnosed = u_mild,
            moderate_undiagnosed = u_moderate,
            severe_undiagnosed = u_severe,
            blind = u_blind,
            observation = u_observation,
            death = 0 ) * n_cycle_length

  # apply age decrement
  # loop over amount of cycles (i.e. rows in trace)
  for (i in 1:(nrow(a_trace)-1)) {
    
    age_factor <- (1 + age_decrement)^-i # define / update factor
    discount_factor <- (1 + discount_rate)^-i # define / update factor
    
    a_trace[i+1, ] <- a_trace[i+1, ] * age_factor # in row i, multiply all utilities with the discount factor
    a_trace[i+1, ] <- a_trace[i+1, ] * discount_factor # in row i, multiply all utilities with the discount factor
  }
  
  # recode negative values to 0
  a_trace[a_trace < 0] <- 0

  # multiply utilities with cohort trace
  v_qaly <- a_trace %*% v_u # sum the utilities of all states for each cycle

  #return the vector of qaly's
  return(sum(v_qaly))
  
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
