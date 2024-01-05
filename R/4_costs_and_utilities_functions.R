getUtilities <- function(a_trace, # cohort trace
                        v_utilities = v_utilities, # vector of utilities  
                        decrement, # annual discount rate for utliities
                        n_cycle_length = 1 # cycle length
                        ){
  
  # obtain the utilities for each state
  u_healthy <- v_utilities$healthy
  u_mild <- v_utilities$mild
  u_moderate <- v_utilities$mod
  u_severe <- v_utilities$severe
  u_blind <- v_utilities$blind
  u_observation <- v_utilities$obs
  u_death <- v_utilities$death

  # scale by the cycle length 
  v_u    <- c(Healthy  = u_healthy, 
              Mild = u_mild, 
              Moderate = u_moderate, 
              Severe  = u_severe,
              Blind = u_blind,
              Observation = u_observation,
              Death = u_death) * n_cycle_length

  # apply decrement
  v_u <- a_trace * (1 - decrement)

  # apply state reward
  #v_qaly <- a_trace %*% v_u # sum the utilities of all states for each cycle

  #return the vector of qaly's
  return(v_u)
  
}

getScreeningCosts <- function(a_trace_ai_noncompliant_screen, # cohort trace of the patients non-compliant with AI screening
                              a_trace_ai_noncompliant_referral, # cohort trace of the patients non-compliant with clinical assessment
                              a_trace_ai_compliant, # cohort trace of the patients compliant with AI screening and clinical assessment
                              screening_cost # screening cost
                              ){
  # save objects of screening costs
  screening_invitation <- screening_cost$screening_invitation
  fundus_photo <- screening_cost$fundus_photo
  ai_costs <- screening_cost$ai_costs
  ophthalmologist <- screening_cost$ophthalmologist
  
  ### noncompliant with screening
  # total number of patients non-compliant with screening
  patients_noncompliant_screen <- sum(a_trace_ai_noncompliant_screen[1,])

  # costs total. Non-compliant screening patients receive only invitation costs.
  costs_noncompliant_screen <- screening_invitation * patients_noncompliant_screen


  ### noncompliant with referral
  # total number of patients non-compliant with referral
  patients_noncompliant_referral <- sum(a_trace_ai_noncompliant_referral[1,])

  # costs total. Non-compliant referral patients receive invitation costs, funsus photo costs, and AI costs.
  costs_noncompliant_referral <- (screening_invitation + fundus_photo + ai_costs) * patients_noncompliant_referral


  ### compliant 
  # total number of fully compliant patients 
  patients_compliant <- sum(a_trace_ai_compliant[1,])
  
  # costs total. Compliant patients receice all costs.
  costs_compliant <- (fundus_photo + screening_invitation + ai_costs + ophthalmologist) * patients_compliant
  
  # total costs
  costs_total <- costs_noncompliant_screen + costs_noncompliant_referral + costs_compliant

  # return costs
  return(costs_total)
}
 


