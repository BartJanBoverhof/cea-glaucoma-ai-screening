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

  # annual decrement factor
  factor <- (1-decrement)

  # scale by the cycle length 
  v_u    <- c(Healthy  = u_healthy, 
              Mild = u_mild, 
              Moderate = u_moderate, 
              Severe  = u_severe,
              Blind = u_blind,
              Observation = u_observation,
              Death = u_death) * n_cycle_length

  # apply decrement
  # loop over all ages (i.e. rows in trace)
  for (i in 1:nrow(a_trace)-1) {
    a_trace[i+1, ] <- a_trace[i+1, ] * factor # in row i, multiply all utilities with the discount factor
    factor <- factor - decrement # update factor
  }
  

  # apply state reward
  #v_qaly <- a_trace %*% v_u # sum the utilities of all states for each cycle

  #return the vector of qaly's
  return(v_u)
  
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
 


