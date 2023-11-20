getUtilities <- function(discount, # annual discount rate for utliities
                         utilities = v_utilities # vector of utilities  
                         ){
  
  # 07 State Rewards 

## Scale by the cycle length 
# Vector of state utilities under strategy SoC
v_u_ai    <- c(Healthy  = u_healthy, 
               Mild = u_mild, 
               Moderate = u_moderate, 
               Severe  = u_severe,
               Blind = u_blind,
               Observation = u_observation,
               Death = u_death) * cycle_length






  # discount rate
  d_e <- discount_qalys     # annual discount rate for QALYs
  
  # within-cycle correction
  v_wcc  <- gen_wcc(n_cycles = n_cycles, method = "Simpson1/3")
  
  # get costs
  c_healthy <- costs$c_healthy
  c_mild <- costs$c_mild
  c_moderate <- costs$c_moderate
  c_severe <- costs$c_severe
  c_blind <- costs$c_blind
  c_death <- costs$c_death
  
  # get utilities
  u_healthy <- utilities$u_healthy
  u_mild <- utilities$u_mild
  u_moderate <- utilities$u_moderate
  u_severe <- utilities$u_severe
  u_blind <- utilities$u_blind
  u_death <- utilities$u_death
  
  ### Discount weight for costs and effects 
  v_dwc   <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
  v_dwe   <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))
}

getCosts <- function(){
  
  # split to costs and utilities 
  # discount rates
  d_c <- discount_costs        # annual discount rate for costs 
  d_e <- discount_qalys     # annual discount rate for QALYs
  
  # within-cycle correction
  v_wcc  <- gen_wcc(n_cycles = n_cycles, method = "Simpson1/3")
  
  # get costs
  c_healthy <- costs$c_healthy
  c_mild <- costs$c_mild
  c_moderate <- costs$c_moderate
  c_severe <- costs$c_severe
  c_blind <- costs$c_blind
  c_death <- costs$c_death
  
  # get utilities
  u_healthy <- utilities$u_healthy
  u_mild <- utilities$u_mild
  u_moderate <- utilities$u_moderate
  u_severe <- utilities$u_severe
  u_blind <- utilities$u_blind
  u_death <- utilities$u_death
  
  ### Discount weight for costs and effects 
  v_dwc   <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
  v_dwe   <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))
}





