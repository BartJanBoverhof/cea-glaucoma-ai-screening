VisualiseTrace <- function(markov_trace){
  
  # prerequisites
  age_init = 50 # initial age
  age_max = 100 # maximum age
  cycle_length <- 1
  n_cycles <- (age_max - age_init)/cycle_length # time horizon, number of cycles
  
  # labels of age vectors
  v_age_names  <- paste(rep(age_init:(age_max-1), each = 1/cycle_length), 
                        1:(1/cycle_length), 
                        sep = ".")
  
  # markov model states
  v_names_states <- c("Healthy",       
                      "Mild", 
                      "Moderate", 
                      "Severe", 
                      "Blind", 
                      #"Observation",
                      "Death")
  
  n_states <- length(v_names_states)   # number of health states 
  
  
  plot_trace(markov_trace[,-ncol(markov_trace)])
}