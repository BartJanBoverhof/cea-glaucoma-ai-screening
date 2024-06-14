# decision tree AI scenario 
getStartDistAI <- function(probabilities, severity_distribution, arm, visualize = TRUE, model_compliance, p_prevalence, cohort){
  
  # read in fixed probabilities
  p_screen_compliance <-  probabilities$screen_comp      # screening compliance
  p_screen_sensitivity <- probabilities$ai_sens       # screening sensitivity 
  p_screen_specificity <- probabilities$ai_spec       # screening specificity 
  p_referral_compliance <- probabilities$ref_comp   # referral compliance
  
  p_severity_mild <- severity_distribution$mild
  p_severity_mod <- severity_distribution$moderate
  p_severity_severe <- severity_distribution$severe
  p_severity_blind <- severity_distribution$blind
  p_observation <- severity_distribution$observation
  
  if (model_compliance == FALSE){
    p_screen_compliance <-  1     # screening compliance
    p_referral_compliance <- 1   # referral compliance
  } else if (model_compliance == TRUE) {
    p_screen_compliance <-  probabilities$screen_comp # screening compliance
    referral_compliance <- probabilities$ref_comp   # referral compliance
  }

  # calculate required probabilities
  ai_tp <- p_screen_sensitivity* p_prevalence  # true positives
  ai_fp <- (1-p_screen_specificity) * (1-p_prevalence) # false positives
  ai_tn <- p_screen_specificity * (1-p_prevalence) # true negatives
  ai_fn <- (1-p_screen_sensitivity) * p_prevalence # false negatives

  ai_positive <- ai_tp + ai_fp # positive test result
  ai_negative <- ai_tn + ai_fn # negative test result
  
  ai_ppv <- ai_tp / ai_positive # positive predictive value
  ai_npv <- ai_tn / ai_negative # negative predictive value
  
  # for the non-compliant to screening cohort (similair to soc)
  if(arm == "soc"){
    
    # decision tree path probabilities
    p_path_no_glaucoma <- (1 - p_screen_compliance) * (1-p_prevalence)  # path no glaucoma
    p_path_obs <- 0 # path observation assumed 0

    p_path_mild_diagnosed <- 0 # path mild diagnosed
    p_path_mod_diagnosed <- 0 # path moderate diagnosed
    p_path_severe_diagnosed <- 0 # path severe diagnosed

    p_path_mild_undiagnosed <-(1 - p_screen_compliance) * p_prevalence * p_severity_mild # path mild
    p_path_mod_undiagnosed <- (1 - p_screen_compliance) * p_prevalence * p_severity_mod # path moderate
    p_path_severe_undiagnosed <- (1 - p_screen_compliance) * p_prevalence * p_severity_severe # path severe
    p_path_blind <- (1 - p_screen_compliance) * p_prevalence * p_severity_blind # path blind
  }

  # for the compliant, but negative and not referred (low risk)
  else if (arm == "low_risk"){
    
    # decision tree path probabilities
    p_path_no_glaucoma <- p_screen_compliance * ai_negative * ai_npv # path healthy (false positives)
    p_path_obs <- 0 # path observation assumed 0

    p_path_mild_diagnosed <- 0 # path mild diagnosed
    p_path_mod_diagnosed <- 0 # path moderate diagnosed
    p_path_severe_diagnosed <- 0 # path severe diagnosed

    p_path_mild_undiagnosed <- p_screen_compliance * ai_negative * (1-ai_npv) * p_severity_mild # path mild
    p_path_mod_undiagnosed <- p_screen_compliance * ai_negative * (1-ai_npv) * p_severity_mod # path moderate
    p_path_severe_undiagnosed <- p_screen_compliance * ai_negative * (1-ai_npv) * p_severity_severe # path severe
    p_path_blind <- p_screen_compliance * ai_negative * (1-ai_npv) * p_severity_blind # path blind
  }

  # for the non-compliant to referral cohort
  else if(arm == "high_risk"){
    
    # decision tree path probabilities
    p_path_no_glaucoma <- p_screen_compliance * ai_positive * (1-p_referral_compliance) * (1-ai_ppv) # path healthy (false positives)
    p_path_obs <- 0  # path observation assumed 0

    p_path_mild_diagnosed <- 0 # path mild diagnosed
    p_path_mod_diagnosed <- 0 # path moderate diagnosed
    p_path_severe_diagnosed <- 0 # path severe diagnosed

    p_path_mild_undiagnosed <- p_screen_compliance * ai_positive * (1-p_referral_compliance) * ai_ppv * p_severity_mild # path mild
    p_path_mod_undiagnosed <- p_screen_compliance * ai_positive * (1-p_referral_compliance) * ai_ppv * p_severity_mod # path moderate
    p_path_severe_undiagnosed <- p_screen_compliance * ai_positive * (1-p_referral_compliance) * ai_ppv * p_severity_severe # path severe
    p_path_blind <- p_screen_compliance * ai_positive * (1-p_referral_compliance) * ai_ppv * p_severity_blind # path blind
  }

  # for the compliant cohort
  if(arm == "compliant"){
    
    # decision tree path probabilities
    p_path_no_glaucoma <- p_screen_compliance * ai_positive * p_referral_compliance * (1-ai_ppv - p_observation) # path healthy (false positives)
    p_path_obs <- p_screen_compliance * ai_positive * p_referral_compliance * p_observation # path observation
    
    p_path_mild_diagnosed <- p_screen_compliance * ai_positive * p_referral_compliance * ai_ppv * p_severity_mild # path mild
    p_path_mod_diagnosed <- p_screen_compliance * ai_positive * p_referral_compliance * ai_ppv  * p_severity_mod # path moderate
    p_path_severe_diagnosed <- p_screen_compliance * ai_positive * p_referral_compliance * ai_ppv * p_severity_severe # path severe
    p_path_blind <- p_screen_compliance * ai_positive * p_referral_compliance * ai_ppv * p_severity_blind # path blind

    p_path_mild_undiagnosed <- 0 # path mild undiagnosed
    p_path_mod_undiagnosed <- 0 # path moderate undiagnosed
    p_path_severe_undiagnosed <- 0 # path severe undiagnosed
  }

  # plot decision tree
  if (visualize) {
    graph <- grViz("
    digraph decision_tree {

    graph [rankdir = \"LR\", ranksep=\"1\", nodesep=\"0.5\"];
    
    # General node styles
    node [fontname=\"Arial\", fontsize=20, shape=\"box\", style=\"rounded,filled\", fillcolor=\"lightgray\", color=\"black\"];

    # Specific node style for 'Population'
    Z [shape=\"box\", style=\"filled\"];  # Here 'Z' is changed to a square shape
    
    # General edge styles
    edge [fontname=\"Arial\", fontsize=14, color=\"gray\", penwidth=1.5];
      
      # Nodes
      Z [label='Population']
      A [label='Compliance']
      B [label='AI Result']
      C [label='Compliance']
      D [label='Clinical Assesment']
      E [label='Mild']
      F [label='Moderate']
      G [label='Severe']
      H [label='Blind']
      I [label='Observation']
      J [label='SoC']
      K [label='SoC healthier']
      L [label='SoC sicker']
      M [label='SoC']
      N [label='Healthy']
    
  
      # Edges
      Z -> A [label='AI Screening']
      Z -> M [label='SoC']
      A -> B [label='Compliant']
      A -> J [label='Non-compliant']
      B -> C [label='Positive']
      B -> K [label='Negative']
      C -> D [label='Compliant']
      C -> L [label='Non-compliant']
      D -> E [label='Glaucoma']
      D -> F [label='Glaucoma']
      D -> G [label='Glaucoma']
      D -> H [label='Glaucoma']
      D -> I [label='Non-definitive decision']
      D -> N [label='False positives']

      }
    ")
  }
  
  # Create the return list
  results <- list(
    p_path_no_glaucoma = p_path_no_glaucoma,
    p_path_obs = p_path_obs,
    p_path_mild_diagnosed = p_path_mild_diagnosed,
    p_path_mod_diagnosed = p_path_mod_diagnosed,
    p_path_severe_diagnosed = p_path_severe_diagnosed,
    p_path_mild_undiagnosed = p_path_mild_undiagnosed,
    p_path_mod_undiagnosed = p_path_mod_undiagnosed,
    p_path_severe_undiagnosed = p_path_severe_undiagnosed,
    p_path_blind = p_path_blind
    )
  
  # Conditionally append the graph object to the return list
  if (visualize) {
    results$graph <- graph
  }
  
  return(p_dt = results)
}

CombineDT <- function(traces) {
  
  starting_probabilities <- mapply(sum, traces$soc , traces$low_risk , traces$high_risk, traces$compliant, SIMPLIFY = FALSE)
  total_sum <- sum(unlist(starting_probabilities))

  print(paste("The decision tree arms sum to a total of", total_sum))

  return(starting_probabilities) 
}

# function to obtain DT probabilities SoC
getStartDistSoc <- function(probabilities, severity_distribution, visualize = TRUE, p_prevalence, cohort){

  p_severity_mild <- severity_distribution$mild
  p_severity_mod <- severity_distribution$moderate
  p_severity_severe <- severity_distribution$severe
  p_severity_blind <- severity_distribution$blind
  p_observation <- severity_distribution$observation
  
  # decision tree path probabilities
  p_path_no_glaucoma <- (1-p_prevalence)  # path no glaucoma
  p_path_obs <- 0 # path observation assumed 0

  p_path_mild_diagnosed <- 0 # path mild diagnosed
  p_path_mod_diagnosed <- 0 # path moderate diagnosed
  p_path_severe_diagnosed <- 0 # path severe diagnosed

  p_path_mild_undiagnosed <- p_prevalence * p_severity_mild # path mild
  p_path_mod_undiagnosed <- p_prevalence * p_severity_mod # path moderate
  p_path_severe_undiagnosed <- p_prevalence * p_severity_severe # path severe
  p_path_blind <- p_prevalence * p_severity_blind # path blind

  # plot decision tree
  if (visualize) {
    graph <- grViz("
    digraph decision_tree {

    graph [rankdir = \"LR\", ranksep=\"1\", nodesep=\"0.5\"];
    
    # General node styles
    node [fontname=\"Arial\", fontsize=20, shape=\"box\", style=\"rounded,filled\", fillcolor=\"lightgray\", color=\"black\"];

    # Specific node style for 'Population'
    Z [shape=\"box\", style=\"filled\"];  # Here 'Z' is changed to a square shape
    
    # General edge styles
    edge [fontname=\"Arial\", fontsize=14, color=\"gray\", penwidth=1.5];
      
      # Nodes
      Z [label='Population']
      A [label='Compliance']
      B [label='AI Result']
      C [label='Compliance']
      D [label='Clinical Assesment']
      E [label='Mild']
      F [label='Moderate']
      G [label='Severe']
      H [label='Blind']
      I [label='Observation']
      J [label='SoC']
      K [label='SoC healthier']
      L [label='SoC sicker']
      M [label='SoC']
      N [label='Healthy']
    
  
      # Edges
      Z -> A [label='AI Screening']
      Z -> M [label='SoC']
      A -> B [label='Compliant']
      A -> J [label='Non-compliant']
      B -> C [label='Positive']
      B -> K [label='Negative']
      C -> D [label='Compliant']
      C -> L [label='Non-compliant']
      D -> E [label='Glaucoma']
      D -> F [label='Glaucoma']
      D -> G [label='Glaucoma']
      D -> H [label='Glaucoma']
      D -> I [label='Non-definitive decision']
      D -> N [label='False positives']

      }
    ")
  }
  
  # Create the return list
  results <- list(
    p_path_no_glaucoma = p_path_no_glaucoma,
    p_path_obs = p_path_obs,
    p_path_mild_diagnosed = p_path_mild_diagnosed,
    p_path_mod_diagnosed = p_path_mod_diagnosed,
    p_path_severe_diagnosed = p_path_severe_diagnosed,
    p_path_mild_undiagnosed = p_path_mild_undiagnosed,
    p_path_mod_undiagnosed = p_path_mod_undiagnosed,
    p_path_severe_undiagnosed = p_path_severe_undiagnosed,
    p_path_blind = p_path_blind
  )
  
  # Conditionally append the graph object to the return list
  if (visualize) {
    results$graph <- graph
  }
  
  return(results)
}