# decision tree AI scenario 
getDtProbabilitiesAI <- function(p_dt_ai, p_severity_undiagnosed, cohort_strategy = c("AI Screening", "Standard of care"), visualize = TRUE){
  
  # read in fixed probabilities
  p_prevalence <- p_dt$prevalence 
  p_screen_compliance <- p_dt$screen_comp    # screening compliance
  p_screen_sensitivity <- p_dt$ai_sens       # screening sensitivity 
  p_screen_specificity <- p_dt$ai_spec       # screening specificity 
  p_referral_compliance <- p_dt$ref_comp     # referral compliance
  p_referral_sensitivity <- p_dt$doct_sens   # referral sensitivity. Currently not used and assumed to be 1. 
  p_referral_specificity <- p_dt$doct_sens   # referral sensitivity. Currently not used and assumed to be 1. 
  
  p_severity_mild <- p_severity_undiagnosed$undiagnosed_mild
  p_severity_mod <- p_severity_undiagnosed$undiagnosed_mod
  p_severity_severe <- p_severity_undiagnosed$undiagnosed_severe
  p_severity_blind <- p_severity_undiagnosed$undiagnosed_blind
  p_observation <- p_severity_undiagnosed$undiagnosed_observation
  
  # calculate required probabilities
  ai_tp <- p_dt$ai_sens * p_dt$prevalence  # true positives
  ai_fp <- (1-p_dt$ai_spec) * (1-p_dt$prevalence) # false positives
  ai_tn <- p_dt$ai_spec * (1-p_dt$prevalence) # true negatives
  ai_fn <- (1-p_dt$ai_sens) * p_dt$prevalence # false negatives
  
  ai_positive <- ai_tp + ai_fp # positive test result
  ai_negative <- ai_tn + ai_fn # negative test result
  
  ai_ppv <- ai_tp / ai_positive # positive predictive value
  ai_npv <- ai_tn / ai_negative # negative predictive value
  
  ### 2. Decision tree weights
  p_path_mild <- p_screen_compliance * ai_positive * p_referral_compliance * ai_ppv * p_severity_mild # path mild
  p_path_mod <- p_screen_compliance * ai_positive * p_referral_compliance * ai_ppv * p_severity_mod # path moderate
  p_path_severe <- p_screen_compliance * ai_positive * p_referral_compliance * ai_ppv * p_severity_severe # path severe
  p_path_blind <- p_screen_compliance * ai_positive * p_referral_compliance * ai_ppv * p_severity_blind # path blind
  p_path_obs <- p_screen_compliance * ai_positive * p_referral_compliance * p_observation  # path observation
  p_path_fp <- p_screen_compliance * ai_positive * p_referral_compliance * (1-ai_ppv - p_observation) # path healthy (false positives)
  
  p_path_soc <- 1-p_screen_compliance # path non-compliant with screening
  p_path_soc_healthier <- p_screen_compliance * ai_negative # path negative screening result
  p_path_soc_sicker <- p_screen_compliance * ai_positive * (1-p_referral_compliance) # path non-compliant with clinical assessment
  
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
    p_path_mild = p_path_mild,
    p_path_mod = p_path_mod,
    p_path_severe = p_path_severe,
    p_path_blind = p_path_blind,
    p_path_obs = p_path_obs,
    p_path_fp = p_path_fp,
    p_path_soc = p_path_soc,
    p_path_soc_healthier = p_path_soc_healthier,
    p_path_soc_sicker = p_path_soc_sicker,
    p_cumulative = sum(p_path_mild, p_path_mod,p_path_severe,p_path_blind,p_path_obs, p_path_fp, p_path_soc, p_path_soc_healthier, p_path_soc_sicker)
  )
  
  # Conditionally append the graph object to the return list
  if (visualize) {
    results$graph <- graph
  }
  
  return(results)
}


# function to obtain DT probabilities SoC
getDtProbabilitiesSoC <- function(incidences, p_severity)

  {}
