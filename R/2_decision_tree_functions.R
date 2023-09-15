# decision tree
get_dt_probabilities <- function(dt_ai, severity_undiagnosed, strategy = c("AI Screening", "Standard of care"), visualize = TRUE){
  
  # read in fixed probabilities
  p_prevalence <- dt_ai$prevalence 
  p_screen_compliance <- dt_ai$screen_comp    # screening compliance
  p_screen_sensitivity <- dt_ai$ai_sens       # screening sensitivity 
  p_screen_specificity <- dt_ai$ai_spec       # screening specificity 
  p_referral_compliance <- dt_ai$ref_comp     # referral compliance
  p_referral_sensitivity <- dt_ai$doct_sens   # referral sensitivity
  p_referral_specificity <- dt_ai$doct_sens   # referral sensitivity
  
  p_severity_mild <- severity_undiagnosed$undiagnosed_mild
  p_severity_mod <- severity_undiagnosed$undiagnosed_mod
  p_severity_severe <- severity_undiagnosed$undiagnosed_severe
  p_severity_blind <- severity_undiagnosed$undiagnosed_blind
  
  # calculate required probabilities
  ai_positive <- 0.05 # placeholder value
  ai_negative <- 1- ai_positive # placeholder value
  
  p_ppv <- p_prevalence * p_screen_sensitivity + (1-p_prevalence) * (1-p_screen_specificity) # predicted positive rate screening
  p_npv <- p_prevalence * (1-p_screen_sensitivity) + (1-p_prevalence) * p_screen_specificity # predicted negative rate screening
  
  ### 2. Decision tree weights
  p_path_mild <- p_screen_compliance * ai_positive * p_referral_compliance * p_ppv * p_severity_mild # path mild
  p_path_mod <- p_screen_compliance * ai_positive * p_referral_compliance * p_ppv * p_severity_mod # path moderate
  p_path_severe <- p_screen_compliance * ai_positive * p_referral_compliance * p_ppv * p_severity_severe # path severe
  p_path_blind <- p_screen_compliance * ai_positive * p_referral_compliance * p_ppv * p_severity_blind # path blind
  p_path_obs <- 0  # path observation
  p_path_fp <- p_screen_compliance * ai_positive * p_referral_compliance * (1-p_ppv) # path healthy (false positives)
  
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
    cumulative_prob = sum(p_path_mild, p_path_mod,p_path_severe,p_path_blind,p_path_obs, p_path_fp, p_path_soc, p_path_soc_healthier, p_path_soc_sicker)
  )
  
  # Conditionally append the graph object to the return list
  if (visualize) {
    results$graph <- graph
  }
  
  return(results)
}





