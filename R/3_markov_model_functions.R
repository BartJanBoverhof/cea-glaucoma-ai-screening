#------------------------------------------------------------------------------#
####                         Decision Tree                                 ####
#------------------------------------------------------------------------------#
get_dt_probabilities <- function(dt_ai, severity_undiagnosed, strategy = c("AI Screening", "Standard of care"), visualize = TRUE){
  
  # Read in fixed probabilities
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
  
  # Calculate required probabilities
  # PPR formula =    True positives                      + False positives
  p_ppr_ai <- p_prevalence * p_screen_sensitivity + (1-p_prevalence) * (1-p_screen_specificity) #Predicted positive rate screening
  p_npr_ai <- p_prevalence * (1-p_screen_sensitivity) + (1-p_prevalence) * p_screen_specificity #Predicted negative rate screening
  
  p_ppr_clinician <- p_prevalence * p_referral_sensitivity + (1-p_prevalence) * (1-p_referral_specificity) #Predicted positive rate screening
  p_npr_clinician <- p_prevalence * (1-p_referral_sensitivity) + (1-p_prevalence) * p_referral_specificity #Predicted negative rate screening
  
  
  ### 2. Decision tree weights
  p_path_mild <- p_screen_compliance * p_ppr_ai * p_referral_compliance * p_ppr_clinician * p_severity_mild #Path mild
  p_path_mod <- p_screen_compliance * p_ppr_ai * p_referral_compliance * p_ppr_clinician * p_severity_mod #Path moderate
  p_path_severe <- p_screen_compliance * p_ppr_ai * p_referral_compliance * p_ppr_clinician * p_severity_severe #Path severe
  p_path_blind <- p_screen_compliance * p_ppr_ai * p_referral_compliance * p_ppr_clinician * p_severity_blind #Path blind
  
  p_path_neg_ref <- p_screen_compliance * p_ppr_ai * p_referral_compliance * p_npr_clinician  #Path where clinician is negative
  p_path_nocomp_ref <- p_screen_compliance * p_ppr_ai * (1-p_referral_compliance) #Path where patient is not compliant with referral
  p_path_neg_ai <- p_screen_compliance * p_npr_ai  #Path where screening is negative
  p_path_nocomp_ai <- (1-p_screen_compliance)  #Path where patients is not compliant with screening
  
  #Plot decision tree
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
    path_mild = p_path_mild,
    path_mod = p_path_mod,
    path_severe = p_path_severe,
    path_blind = p_path_blind,
    path_neg_ref = p_path_neg_ref,
    path_nocomp_ref = p_path_nocomp_ref,
    path_neg_ai = p_path_neg_ai,
    path_nocomp_ai = p_path_nocomp_ai,
    cumulative_prob = sum(p_path_mild, p_path_mod,p_path_severe,p_path_blind,p_path_neg_ref,p_path_nocomp_ref,p_path_neg_ai,p_path_nocomp_ai)
  )
  
  # Conditionally append the graph object to the return list
  if (visualize) {
    results$graph <- graph
  }
  
  return(results)
}






