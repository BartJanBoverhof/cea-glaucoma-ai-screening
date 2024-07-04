runDSA <- function(parameter){

    strategy <- get("strategy", envir = parent.frame())

    # modify selected parameter
    if (parameter == "transition_untreated"){
        
        p_transition <- p_transition # local save

        # transfer paramaters to lower bound
        p_transition$p_mild_mod_untreated <- p_transition$p_mild_mod_untreated * (1 - p_transition$untreated_dev)
        p_transition$p_mod_sev_untreated <- p_transition$p_mod_sev_untreated * (1 - p_transition$untreated_dev)
        p_transition$p_sev_blind_untreated <- p_transition$p_sev_blind_untreated * (1 - p_transition$untreated_dev)

        lower <- callModel() # lower bound

        # reset variable from global variable
        p_transition <- get("p_transition", envir =globalenv())

        # transfer paramaters to upper bound
        p_transition$p_mild_mod_untreated <- p_transition$p_mild_mod_untreated * (1 + p_transition$untreated_dev)
        p_transition$p_mod_sev_untreated <- p_transition$p_mod_sev_untreated * (1 + p_transition$untreated_dev)
        p_transition$p_sev_blind_untreated <- p_transition$p_sev_blind_untreated * (1 + p_transition$untreated_dev)

        upper <- callModel() # lower bound

    } else if (parameter == "transition_treated"){
            
        # reset variable from global variable
        p_transition <- get("p_transition", envir =globalenv())

        # transfer paramaters to lower bound
        p_transition$p_mild_mod_treated <- p_transition$p_mild_mod_treated * (1 - p_transition$treated_dev)
        p_transition$p_mod_sev_treated <- p_transition$p_mod_sev_treated * (1 - p_transition$treated_dev)
        p_transition$p_sev_blind_treated <- p_transition$p_sev_blind_treated * (1 - p_transition$treated_dev)

        lower <- callModel() # lower bound

        # reset variable from global variable
        p_transition <- get("p_transition", envir =globalenv())

        # transfer paramaters to upper bound
        p_transition$p_mild_mod_treated <- p_transition$p_mild_mod_treated * (1 + p_transition$treated_dev)
        p_transition$p_mod_sev_treated <- p_transition$p_mod_sev_treated * (1 + p_transition$treated_dev)
        p_transition$p_sev_blind_treated <- p_transition$p_sev_blind_treated * (1 + p_transition$treated_dev)

        upper <- callModel() # lower bound
    
    } else if (parameter == "sensitivity"){
        
        p_dt <- p_dt # local save

        # transform parameters to lower bound
        p_dt$ai_sens <- p_dt$ai_sens * (1 - p_dt$ai_sens_dev)
        
        lower <- callModel() # lower bound

        # reset variable from global variable
        p_dt <- get("p_dt", envir =globalenv())

        # transform parameters to upper bound
        p_dt$ai_sens <- p_dt$ai_sens * (1 + p_dt$ai_sens_dev)
        if (p_dt$ai_sens > 1) {p_dt$ai_sens <- 1} # if higher than 1, make it 1

        upper <- callModel() # lower bound
    
    } else if (parameter == "specificity"){
    
        p_dt <- p_dt # local save

        # transform parameters to lower bound
        p_dt$ai_spec <- p_dt$ai_spec * (1 - p_dt$ai_spec_dev)

        lower <- callModel() # lower bound

        # reset variable from global variable
        p_dt <- get("p_dt", envir =globalenv())

        # transform parameters to upper bound
        p_dt$ai_spec <- p_dt$ai_spec * (1+ p_dt$ai_spec_dev)
        if (p_dt$ai_spec > 1) {p_dt$ai_spec <- 1} # if higher than 1, make it 1

        upper <- callModel() # lower bound

    } else if (parameter == "prevalence"){
    
        v_prevalence <- v_prevalence # local save

        # transform parameters to lower bound
        v_prevalence$prevalence <- v_prevalence$prevalence * (1 - v_prevalence$prevalence_dev)

        lower <- callModel() # lower bound

        # reset variable from global variable
        v_prevalence <- get("v_prevalence", envir =globalenv())

        # transform parameters to upper bound
        v_prevalence$prevalence <- v_prevalence$prevalence * (1 + v_prevalence$prevalence_dev)

        upper <- callModel() # lower bound

    } else if (parameter == "incidences_of"){

        v_incidences_of <- v_incidences_of # local save

        # transform parameters to lower bound
        v_incidences_of$incidence <- v_incidences_of$incidence * (1 - v_incidences_of$incidence_dev)
        
        lower <- callModel() # lower bound

        # reset variable from global variable
        v_incidences_of <- get("v_incidences_of", envir =globalenv())

        # transform parameters to upper bound
        v_incidences_of$incidence <- v_incidences_of$incidence * (1 + v_incidences_of$incidence_dev)

        upper <- callModel() # lower bound

    } else if (parameter == "incidences_screening"){
    
        v_incidences_screening <- v_incidences_screening # local save

        # transform parameters to lower bound
        v_incidences_screening$incidence <- v_incidences_screening$incidence * (1 - v_incidences_screening$incidence_dev)
        
        lower <- callModel() # lower bound

        # reset variable from global variable
        v_incidences_screening <- get("v_incidences_screening", envir =globalenv())

        # transform parameters to upper bound
        v_incidences_screening$incidence <- v_incidences_screening$incidence * (1 + v_incidences_screening$incidence_dev)

        upper <- callModel() # lower bound

    } else if (parameter == "utilities_untreated"){
    
        v_utilities <- v_utilities # local save

        # transform parameters to lower bound
        v_utilities$mild_untreated <- v_utilities$mild_untreated * (1 - v_utilities$utilities_dev)
        v_utilities$mod_untreated <- v_utilities$mod_untreated * (1 - v_utilities$utilities_dev)
        v_utilities$severe_untreated <- v_utilities$severe_untreated * (1 - v_utilities$utilities_dev)
        v_utilities$blind <- v_utilities$blind * (1 - v_utilities$utilities_dev)

        lower <- callModel() # lower bound

        # reset variable from global variable
        v_utilities <- get("v_utilities", envir =globalenv())

        # transform parameters to upper bound
        v_utilities$mild_untreated <- v_utilities$mild_untreated * (1 + v_utilities$utilities_dev)
        v_utilities$mod_untreated <- v_utilities$mod_untreated * (1 + v_utilities$utilities_dev)
        v_utilities$severe_untreated <- v_utilities$severe_untreated * (1 + v_utilities$utilities_dev)
        v_utilities$blind <- v_utilities$blind * (1 + v_utilities$utilities_dev)

        upper <- callModel() # lower bound

    } else if (parameter == "utilities_treated"){

        v_utilities <- v_utilities # local save

        # transform parameters to lower bound
        v_utilities$mild_treated <- v_utilities$mild_treated * (1 - v_utilities$utilities_dev)
        v_utilities$mod_treated <- v_utilities$mod_treated * (1 - v_utilities$utilities_dev)
        v_utilities$severe_treated <- v_utilities$severe_treated * (1 - v_utilities$utilities_dev)
        v_utilities$blind <- v_utilities$blind * (1 - v_utilities$utilities_dev)

        lower <- callModel() # lower bound

        # reset variable from global variable
        v_utilities <- get("v_utilities", envir =globalenv())

        # transform parameters to upper bound
        v_utilities$mild_treated <- v_utilities$mild_treated * (1 + v_utilities$utilities_dev)
        v_utilities$mod_treated <- v_utilities$mod_treated * (1 + v_utilities$utilities_dev)
        v_utilities$severe_treated <- v_utilities$severe_treated * (1 + v_utilities$utilities_dev)
        v_utilities$blind <- v_utilities$blind * (1 + v_utilities$utilities_dev)

        upper <- callModel() # lower bound

    } else if (parameter == "costs_screening"){

      cost_multiplier <- 0.8 # local save
      lower <- callModel() # lower bound

      cost_multiplier <- 1.2 # local save
      upper <- callModel() # lower bound

    } else if (parameter == "costs_medicine"){
    
      cost_multiplier <- 0.8 # local save
      lower <- callModel() # lower bound

      cost_multiplier <- 1.2 # local save
      upper <- callModel() # lower bound

    } else if (parameter == "costs_diagnostics"){
    
      cost_multiplier <- 0.8 # local save
      lower <- callModel() # lower bound

      cost_multiplier <- 1.2 # local save
      upper <- callModel() # lower bound

    } else if (parameter == "costs_intervention"){
    
      cost_multiplier <- 0.8 # local save
      lower <- callModel() # lower bound

      cost_multiplier <- 1.2 # local save
      upper <- callModel() # lower bound

    } else if (parameter == "costs_burden_disease"){
    
      cost_multiplier <- 0.8 # local save
      lower <- callModel() # lower bound

      cost_multiplier <- 1.2 # local save
      upper <- callModel() # lower bound

    } else if (parameter == "costs_productivity"){
    
      cost_multiplier <- 0.8 # local save
      lower <- callModel() # lower bound

      cost_multiplier <- 1.2 # local save
      upper <- callModel() # lower bound

    } else {
        print("Error: parameter not found")
    }
    
  return(c(lower, upper))
}

runPSA <- function(parameter){

    strategy <- get("strategy", envir = parent.frame())

    # local save for all parameters
    v_prevalence <- get("v_prevalence", envir =globalenv())
    v_incidences_of <- get("v_incidences_of", envir =globalenv())
    v_incidences_screening <- get("v_incidences_screening", envir =globalenv())
    p_dt <- get("p_dt", envir =globalenv())
    p_transition <- get("p_transition", envir =globalenv())

    # sample parameter values from distributions
    # prevalence
    mu_prevalence <- v_prevalence$prevalence
    sd_prevalence <- mu_prevalence * v_psa_sd$prevalence
    v_prevalence$prevalence <- sampleBeta(mu_prevalence, sd_prevalence)

    #incidence of screening
    mu_incidences_screening <- v_incidences_screening$incidence
    sd_incidences_screening <- mu_incidences_screening * v_psa_sd$incidence_screening
    v_incidences_screening$incidence <- sampleBeta(mu_incidences_screening, sd_incidences_screening)

    # incidence of OF
    mu_incidences_of <- v_incidences_of$incidence
    sd_incidences_of <- mu_incidences_of * v_psa_sd$incidence_of
    v_incidences_of$incidence <- sampleBeta(mu_incidences_of, sd_incidences_of)

    # sensitivity AI
    mu_sensitivity <- p_dt$ai_sens
    sd_sensitivity <- mu_sensitivity * v_psa_sd$ai_sensitivity
    p_dt$ai_sens <- sampleBeta(mu_sensitivity, sd_sensitivity)

    # specificity AI 
    mu_specificity <- p_dt$ai_spec
    sd_specificity <- mu_specificity * v_psa_sd$ai_specificity
    p_dt$ai_spec <- sampleBeta(mu_specificity, sd_specificity)

    # transition probabilities
    mu_transition  <- p_transition
    sd_transition  <- mu_transition * v_psa_sd$transitions
    p_transition <- sampleBeta(mu_transition$p_mild_mod_untreated, sd_transition)

    # utilities (treated)
    mu_utilities <- v_utilities
    sd_utilities <- mu_utilities * v_psa_sd$utilities
    v_utilities <- sampleBeta(mu_utilities$mild_treated, sd_utilities)

    # utilities (untreated)

}



runScenario <- function(vary, perspective, descriptives, output = "icer"){
    strategy <- get("strategy", envir = parent.frame())

    if (vary == "screening_age"){

      return_list <- list()

      # Scenario - 50 to 85 years
      print("----------------------------------")
      print("Scenario: 50 to 85 years")
      age_categories <- get("age_categories", envir = globalenv()) #re-obtain age_categories from global environment
      
      age_categories <- c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years", "75 to 80 years", "80 to 85 years")
      t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)

      return_list$screening_age_1 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - 55 to 75 years
      print("----------------------------------")
      print("Scenario: 60 to 75 years")
      age_categories <- get("age_categories", envir = globalenv()) #re-obtain age_categories from global environment
      
      age_categories <- c("60 to 65 years", "65 to 70 years", "70 to 75 years")
      t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)

      return_list$screening_age_2 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - 60 to 70 years
      print("----------------------------------")
      print("Scenario: 60 to 75 years")
      age_categories <- get("age_categories", envir = globalenv()) #re-obtain age_categories from global environment

      
      age_categories <- c("60 to 65 years", "65 to 70 years")
      t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)

      return_list$screening_age_3 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

    } else if (vary == "screening_interval") {
      return_list <- list()

      # Scenario - every 3 years
      print("----------------------------------")
      print("Scenario: screening every 3 year")
      screening_interval <- get("screening_interval", envir = globalenv()) #re-obtain screening_interval from global environment
      
      screening_interval <- 3
      return_list$interval_1 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - every 10 years
      print("----------------------------------")
      print("Scenario: screening every 10 year")
      screening_interval <- get("screening_interval", envir = globalenv()) #re-obtain screening_interval from global environment

      screening_interval <- 10
      return_list$interval_2 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - once
      print("----------------------------------")
      print("Scenario: screening at 50 and 65")
      screening_interval <- get("screening_interval", envir = globalenv()) #re-obtain screening_interval from global environment

      screening_interval <- 15
      return_list$interval_3 <- callModel(descriptives = descriptives, perspective = perspective, output = output)
      # varying transition probabilities
    } else if (vary == "transition") {
      return_list <- list()

      # Scenario 1 - untreated probabilities Burr (2014)
      print("Scenario: Untreated transition Burr 2014")

      p_transition <- get("p_transition", envir = globalenv()) #re-obtain p_transition from global environment
      p_transition$p_mild_mod_untreated <- 0.129
      p_transition$p_mod_sev_untreated <- 0.048
      p_transition$p_sev_blind_untreated <- 0.042

      return_list$transition_1 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - treated Chuahan (2014)
      print("Scenario: treated transition Chauhan 2014")

      p_transition <- get("p_transition", envir = globalenv()) #re-obtain p_transition from global environment
      p_transition$p_mild_mod_treated <- 0.020507299
      p_transition$p_mod_sev_treated <- 0.043812862
      p_transition$p_sev_blind_treated <- 0.018346021

      return_list$transition_2 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - untreated & treated probabilities Garway (2014)
      print("Scenario: Transitions garway")

      p_transition <- get("p_transition", envir = globalenv()) #re-obtain p_transition from global environment
      p_transition$p_mild_mod_untreated <- 0.10037277
      p_transition$p_mod_sev_untreated <- 0.075089004
      p_transition$p_sev_blind_untreated <- 0.056142589
      p_transition$p_mild_mod_treated <- 0.033819931
      p_transition$p_mod_sev_treated <- 0.023480845
      p_transition$p_sev_blind_treated <- 0.018810467

      return_list$transition_3 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

    } else if (vary == "utilities") {
      return_list <- list()

      # Scenario - utilities (untreated) Burr (2014)
      print("Scenario: utilities HUI-3")

      v_utilities <- get("v_utilities", envir = globalenv()) #re-obtain v_utilities from global environment
      v_utilities$mild_untreated <- 0.977011494
      v_utilities$mild_treated <- 0.977011494
      v_utilities$mod_untreated <- 0.850574713
      v_utilities$mod_treated <- 0.850574713
      v_utilities$severe_untreated <- 0.666666667
      v_utilities$severe_treated <- 0.666666667
      v_utilities$blind <- 0.5350

      return_list$utilities_1 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - utilities (treated) Chuahan (2014)
      print("Scenario: utilities Burr (2007)")

      v_utilities <- get("v_utilities", envir = globalenv()) #re-obtain v_utilities from global environment
      v_utilities$mild_untreated <- 0.8015
      v_utilities$mild_treated <- 0.8015
      v_utilities$mod_untreated <- 0.7471
      v_utilities$mod_treated <- 0.7471
      v_utilities$severe_untreated <- 0.7113
      v_utilities$severe_treated <- 0.7113
      v_utilities$blind <- 0.5350

      return_list$utilities_2 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

    } else if (vary == "compliance") {
      return_list <- list()

      # Scenario - compliance Burr (2014)
      print("Scenario: Compliance: 100% ")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment
      p_dt$screen_comp <- 1
      p_dt$ref_comp <- 1
      return_list$compliance_1 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - compliance literature
      print("Scenario: Compliance literature")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment
      p_dt$screen_comp <- 0.345
      p_dt$ref_comp <- 0.69
      return_list$compliance_2 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

    } else if (vary == "ai_senstivity") {
      return_list <- list()

      # Scenario - ensemble model
      print("Scenario: Ensemble model")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment
      p_dt$ai_sens <- 0.87

      return_list$ai_performance_1 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - high performance
      print("Scenario: high performance (90% sensitivity)")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment 
      p_dt$ai_sens <- 0.90

      return_list$ai_performance_2 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - low performance
      print("Scenario: low performance (75% sensitivity)")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment
      p_dt$ai_sens <- 0.75

      return_list$ai_performance_3 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

    } else if (vary == "ai_specificity") {
      return_list <- list()

      # Scenario - ensemble model
      print("Scenario: high performance (98% specificity)")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment
      p_dt$ai_spec <- 0.975

      return_list$ai_performance_1 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - high performance
      print("Scenario: low performance (90% specificity)")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment
      p_dt$ai_spec <- 0.90

      return_list$ai_performance_2 <- callModel(descriptives = descriptives, perspective = perspective, output = output)


    } else if (vary == "expert_utilisation") {
      return_list <- list()

      # Scenario - expert utilisation minmum
      print("Scenario: Expert utilisation minimum")

      v_cost_utilisation_diagnostics <- get("v_cost_utilisation_diagnostics", envir = globalenv()) #re-obtain from global environment
      v_cost_utilisation_diagnostics$average_best <- v_cost_utilisation_diagnostics$lower_bound           

      v_cost_utilisation_intervention <- get("v_cost_utilisation_intervention", envir = globalenv()) #re-obtain from global environment
      v_cost_utilisation_intervention$average_best <- v_cost_utilisation_intervention$lower_bound

      return_list$expert_utilisation_1 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - expert utilisation maximum
      print("Scenario: Expert utilisation maximum")

      v_cost_utilisation_diagnostics <- get("v_cost_utilisation_diagnostics", envir = globalenv()) #re-obtain from global environment
      v_cost_utilisation_diagnostics$average_best <- v_cost_utilisation_diagnostics$upper_bound

      v_cost_utilisation_intervention <- get("v_cost_utilisation_intervention", envir = globalenv()) #re-obtain from global environment
      v_cost_utilisation_intervention$average_best <- v_cost_utilisation_intervention$upper_bound

      return_list$expert_utilisation_2 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

    } else if (vary == "expert_observation") {
      return_list <- list()

      # Scenario - expert observation minimum
      print("Scenario: Observation transitions expert minimum")
      p_transition <- get("p_transition", envir = globalenv()) #re-obtain p_transition from global environment
      p_severity_undiagnosed <- get("p_severity_undiagnosed", envir = globalenv()) #re-obtain p_dt from global environment
      p_transition$healthy_obs <- 0.01
      p_transition$obs_healthy <- 0.160833333
      p_severity_undiagnosed$observation <- 0.152

      return_list$expert_observation_1 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

      # Scenario - expert observation minimum
      print("Scenario: Observation transitions expert maximum")
      p_transition <- get("p_transition", envir = globalenv()) #re-obtain p_transition from global environment
      p_severity_undiagnosed <- get("p_severity_undiagnosed", envir = globalenv()) #re-obtain p_dt from global environment

      p_transition$healthy_obs <- 0.075
      p_transition$obs_healthy <- 0.461666667
      p_severity_undiagnosed$observation <- 0.42

      return_list$expert_observation_2 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

    } else {
            print("Error: parameter not found")

    } 
    return(return_list)
  }

scenarioPlot <- function(){
  # run base case
  strategy <- "base"
  base_icer <- callModel(descriptives = TRUE, perspective= "societal", total_population = FALSE, output = "icer")
  base_qaly <- callModel(descriptives = TRUE, perspective= "societal", total_population = FALSE, output = "qaly")
  base_costs <- callModel(descriptives = TRUE, perspective= "societal", total_population = FALSE, output = "costs")

  # run base case
  strategy <- "scenario"
  icer_screening_age <- runScenario(vary = "screening_age", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_screening_interval <- runScenario(vary = "screening_interval", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_transition <- runScenario(vary = "transition", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_utilities <- runScenario(vary = "utilities", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_compliance <- runScenario(vary = "compliance", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_sensitivity <- runScenario(vary = "ai_senstivity", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_specificity <- runScenario(vary = "ai_specificity", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_expert_utilisation <- runScenario(vary = "expert_utilisation", perspective = "societal", descriptives = TRUE, output = "icer")
  icer_expert_observation <- runScenario(vary = "expert_observation", perspective = "societal", descriptives = TRUE, output = "icer")

  qaly_screening_age <- runScenario(vary = "screening_age", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_screening_interval <- runScenario(vary = "screening_interval", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_transition <- runScenario(vary = "transition", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_utilities <- runScenario(vary = "utilities", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_compliance <- runScenario(vary = "compliance", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_sensitivity <- runScenario(vary = "ai_senstivity", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_specificity <- runScenario(vary = "ai_specificity", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_expert_utilisation <- runScenario(vary = "expert_utilisation", perspective = "societal", descriptives = TRUE, output = "qaly")
  qaly_expert_observation <- runScenario(vary = "expert_observation", perspective = "societal", descriptives = TRUE, output = "qaly")

  costs_screening_age <- runScenario(vary = "screening_age", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_screening_interval <- runScenario(vary = "screening_interval", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_transition <- runScenario(vary = "transition", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_utilities <- runScenario(vary = "utilities", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_compliance <- runScenario(vary = "compliance", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_sensitivity <- runScenario(vary = "ai_senstivity", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_specificity <- runScenario(vary = "ai_specificity", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_expert_utilisation <- runScenario(vary = "expert_utilisation", perspective = "societal", descriptives = TRUE, output = "costs")
  costs_expert_observation <- runScenario(vary = "expert_observation", perspective = "societal", descriptives = TRUE, output = "costs")

  # Combine all icer_... lists into a list of lists for easier processing
  icer_lists <- list(
      expert_observation = icer_expert_observation,
      expert_utilisation = icer_expert_utilisation,
      ai_sensitivity = icer_sensitivity,
      ai_specificity = icer_specificity,
      compliance = icer_compliance,
      utilities = icer_utilities,
      transition = icer_transition,
      screening_interval = icer_screening_interval,
      screening_age = icer_screening_age,
      base_icer = base_icer
  )

  qaly_lists <- list(
      expert_observation = qaly_expert_observation,
      expert_utilisation = qaly_expert_utilisation,
      ai_sensitivity = qaly_sensitivity,
      ai_specificity = qaly_specificity,
      compliance = qaly_compliance,
      utilities = qaly_utilities,
      transition = qaly_transition,
      screening_interval = qaly_screening_interval,
      screening_age = qaly_screening_age,
      base_qaly = base_qaly
  ) 

  costs_lists <- list(
      expert_observation = costs_expert_observation,
      expert_utilisation = costs_expert_utilisation,
      ai_sensitivity = costs_sensitivity,
      ai_specificity = costs_specificity,
      compliance = costs_compliance,
      utilities = costs_utilities,
      transition = costs_transition,
      screening_interval = costs_screening_interval,
      screening_age = costs_screening_age,
      base_costs = base_costs
  )

  # Initialize an empty data frame to store ICERs and their types
  icer_data <- data.frame(ICER = numeric(), Type = character(), stringsAsFactors = FALSE)
  qaly_data <- data.frame(QALY = numeric(), Type = character(), stringsAsFactors = FALSE)
  costs_data <- data.frame(Costs = numeric(), Type = character(), stringsAsFactors = FALSE)

  # Loop through each list to extract ICERs and their corresponding types
  for (type in names(icer_lists)) {
    for (scenario in names(icer_lists[[type]])) {
      icer_value <- icer_lists[[type]][[scenario]]
      icer_data <- rbind(icer_data, data.frame(ICER = icer_value, Type = paste(type, scenario, sep = "_")))
    }
  }

  for (type in names(qaly_lists)) {
    for (scenario in names(qaly_lists[[type]])) {
      qaly_value <- qaly_lists[[type]][[scenario]]
      qaly_data <- rbind(qaly_data, data.frame(QALY = qaly_value, Type = paste(type, scenario, sep = "_")))
    }
  }

  for (type in names(costs_lists)) {
    for (scenario in names(costs_lists[[type]])) {
      costs_value <- costs_lists[[type]][[scenario]]
      costs_data <- rbind(costs_data, data.frame(Costs = costs_value, Type = paste(type, scenario, sep = "_")))
    }
  }


  # Modify the Type assignment to ensure all ICERs of the same category share the same y-coordinate
  icer_data$Category <- factor(substr(icer_data$Type, 1, nchar(icer_data$Type) - 2), levels = unique(substr(icer_data$Type, 1, nchar(icer_data$Type) - 2)))
  qaly_data$Category <- factor(substr(qaly_data$Type, 1, nchar(qaly_data$Type) - 2), levels = unique(substr(qaly_data$Type, 1, nchar(qaly_data$Type) - 2)))
  costs_data$Category <- factor(substr(costs_data$Type, 1, nchar(costs_data$Type) - 2), levels = unique(substr(costs_data$Type, 1, nchar(costs_data$Type) - 2)))

  # Add "base" ICER to the data frame
  icer_data <- rbind(icer_data, data.frame(ICER = base_icer, Type = "base", Category = "base"))
  qaly_data <- rbind(qaly_data, data.frame(QALY = base_qaly, Type = "base", Category = "base"))
  costs_data <- rbind(costs_data, data.frame(Costs = base_costs, Type = "base", Category = "base"))

  # Assuming 'base' and 'icer_data' are already defined

  # Example of adding "Shape" and "Color" columns to the dataframe
  # This is a simplified example. You'll need to assign unique shapes and colors based on your actual data
  icer_data$Shape <- c(24,21, 24,21, 24,22,21, 24,21, 24,21, 24,22,21, 24,22,21, 24,22,21, 24,22,21)
  icer_data$Color <- c("#800080","#800080","#00FF00","#00FF00","#0000FF","#0000FF","#0000FF", "#964B00", "#964B00", 
  "#FFFF00","#FFFF00","#FF00FF","#FF00FF","#00FFFF","#00FFFF","#00FFFF","#FFA500","#FFA500","#FFA500","#A9A9A9","#A9A9A9","#A9A9A9","#FF0000")

  qaly_data$Shape <- c(24,21, 24,21, 24,22,21, 24,21, 24,21, 24,22,21, 24,22,21, 24,22,21, 24,22,21)
  qaly_data$Color <- c("#800080","#800080","#00FF00","#00FF00","#0000FF","#0000FF","#0000FF", "#964B00", "#964B00", 
  "#FFFF00","#FFFF00","#FF00FF","#FF00FF","#00FFFF","#00FFFF","#00FFFF","#FFA500","#FFA500","#FFA500","#A9A9A9","#A9A9A9","#A9A9A9","#FF0000")

  costs_data$Shape <- c(24,21, 24,21, 24,22,21, 24,21, 24,21, 24,22,21, 24,22,21, 24,22,21, 24,22,21)
  costs_data$Color <- c("#800080","#800080","#00FF00","#00FF00","#0000FF","#0000FF","#0000FF", "#964B00", "#964B00", 
  "#FFFF00","#FFFF00","#FF00FF","#FF00FF","#00FFFF","#00FFFF","#00FFFF","#FFA500","#FFA500","#FFA500","#A9A9A9","#A9A9A9","#A9A9A9","#FF0000")

  # Ensure shapes can be filled (using shapes 21-25)
  # If your data already uses shapes 21-25, this step is correct. Otherwise, adjust your shape assignments accordingly.
  icer_data$Identifier <- c("Expert observation min", "Expert observation max","Expert utilisitation min", "Expert utilisation max","AI sensitivity 87%", "AI sensitivity 90%", "AI sensitivity 75%","AI specificity 98%", "AI specificity 90%","Compliance 100%", "Compliance from literature","Uilities HUI-3", "Utilities EQ-5D Burr (2007)","Transitions from Burr (2014)", "Transitions from Chauhan (2014)", "Transitions from Garway (2015)","Screening interval every 3 years", "Screening interval age 50, 60 & 70", "Screening interval age 50 & 65","Screening age 50-85", "Screening age 60-75", "Screening age 60-70", "Base case")

  qaly_data$Identifier <- c("Expert observation min", "Expert observation max","Expert utilisitation min", "Expert utilisation max","AI sensitivity 87%", "AI sensitivity 90%", "AI sensitivity 75%","AI specificity 98%", "AI specificity 90%","Compliance 100%", "Compliance from literature","Uilities HUI-3", "Utilities EQ-5D Burr (2007)","Transitions from Burr (2014)", "Transitions from Chauhan (2014)", "Transitions from Garway (2015)","Screening interval every 3 years", "Screening interval age 50, 60 & 70", "Screening interval age 50 & 65","Screening age 50-85", "Screening age 60-75", "Screening age 60-70", "Base case")

  costs_data$Identifier <- c("Expert observation min", "Expert observation max","Expert utilisitation min", "Expert utilisation max","AI sensitivity 87%", "AI sensitivity 90%", "AI sensitivity 75%","AI specificity 98%", "AI specificity 90%","Compliance 100%", "Compliance from literature","Uilities HUI-3", "Utilities EQ-5D Burr (2007)","Transitions from Burr (2014)", "Transitions from Chauhan (2014)", "Transitions from Garway (2015)","Screening interval every 3 years", "Screening interval age 50, 60 & 70", "Screening interval age 50 & 65","Screening age 50-85", "Screening age 60-75", "Screening age 60-70", "Base case")

  # Add ICER values to the Identifier string
  icer_data$Identifier <- paste(icer_data$Identifier, " - (",round(icer_data$ICER,0),")", sep = "")
  qaly_data$Identifier <- paste(qaly_data$Identifier, " - (",round(qaly_data$QALY,4),")", sep = "")
  costs_data$Identifier <- paste(costs_data$Identifier, " - (",round(costs_data$Costs,0),")", sep = "")

  # Adjust the plot to use 'Identifier' for color, shape, and fill in a single legend
  # Load necessary libraries
  library(ggplot2)

  legend_plot <- ggplot(icer_data, aes(x = ICER, y = Category, color = Identifier, shape = Identifier, fill = Identifier)) +
    geom_point(position = position_dodge(width = 0), size = 8, stroke = 1, color = "black", alpha = 1) +
    scale_color_manual(values = setNames(icer_data$Color, icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
    scale_fill_manual(values = setNames(icer_data$Color, icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
    scale_shape_manual(values = setNames(as.numeric(icer_data$Shape), icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
    labs(
      title = "",
      x = "",
      y = "",
      shape = "Scenario (ICER)",
      fill = "Scenario (ICER)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "left",
      legend.title = element_text(size = 15, face = "bold", hjust = 0.04),
      legend.text = element_text(size = 9.5, hjust = 1),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12),
      panel.grid.major = element_line(color = "#adadad"),  
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray")
    ) +
    geom_vline(xintercept = base_icer, linetype = "solid", color = "black", size = 1) +
    scale_x_continuous(
      breaks = seq(10000, 70000, by = 10000),  # Major breaks for axis labels
      minor_breaks = seq(0, 70000, by = 2500)  # Minor breaks for grid lines
    ) +
    geom_point(data = icer_data, aes(x = ICER, y = Category), position = position_dodge(width = 0), size = 0.4, color = "black", alpha = 1, inherit.aes = FALSE)


  # Adjust the plot to use 'Identifier' for color, shape, and fill in a single legend
  plot_icer <- ggplot(icer_data, aes(x = ICER, y = Category, color = Identifier, shape = Identifier, fill = Identifier)) +
    geom_point(position = position_dodge(width = 0), size = 8, stroke = 1, color = "black", alpha = 0.6) +
    scale_color_manual(values = setNames(icer_data$Color, icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
    scale_fill_manual(values = setNames(icer_data$Color, icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
    scale_shape_manual(values = setNames(as.numeric(icer_data$Shape), icer_data$Identifier), breaks = rev(icer_data$Identifier)) +
    labs(
      title = "",
      x = "",
      y = "",
      shape = "Scenario (ICER)",
      fill = "Scenario (ICER)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      legend.title = element_text(size = 15, face = "bold", hjust = 0.04),
      legend.text = element_text(size = 9.5, hjust = 1),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12),
      panel.grid.major = element_line(color = "#adadad"),  
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray")
    ) +
    geom_vline(xintercept = base_icer, linetype = "solid", color = "black", size = 1) +
    scale_x_continuous(
      breaks = seq(10000, 70000, by = 10000),  # Major breaks for axis labels
      minor_breaks = seq(0, 70000, by = 2500)  # Minor breaks for grid lines
    ) +
    geom_point(data = icer_data, aes(x = ICER, y = Category), position = position_dodge(width = 0), size = 0.4, color = "black", alpha = 1, inherit.aes = FALSE)


  plot_qaly <- ggplot(qaly_data, aes(x = QALY, y = Category, color = Identifier, shape = Identifier, fill = Identifier)) +
    geom_point(position = position_dodge(width = 0), size = 8, stroke = 1, color = "black", alpha = 0.6) +
    #geom_point(position = position_dodge(width = 0.5), size = 1, stroke = 1, fill = "black", alpha = 1, show.legend = FALSE) +
    scale_color_manual(values = setNames(qaly_data$Color, qaly_data$Identifier), breaks = rev(qaly_data$Identifier)) +
    scale_fill_manual(values = setNames(qaly_data$Color, qaly_data$Identifier), breaks = rev(qaly_data$Identifier)) +
    scale_shape_manual(values = setNames(as.numeric(qaly_data$Shape), qaly_data$Identifier), breaks = rev(qaly_data$Identifier)) +
    labs(
      title = "",
      x = "Incremental QALY",
      y = "",
      shape = "Scenario (QALY)",
      fill = "Scenario (QALY)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 12, hjust = 1),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12),
      panel.grid.major = element_line(color = "#adadad"),  
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray")
    ) + 
    geom_vline(xintercept = base_qaly, linetype = "solid", color = "black", size = 1) +
    scale_x_continuous(breaks = seq(0, 0.1, by = 0.005)) +
    geom_point(data = qaly_data, aes(x = QALY, y = Category), position = position_dodge(width = 0), size = 0.4, color = "black", alpha = 1, inherit.aes = FALSE)


  plot_costs <- ggplot(costs_data, aes(x = Costs, y = Category, color = Identifier, shape = Identifier, fill = Identifier)) +
    geom_point(position = position_dodge(width = 0), size = 8, stroke = 1, color = "black", alpha = 0.6) +
    scale_color_manual(values = setNames(costs_data$Color, costs_data$Identifier), breaks = rev(costs_data$Identifier)) +
    scale_fill_manual(values = setNames(costs_data$Color, costs_data$Identifier), breaks = rev(costs_data$Identifier)) +
    scale_shape_manual(values = setNames(as.numeric(costs_data$Shape), costs_data$Identifier), breaks = rev(costs_data$Identifier)) +
    labs(
      title = "",
      x = "Incremental Costs",
      y = "",
      shape = "Scenario (Costs)",
      fill = "Scenario (Costs)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",  # Remove legend
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 12),
      panel.grid.major = element_line(color = "#adadad"),  
      panel.grid.minor = element_line(color = "#adadad"),
      panel.background = element_rect(fill = "lightgray")
    ) +
    geom_vline(xintercept = base_costs, linetype = "solid", color = "black", size = 1) +
    scale_x_continuous(breaks = seq(0, 700, by = 50)) +
    geom_point(data = costs_data, aes(x = Costs, y = Category), position = position_dodge(width = 0), size = 0.4, color = "black", alpha = 1, inherit.aes = FALSE)

  # Extract the legend
  g <- ggplotGrob(legend_plot)
  legend <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]

  plot_icer <- plot_icer + theme(legend.position = "none")

  # Arrange the plots in a 2x2 grid with the legend in the bottom left quadrant
  plot <- grid.arrange(
    plot_qaly, plot_costs, legend, plot_icer,
    ncol = 2,
    nrow = 2,
    layout_matrix = rbind(c(1, 2),
                          c(3, 4)),
    heights = c(2, 2),  # Adjust the heights to make the bottom row taller
    widths = c(2, 2)    # Adjust the widths to make the left column wider
  )

  # Save plot to figures folder
  ggsave("figures/scenario.png", plot = plot, width = 12, height = 10)
}
    
tornadoPlot <-function(Parms, Outcomes, titleName, outcomeName){
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  # Grouped Bar Plot
  # Determine the overall optimal strategy
  paramNames2 <- Parms
  
  # Combine the parameter list with the data
  ymean <- Outcomes[1,1]
  yMin <- Outcomes[,2] - ymean
  yMax <- Outcomes[,3] - ymean
  ySize <- abs(yMax - yMin)  #High value - Low value
  
  
  rankY<- order(ySize)
  nParams <- length(paramNames2)
  
  Tor <- data.frame(
    Parameter=c(paramNames2[rankY],paramNames2[rankY]),  
    Level=c(rep("Lower bound",nParams),rep("Upper bound",nParams)),
    value=ymean+c(yMin[rankY],yMax[rankY]),
    sort=seq(1,nParams)
  )
  
  #re-order the levels in the order of appearance in the data.frame
  Tor$Parameter2 <- ordered(Tor$Parameter, Tor$Parameter[1:(length(Tor$Parameter)/2)])
  # Tor$Parameter2 <- factor(Tor$Parameter, as.character(Tor$Parameter))
  #Define offset as a new axis transformation. Source: http://blog.ggplot2.org/post/25938265813/defining-a-new-transformation-for-ggplot2-scales  
  offset_trans <- function(offset=0) {
    trans_new(paste0("offset-", format(offset)), function(x) x-offset, function(x) x+offset)
  }
  #Plot the Tornado diagram.
  txtsize<-12
  print(
  ggplot(Tor, aes(x=Parameter2, y=value, fill=Level)) +  # Ensure 'fill' is mapped to 'Level' within 'aes()'
    geom_bar(data=Tor[Tor$Level=="Lower bound",], aes(x=Parameter2, y=value, fill="Lower bound"), stat="identity", alpha=0.8) +
    geom_bar(data=Tor[Tor$Level=="Upper bound",], aes(x=Parameter2, y=value, fill="Upper bound"), stat="identity", alpha=0.8) +
    ggtitle("", subtitle = outcomeName) +
    scale_fill_manual(name="Parameter Level", values=c("Lower bound"="blue", "Upper bound"="red")) +  # Define colors for 'Low' and 'High'
    scale_y_continuous(name="ICER", trans=offset_trans(offset=ymean), labels = function(x) ifelse(x == ymean, paste(x, " (ymean)", sep = ""), x)) +
    scale_x_discrete(name="Parameter") +
    geom_hline(yintercept = ymean, linetype = "dotted", size=0.5) +
    theme_bw(base_size = 14) +
    coord_flip() +
    theme_minimal(base_size = 15) +
    theme(legend.position = c(0.95, 0.05), # This positions the legend at the right bottom corner
          legend.justification = c(1, 0), # This ensures the legend aligns at its bottom-right corner
          legend.box.just = "right", # Aligns the legend box to the right
          legend.margin = margin(), # Adjust margins if necessary
          legend.box = "vertical",
          legend.title=element_text(size = txtsize, angle = 0, hjust = 1),
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize),
          title = element_text(face="bold", size=15),
          axis.title.x = element_text(face="bold", size=txtsize),
          axis.title.y = element_text(face="bold", size=txtsize),
          axis.text.y = element_text(size=txtsize),
          axis.text.x = element_text(size=txtsize),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_line(color = "#adadad"),  
          panel.grid.minor = element_line(color = "#adadad"),
          panel.background = element_rect(fill = "lightgray")
    )
  )
}
