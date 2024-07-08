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



runPSA <- function(n_sample){

    strategy <- get("strategy", envir = parent.frame())
    psa_out <- list()
    # run psa samples
    # print progress message
    print(paste("PSA sample",1,"of",n_sample,"..."))

    for (i in 1:n_sample){
      # local save for all parameters
      v_prevalence <- get("v_prevalence", envir =globalenv())
      p_severity_undiagnosed <- get("p_severity_undiagnosed", envir =globalenv()) 
      v_incidences_of <- get("v_incidences_of", envir =globalenv())
      v_incidences_screening <- get("v_incidences_screening", envir =globalenv())
      p_dt <- get("p_dt", envir =globalenv())
      p_transition <- get("p_transition", envir =globalenv())
      v_utilities <- get("v_utilities", envir =globalenv())

      v_cost_utilisation_diagnostics <- get("v_cost_utilisation_diagnostics", envir =globalenv())
      v_cost_utilisation_intervention <- get("v_cost_utilisation_intervention", envir =globalenv())
      v_cost_medicine <- get("v_cost_medicine", envir =globalenv())
      v_cost_burden_disease <- get("v_cost_burden_disease", envir =globalenv())
      v_cost_dt <- get("v_cost_dt", envir =globalenv())
    
      # BETA samples
      # prevalence
      mu_prevalence <- v_prevalence$prevalence
      se_prevalence <- rep(v_psa_se$prevalence,length(mu_prevalence))
      v_prevalence$prevalence <- sampleBeta(mu_prevalence, se_prevalence)

      #incidence of screening
      mu_incidences_screening <- v_incidences_screening$incidence
      se_incidences <- rep(v_psa_se$incidence_screening,length(mu_incidences_screening))
      v_incidences_screening$incidence <- sampleBeta(mu_incidences_screening, se_incidences)

      # incidence of OF
      mu_incidences_of <- v_incidences_of$incidence
      v_incidences_of$incidence <- sampleBeta(mu_incidences_of, se_incidences)

      # sensitivity AI
      mu_sensitivity <- p_dt$ai_sens
      se_sensitivity <- v_psa_se$ai_sensitivity
      p_dt$ai_sens <- sampleBeta(mu_sensitivity, se_sensitivity)

      # specificity AI 
      mu_specificity <- p_dt$ai_spec
      se_specificity <- v_psa_se$ai_specificity
      p_dt$ai_spec <- sampleBeta(mu_specificity, se_specificity)

      # non-definitive decisions in DT
      mu_observation <- p_severity_undiagnosed$observation
      se_observation <- v_psa_se$obs_state
      p_severity_undiagnosed$observation <- sampleBeta(mu_observation, se_observation)

      # utilities 
      mu_utilities <- v_utilities
      
      se_utilities_mild <- v_psa_se$utilities_mild
      se_utilities_mod <- v_psa_se$utilities_mod
      se_utilities_sev <- v_psa_se$utilities_sev
      se_utilities_vi <- v_psa_se$utilities_vi
      
      v_utilities$mild_treated <- v_utilities$mild_untreated <- sampleBeta(mu_utilities$mild_treated, se_utilities_mild)
      v_utilities$mod_treated <- v_utilities$mod_untreated <- sampleBeta(mu_utilities$mod_treated, se_utilities_mod)
      v_utilities$severe_treated <- v_utilities$severe_untreated <- sampleBeta(mu_utilities$severe_treated, se_utilities_sev)
      v_utilities$blind <- sampleBeta(mu_utilities$blind, se_utilities_vi)

      # Assuming v_utilities is already constructed and contains the utilities as shown

      # List of utility categories in the order they should be checked
      utility_categories <- c("mild_treated", "mod_treated", "severe_treated", "blind")

        # Iterate through the utility categories, starting from the second one
        for (z in 2:length(utility_categories)) {
          # Get the current and previous utility category names
          current_category <- utility_categories[z]
          previous_category <- utility_categories[z - 1]
          
          # Check if the current utility is greater than the previous utility
          if (v_utilities[[current_category]] > v_utilities[[previous_category]]) {
            # If so, set the current utility to the previous utility's value
            v_utilities[[current_category]] <- v_utilities[[previous_category]]
          }
        }

      # transition probabilities
      mu_transition <- c(p_transition$p_mild_mod_untreated, p_transition$p_mod_sev_untreated, p_transition$p_sev_blind_untreated,
                        p_transition$p_mild_mod_treated, p_transition$p_mod_sev_treated, p_transition$p_sev_blind_treated)
      se_transition <- c(v_psa_se$p_mild_mod_untreated, v_psa_se$p_mod_sev_untreated, v_psa_se$p_sev_blind_untreated,
                        v_psa_se$p_mild_mod_treated, v_psa_se$p_mod_sev_treated, v_psa_se$p_sev_blind_treated)
      p_transition[-(1:2)] <- as.list(sampleBeta(mu_transition, se_transition))

      #GAMMA samples (costs)
      # screening costs
      mu_screening <- v_cost_dt
      se_screening <- mu_screening * v_psa_se$costs
      v_cost_dt <- as.list(sampleGamma(mu_screening, se_screening))

      # diagnostics
      mu_diagnosics <- v_cost_utilisation_diagnostics$price[1:(length(v_cost_utilisation_diagnostics$price) / 5)] # take the 8 cost items
      se_diagnostics <- mu_diagnosics * v_psa_se$costs
      v_cost_utilisation_diagnostics$price <- rep(sampleGamma(mu_diagnosics, se_diagnostics),5)
      
      # intervention
      mu_intervention <- v_cost_utilisation_intervention$price[1:8] # take the 8 cost items
      se_intervention <- mu_intervention * v_psa_se$costs
      v_cost_utilisation_intervention$price <- c(sampleGamma(mu_intervention, se_intervention), rep(NA, length(v_cost_utilisation_intervention$price) - length(mu_intervention)))

      # medicine
      mu_medicine <- v_cost_medicine
      se_medicine <- mu_medicine * v_psa_se$costs
      v_cost_medicine <- as.list(sampleGamma(mu_medicine, se_medicine))
      
      # burden of disease
      mu_burden <- v_cost_burden_disease[-1]
      se_burden <- mu_burden * v_psa_se$costs
      v_cost_burden_disease[-1] <- as.list(sampleGamma(mu_burden, se_burden))
      v_cost_burden_disease <- lapply(v_cost_burden_disease, function(x) {if(is.nan(x)) 0 else x})

      result <- callModel(output = "qaly_costs")
      psa_out[[i]] <- result
    }

    return(psa_out)
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

