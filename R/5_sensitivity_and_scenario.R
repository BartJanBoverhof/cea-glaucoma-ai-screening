runDSA <- function(parameter){

    strategy <- get("strategy", envir = parent.frame())

    # modify selected parameter
    if (parameter == "transition_untreated"){
        
        p_transition <- p_transition # local save

        # transfer paramaters to lower bound
        p_transition$p_mild_mod_untreated <- p_transition$p_mild_mod_untreated * (1 - v_dsa_se$p_mild_mod_untreated)
        p_transition$p_mod_sev_untreated <- p_transition$p_mod_sev_untreated * (1 - v_dsa_se$p_mod_sev_untreated)
        p_transition$p_sev_blind_untreated <- p_transition$p_sev_blind_untreated * (1 - v_dsa_se$p_sev_blind_untreated)

        lower <- callModel() # lower bound

        # reset variable from global variable
        p_transition <- get("p_transition", envir =globalenv())

        # transfer paramaters to upper bound
        p_transition$p_mild_mod_untreated <- p_transition$p_mild_mod_untreated * (1 + v_dsa_se$p_mild_mod_untreated)
        p_transition$p_mod_sev_untreated <- p_transition$p_mod_sev_untreated * (1 + v_dsa_se$p_mod_sev_untreated)
        p_transition$p_sev_blind_untreated <- p_transition$p_sev_blind_untreated * (1 + v_dsa_se$p_sev_blind_untreated)

        upper <- callModel() # lower bound

    } else if (parameter == "transition_treated"){
            
        # reset variable from global variable
        p_transition <- get("p_transition", envir =globalenv())

        # transfer paramaters to lower bound
        p_transition$p_mild_mod_treated <- p_transition$p_mild_mod_treated * (1 - v_dsa_se$p_mild_mod_treated)
        p_transition$p_mod_sev_treated <- p_transition$p_mod_sev_treated * (1 - v_dsa_se$p_mod_sev_treated)
        p_transition$p_sev_blind_treated <- p_transition$p_sev_blind_treated * (1 - v_dsa_se$p_sev_blind_treated)

        lower <- callModel() # lower bound

        # reset variable from global variable
        p_transition <- get("p_transition", envir =globalenv())

        # transfer paramaters to upper bound
        p_transition$p_mild_mod_treated <- p_transition$p_mild_mod_treated * (1 + v_dsa_se$p_mild_mod_treated)
        p_transition$p_mod_sev_treated <- p_transition$p_mod_sev_treated * (1 + v_dsa_se$p_mod_sev_treated)
        p_transition$p_sev_blind_treated <- p_transition$p_sev_blind_treated * (1 + v_dsa_se$p_sev_blind_treated)

        upper <- callModel() # lower bound
    
    } else if (parameter == "sensitivity"){
        
        p_dt <- p_dt # local save

        # transform parameters to lower bound
        p_dt$ai_sens <- p_dt$ai_sens * (1 - v_dsa_se$ai_sensitivity)
        
        lower <- callModel() # lower bound

        # reset variable from global variable
        p_dt <- get("p_dt", envir =globalenv())

        # transform parameters to upper bound
        p_dt$ai_sens <- p_dt$ai_sens * (1 + v_dsa_se$ai_sensitivity)
        if (p_dt$ai_sens > 1) {p_dt$ai_sens <- 1} # if higher than 1, make it 1

        upper <- callModel() # lower bound
    
    } else if (parameter == "specificity"){
    
        p_dt <- p_dt # local save

        # transform parameters to lower bound
        p_dt$ai_spec <- p_dt$ai_spec * (1 - v_dsa_se$ai_specificity)

        lower <- callModel() # lower bound

        # reset variable from global variable
        p_dt <- get("p_dt", envir =globalenv())

        # transform parameters to upper bound
        p_dt$ai_spec <- p_dt$ai_spec * (1 + v_dsa_se$ai_specificity)
        if (p_dt$ai_spec > 1) {p_dt$ai_spec <- 1} # if higher than 1, make it 1

        upper <- callModel() # lower bound

    } else if (parameter == "prevalence"){
    
        v_prevalence <- v_prevalence # local save

        # transform parameters to lower bound
        v_prevalence$prevalence <- v_prevalence$prevalence * (1 - v_dsa_se$prevalence)

        lower <- callModel() # lower bound

        # reset variable from global variable
        v_prevalence <- get("v_prevalence", envir =globalenv())

        # transform parameters to upper bound
        v_prevalence$prevalence <- v_prevalence$prevalence * (1 + v_dsa_se$prevalence)

        upper <- callModel() # lower bound

    } else if (parameter == "incidences_of"){

        v_incidences_of <- v_incidences_of # local save

        # transform parameters to lower bound
        v_incidences_of$incidence <- v_incidences_of$incidence * (1 - v_dsa_se$incidence_of)
        
        lower <- callModel() # lower bound

        # reset variable from global variable
        v_incidences_of <- get("v_incidences_of", envir =globalenv())

        # transform parameters to upper bound
        v_incidences_of$incidence <- v_incidences_of$incidence * (1 + v_dsa_se$incidence_of)

        upper <- callModel() # lower bound

    } else if (parameter == "incidences_screening"){
    
        v_incidences_screening <- v_incidences_screening # local save

        # transform parameters to lower bound
        v_incidences_screening$incidence <- v_incidences_screening$incidence * (1 - v_dsa_se$incidence_screening)
        
        lower <- callModel() # lower bound

        # reset variable from global variable
        v_incidences_screening <- get("v_incidences_screening", envir =globalenv())

        # transform parameters to upper bound
        v_incidences_screening$incidence <- v_incidences_screening$incidence * (1 + v_dsa_se$incidence_screening)

        upper <- callModel() # lower bound

    } else if (parameter == "utilities_untreated"){
    
        v_utilities <- v_utilities # local save

        # transform parameters to lower bound
        v_utilities$mild_untreated <- v_utilities$mild_untreated * (1 - v_dsa_se$utilities_mild)
        v_utilities$mod_untreated <- v_utilities$mod_untreated * (1 - v_dsa_se$utilities_mod)
        v_utilities$severe_untreated <- v_utilities$severe_untreated * (1 - v_dsa_se$utilities_sev)
        v_utilities$blind <- v_utilities$blind * (1 - v_dsa_se$utilities_vi)

        lower <- callModel() # lower bound

        # reset variable from global variable
        v_utilities <- get("v_utilities", envir =globalenv())

        # transform parameters to upper bound
        v_utilities$mild_untreated <- v_utilities$mild_untreated * (1 +  v_dsa_se$utilities_mild)
        v_utilities$mod_untreated <- v_utilities$mod_untreated * (1 + v_dsa_se$utilities_mod)
        v_utilities$severe_untreated <- v_utilities$severe_untreated * (1 + v_dsa_se$utilities_sev)
        v_utilities$blind <- v_utilities$blind * (1 + v_dsa_se$utilities_vi)

        upper <- callModel() # lower bound

    } else if (parameter == "utilities_treated"){

        v_utilities <- v_utilities # local save

        # transform parameters to lower bound
        v_utilities$mild_treated <- v_utilities$mild_treated * (1 -  v_dsa_se$utilities_mild)
        v_utilities$mod_treated <- v_utilities$mod_treated * (1 - v_dsa_se$utilities_mod)
        v_utilities$severe_treated <- v_utilities$severe_treated * (1 - v_dsa_se$utilities_sev)
        v_utilities$blind <- v_utilities$blind * (1 - v_dsa_se$utilities_vi)

        lower <- callModel() # lower bound

        # reset variable from global variable
        v_utilities <- get("v_utilities", envir =globalenv())

        # transform parameters to upper bound
        v_utilities$mild_treated <- v_utilities$mild_treated * (1 +  v_dsa_se$utilities_mild)
        v_utilities$mod_treated <- v_utilities$mod_treated * (1 + v_dsa_se$utilities_mod)
        v_utilities$severe_treated <- v_utilities$severe_treated * (1 + v_dsa_se$utilities_sev)
        v_utilities$blind <- v_utilities$blind * (1 + v_dsa_se$utilities_vi)

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

    for (i in 1:n_sample){

      print(paste("PSA sample",i,"of",n_sample,"..."))

      psa_iteration <<- i
       
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
      v_utilities$mild_treated <- v_utilities$mild_untreated <- sampleBeta(v_utilities$mild_treated, v_psa_se$utilities_mild)
      v_utilities$mod_treated <- v_utilities$mod_untreated <- sampleBeta(v_utilities$mod_treated, v_psa_se$utilities_mod)
      v_utilities$severe_treated <- v_utilities$severe_untreated <- sampleBeta(v_utilities$severe_treated, v_psa_se$utilities_sev)
      v_utilities$blind <- sampleBeta(v_utilities$blind, v_psa_se$utilities_vi)

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
      p_transition$p_mild_mod_untreated <- sampleBeta(p_transition$p_mild_mod_untreated, v_psa_se$p_mild_mod_untreated)
      p_transition$p_mod_sev_untreated <- sampleBeta(p_transition$p_mod_sev_untreated, v_psa_se$p_mod_sev_untreated)
      p_transition$p_sev_blind_untreated <- sampleBeta(p_transition$p_sev_blind_untreated, v_psa_se$p_sev_blind_untreated)
      p_transition$p_mild_mod_treated <- sampleBeta(p_transition$p_mild_mod_treated, v_psa_se$p_mild_mod_treated)
      p_transition$p_mod_sev_treated <- sampleBeta(p_transition$p_mod_sev_treated, v_psa_se$p_mod_sev_treated)
      p_transition$p_sev_blind_treated <- sampleBeta(p_transition$p_sev_blind_treated, v_psa_se$p_sev_blind_treated)

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
      v_cost_burden_disease <- as.list(sampleGamma(mu_burden, se_burden))
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

    } else if (vary == "discount") {
      return_list <- list()
      print("Scenario: No discounting")
      discount <- get("discount", envir = globalenv()) #re-obtain discount from global environment
      discount <- FALSE

      return_list$discount_1 <- callModel(descriptives = descriptives, perspective = perspective, output = output)

    } else {
            print("Error: parameter not found")

    } 
    return(return_list)
  }

calculateICER <- function(out) {
    results_df <- data.frame(Simulation = integer(), Incremental_QALY = numeric(), Incremental_Costs = numeric())

    # ICER calculation
    for (i in 1:length(out)) {
        # Extract incremental QALYs and Costs for each simulation
        incremental_qaly <- qaly_costs_list[[i]][[1]]
        incremental_costs <- qaly_costs_list[[i]][[2]]

        results_df <- rbind(results_df, data.frame(Simulation = i, Incremental_QALY = incremental_qaly, Incremental_Costs = incremental_costs))
    }

    # Calculate ICER
    ICER <- mean(results_df$Incremental_Costs) / mean(results_df$Incremental_QALY)
    
    
    return(ICER)
}

# Define a function to calculate the mean, differences, and quantiles for specific cost parameters
costStats <- function(costs_list) {
    # Parameters to calculate differences for
    diff_parameters <- c("ai_screening_pp", 
                         "ai_medicine_pp - soc_medicine_pp",
                         "ai_diagnostic_pp - soc_diagnostic_pp",
                         "ai_intervention_pp - soc_intervention_pp",
                         "ai_burden_pp - soc_burden_pp",
                         "ai_productivity_pp - soc_productivity_pp")
    
    # Initialize lists to store results
    mean_ai <- list()
    mean_soc <- list()
    mean_diffs <- list()
    quantiles_2_5 <- list()
    quantiles_97_5 <- list()
    
    # Loop through each difference parameter
    for (param in diff_parameters) {
        if (param == "ai_screening_pp") {
            # Directly extract the AI screening cost parameter
            param_values <- sapply(costs_list, function(iteration) iteration[[param]])
            
            # Calculate the mean value for AI screening
            mean_ai[[param]] <- mean(param_values)
            mean_soc[[param]] <- NA  # No SoC counterpart
            
            # Calculate the mean difference (which is just the AI mean here)
            mean_diffs[[param]] <- mean(param_values)
            
            # Calculate the 2.5% and 97.5% quantiles of the AI values
            quantiles_2_5[[param]] <- quantile(param_values, 0.025)
            quantiles_97_5[[param]] <- quantile(param_values, 0.975)
        } else {
            # Split the parameter names to calculate the difference
            params_split <- strsplit(param, " - ")[[1]]
            ai_param <- params_split[1]
            soc_param <- params_split[2]
            
            # Calculate the AI and SoC values for each iteration
            ai_values <- sapply(costs_list, function(iteration) iteration[[ai_param]])
            soc_values <- sapply(costs_list, function(iteration) iteration[[soc_param]])
            
            # Calculate the mean value for AI and SoC
            mean_ai[[param]] <- mean(ai_values)
            mean_soc[[param]] <- mean(soc_values)
            
            # Calculate the difference between AI and SoC for each iteration
            param_values <- ai_values - soc_values
            
            # Calculate the mean difference
            mean_diffs[[param]] <- mean(param_values)
            
            # Calculate the 2.5% and 97.5% quantiles of the differences
            quantiles_2_5[[param]] <- quantile(param_values, 0.025)
            quantiles_97_5[[param]] <- quantile(param_values, 0.975)
        }
    }
    
    # Combine the results into a data frame
    results <- data.frame(
        Parameter = diff_parameters,
        Mean_AI = unlist(mean_ai),
        Mean_SoC = unlist(mean_soc),
        Mean_Difference = unlist(mean_diffs),
        `2.5%_Quantile` = unlist(quantiles_2_5),
        `97.5%_Quantile` = unlist(quantiles_97_5)
    )
    
    return(results)
}



# Define a function to calculate the mean, 2.5% quantile, and 97.5% quantile for ICER per year of VI
icerVIStats <- function(out_list) {
    # Extract the ICER values for VI from each element in the list
    icer_vi <- lapply(out_list, function(x) x$icer_vi)
    
    # Unlist the ICER values to get a single vector
    icer_vi_values <- unlist(icer_vi)
    
    # Calculate the mean of the ICER values
    mean_icer <- mean(icer_vi_values)
    
    
    # Combine the results into a list or data frame
    result <- data.frame(
        Mean_ICER = mean_icer
    )
    
    return(result)
}

# Define a function to calculate the mean, differences, and quantiles for QALYs
QALYStats <- function(out_list) {
    # Extract the QALY values for AI and SOC from each element in the list
    qaly_ai <- lapply(out_list, function(x) x$qaly$ai_qaly_pp)
    qaly_soc <- lapply(out_list, function(x) x$qaly$soc_qaly_pp)
    
    # Unlist the QALY values to get a single vector
    qaly_ai_values <- unlist(qaly_ai)
    qaly_soc_values <- unlist(qaly_soc)
    
    # Calculate the mean for AI and SOC QALYs
    mean_qaly_ai <- mean(qaly_ai_values)
    mean_qaly_soc <- mean(qaly_soc_values)
    
    # Calculate the difference between AI and SOC QALYs for each iteration
    qaly_diff_values <- qaly_ai_values - qaly_soc_values
    
    # Calculate the mean difference
    mean_qaly_diff <- mean(qaly_diff_values)
    
    # Calculate the 2.5% and 97.5% quantiles for the difference
    quantile_2_5_diff <- quantile(qaly_diff_values, 0.025)
    quantile_97_5_diff <- quantile(qaly_diff_values, 0.975)
    
    # Combine the results into a data frame
    results <- data.frame(
        Parameter = c("QALY_AI", "QALY_SoC", "QALY_Difference"),
        Mean = c(mean_qaly_ai, mean_qaly_soc, mean_qaly_diff),
        `2.5%_Quantile` = c(NA, NA, quantile_2_5_diff),  # Quantiles only for the difference
        `97.5%_Quantile` = c(NA, NA, quantile_97_5_diff)  # Quantiles only for the difference
    )
    
    return(results)
}

