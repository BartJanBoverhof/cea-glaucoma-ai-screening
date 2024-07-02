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
    Level=c(rep("Low",nParams),rep("High",nParams)),
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
    ggplot(Tor[Tor$Level=="Low",], aes(x=Parameter2, y=value, fill=level)) +
      geom_bar(stat="identity", fill="blue") +
      ggtitle("Tornado diagram", subtitle = outcomeName) +
      scale_fill_discrete("Parameter Level: ", l=50) +
      scale_y_continuous(name="ICER", trans=offset_trans(offset=ymean), labels = function(x) ifelse(x == ymean, paste(x, " (ymean)", sep = ""), x)) +
      scale_x_discrete(name="Parameter") +
      geom_bar(data=Tor[Tor$Level=="High",], aes(x=Parameter2, y=value, fill=level), stat="identity", fill="red", alpha=0.8) +
      geom_hline(yintercept = ymean, linetype = "dotted", size=0.5) +
      theme_bw(base_size = 14) +
      coord_flip() +
      theme_minimal(base_size = 15) +
      theme(legend.position="bottom",
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

runScenario <- function(vary, perspective, descriptives){
    strategy <- get("strategy", envir = parent.frame())

    if (vary == "screening_age"){

      return_list <- list()

      # Scenario - 50 to 85 years
      print("----------------------------------")
      print("Scenario: 50 to 85 years")
      age_categories <- get("age_categories", envir = globalenv()) #re-obtain age_categories from global environment
      
      age_categories <- c("50 to 55 years", "55 to 60 years", "60 to 65 years", "65 to 70 years", "70 to 75 years", "75 to 80 years", "80 to 85 years")
      t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)

      return_list$screening_age_1 <- callModel(descriptives = descriptives, perspective = perspective)

      # Scenario - 55 to 75 years
      print("----------------------------------")
      print("Scenario: 60 to 75 years")
      age_categories <- get("age_categories", envir = globalenv()) #re-obtain age_categories from global environment
      
      age_categories <- c("60 to 65 years", "65 to 70 years", "70 to 75 years")
      t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)

      return_list$screening_age_2 <- callModel(descriptives = descriptives, perspective = perspective)

      # Scenario - 60 to 70 years
      print("----------------------------------")
      print("Scenario: 60 to 75 years")
      age_categories <- get("age_categories", envir = globalenv()) #re-obtain age_categories from global environment

      
      age_categories <- c("60 to 65 years", "65 to 70 years")
      t_total_cohort <- getCohort(df_mortality, age_categories = age_categories) ### (function returns distribution of age cohort category)

      return_list$screening_age_3 <- callModel(descriptives = descriptives, perspective = perspective)

    } else if (vary == "screening_interval") {
      return_list <- list()

      # Scenario - every 3 years
      print("----------------------------------")
      print("Scenario: screening every 3 year")
      screening_interval <- get("screening_interval", envir = globalenv()) #re-obtain screening_interval from global environment
      
      screening_interval <- 3
      return_list$interval_1 <- callModel(descriptives = descriptives, perspective = perspective)

      # Scenario - every 10 years
      print("----------------------------------")
      print("Scenario: screening every 10 year")
      screening_interval <- get("screening_interval", envir = globalenv()) #re-obtain screening_interval from global environment

      screening_interval <- 10
      return_list$interval_2 <- callModel(descriptives = descriptives, perspective = perspective)

      # Scenario - once
      print("----------------------------------")
      print("Scenario: screening at 50 and 65")
      screening_interval <- get("screening_interval", envir = globalenv()) #re-obtain screening_interval from global environment

      screening_interval <- 15
      return_list$interval_3 <- callModel(descriptives = descriptives, perspective = perspective)

      # varying transition probabilities
    } else if (vary == "transition") {
      return_list <- list()

      # Scenario 1 - untreated probabilities Burr (2014)
      print("Scenario: Untreated transition Burr 2014")

      p_transition <- get("p_transition", envir = globalenv()) #re-obtain p_transition from global environment
      p_transition$p_mild_mod_untreated <- 0.129
      p_transition$p_mod_sev_untreated <- 0.048
      p_transition$p_sev_blind_untreated <- 0.042

      return_list$transition_1 <- callModel(descriptives = descriptives, perspective = perspective)

      # Scenario - treated Chuahan (2014)
      print("Scenario: treated transition Chauhan 2014")

      p_transition <- get("p_transition", envir = globalenv()) #re-obtain p_transition from global environment
      p_transition$p_mild_mod_treated <- 0.020507299
      p_transition$p_mod_sev_treated <- 0.043812862
      p_transition$p_sev_blind_treated <- 0.018346021

      return_list$transition_2 <- callModel(descriptives = descriptives, perspective = perspective)

      # Scenario - untreated & treated probabilities Garway (2014)
      print("Scenario: Transitions garway")

      p_transition <- get("p_transition", envir = globalenv()) #re-obtain p_transition from global environment
      p_transition$p_mild_mod_untreated <- 0.10037277
      p_transition$p_mod_sev_untreated <- 0.075089004
      p_transition$p_sev_blind_untreated <- 0.056142589
      p_transition$p_mild_mod_treated <- 0.033819931
      p_transition$p_mod_sev_treated <- 0.023480845
      p_transition$p_sev_blind_treated <- 0.018810467

      return_list$transition_3 <- callModel(descriptives = descriptives, perspective = perspective)

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

      return_list$utilities_1 <- callModel(descriptives = descriptives, perspective = perspective)

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

      return_list$utilities_2 <- callModel(descriptives = descriptives, perspective = perspective)

    } else if (vary == "compliance") {
      return_list <- list()

      # Scenario - compliance Burr (2014)
      print("Scenario: Compliance: 100% ")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment
      p_dt$screen_comp <- 1
      p_dt$ref_comp <- 1
      return_list$compliance_1 <- callModel(descriptives = descriptives, perspective = perspective)

      # Scenario - compliance literature
      print("Scenario: Compliance literature")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment
      p_dt$screen_comp <- 0.345
      p_dt$ref_comp <- 0.69
      return_list$compliance_2 <- callModel(descriptives = descriptives, perspective = perspective)

    } else if (vary == "ai_performance") {
      return_list <- list()

      # Scenario - ensemble model
      print("Scenario: Ensemble model")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment
      p_dt$ai_sens <- 0.87
      p_dt$ai_spec <- 0.95

      return_list$ai_performance_1 <- callModel(descriptives = descriptives, perspective = perspective)

      # Scenario - high performance
      print("Scenario: high performance (90% sensnitivity)")

      p_dt <- get("p_dt", envir = globalenv()) #re-obtain p_dt from global environment 
      p_dt$ai_sens <- 0.90
      p_dt$ai_spec <- 0.95

      return_list$ai_performance_2 <- callModel(descriptives = descriptives, perspective = perspective)

    } else if (vary == "expert_utilisation") {
      return_list <- list()

      # Scenario - expert utilisation minmum
      print("Scenario: Expert utilisation minimum")

      v_cost_utilisation_diagnostics <- get("v_cost_utilisation_diagnostics", envir = globalenv()) #re-obtain from global environment
      v_cost_utilisation_diagnostics$average_best <- v_cost_utilisation_diagnostics$lower_bound           

      v_cost_utilisation_intervention <- get("v_cost_utilisation_intervention", envir = globalenv()) #re-obtain from global environment
      v_cost_utilisation_intervention$average_best <- v_cost_utilisation_intervention$lower_bound

      return_list$expert_utilisation_1 <- callModel(descriptives = descriptives, perspective = perspective)

      # Scenario - expert utilisation maximum
      print("Scenario: Expert utilisation maximum")

      v_cost_utilisation_diagnostics <- get("v_cost_utilisation_diagnostics", envir = globalenv()) #re-obtain from global environment
      v_cost_utilisation_diagnostics$average_best <- v_cost_utilisation_diagnostics$upper_bound

      v_cost_utilisation_intervention <- get("v_cost_utilisation_intervention", envir = globalenv()) #re-obtain from global environment
      v_cost_utilisation_intervention$average_best <- v_cost_utilisation_intervention$upper_bound

      return_list$expert_utilisation_2 <- callModel(descriptives = descriptives, perspective = perspective)

    } else if (vary == "expert_observation") {
      return_list <- list()

      # Scenario - expert observation minimum
      print("Scenario: Observation transitions expert minimum")
      p_transition <- get("p_transition", envir = globalenv()) #re-obtain p_transition from global environment
      p_severity_undiagnosed <- get("p_severity_undiagnosed", envir = globalenv()) #re-obtain p_dt from global environment
      p_transition$healthy_obs <- 0.01
      p_transition$obs_healthy <- 0.160833333
      p_severity_undiagnosed$observation <- 0.152

      return_list$expert_observation_1 <- callModel(descriptives = descriptives, perspective = perspective)

      # Scenario - expert observation minimum
      print("Scenario: Observation transitions expert maximum")
      p_transition <- get("p_transition", envir = globalenv()) #re-obtain p_transition from global environment
      p_severity_undiagnosed <- get("p_severity_undiagnosed", envir = globalenv()) #re-obtain p_dt from global environment

      p_transition$healthy_obs <- 0.075
      p_transition$obs_healthy <- 0.461666667
      p_severity_undiagnosed$observation <- 0.42

      return_list$expert_observation_2 <- callModel(descriptives = descriptives, perspective = perspective)

    } else {
            print("Error: parameter not found")

    } 
    return(return_list)
  }
        
      
    
