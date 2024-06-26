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
      geom_bar(data=Tor[Tor$Level=="High",], aes(x=Parameter2, y=value, fill=level), stat="identity", fill="red", alpha=0.5) +
      geom_hline(yintercept = ymean, linetype = "dotted", size=0.5) +
      theme_bw(base_size = 14) +
      coord_flip() +
      theme(legend.position="bottom",
            legend.title=element_text(size = txtsize, angle = 0, hjust = 1),
            legend.key = element_rect(colour = "black"),
            legend.text = element_text(size = txtsize),
            title = element_text(face="bold", size=15),
            axis.title.x = element_text(face="bold", size=txtsize),
            axis.title.y = element_text(face="bold", size=txtsize),
            axis.text.y = element_text(size=txtsize),
            axis.text.x = element_text(size=txtsize),
            axis.ticks.y = element_blank())
  )
  # ggsave(paste("results/", titleName,".png"))
}

runScenario <- function(vary, perspective){
    strategy <- get("strategy", envir = parent.frame())

    if (vary == "transition"){

        print("Base case")
        callModel(descriptives = TRUE, perspective= perspective)
        
        # Scenario 1 - untreated probabilities Burr (2014)
        p_transition <- get("p_transition", envir =globalenv()) #re-obtain p_transition from global environment
        p_transition$p_mild_mod_untreated <- 0.129
        p_transition$p_mod_sev_untreated <- 0.048
        p_transition$p_sev_blind_untreated <- 0.042
        
        print("Scenario: Untreated transition Burr 2014")
        callModel(descriptives = TRUE, perspective= perspective)

        # Scenario 2 - treated Chuahan (2014)
        p_transition <- get("p_transition", envir =globalenv()) #re-obtain p_transition from global environment
        p_transition$p_mild_mod_treated <- 0.020507299
        p_transition$p_mod_sev_treated <- 0.043812862
        p_transition$p_sev_blind_treated <- 0.018346021

        print("Scenario: Untreated transition Burr 2014")
        callModel(descriptives = TRUE, perspective= perspective)

        # Scenario 3 - untreated & treated probabilities Garway (2014)
        p_transition <- get("p_transition", envir =globalenv()) #re-obtain p_transition from global environment
        p_transition$p_mild_mod_untreated <- 0.10037277
        p_transition$p_mod_sev_untreated <- 0.075089004
        p_transition$p_sev_blind_untreated <- 0.056142589
        p_transition$p_mild_mod_treated <- 0.033819931
        p_transition$p_mod_sev_treated <- 0.023480845
        p_transition$p_sev_blind_treated <- 0.018810467

        print("Scenaio: Untreated transition Burr 2014")
        callModel(descriptives = TRUE, perspective= perspective)


    } 
  } 
