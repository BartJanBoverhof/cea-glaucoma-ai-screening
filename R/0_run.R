callModel <- function(descriptives = FALSE, perspective = "societal"){
## Isaac: we should add some guidance about how to get the model to run.
## I've been trying for some time and I'm not able to do it. I assumed this could be done from this script, but I'm not getting any results. 
## The idea is to have a model that is as user friendly as possible.
## Without being able to run the model, it's very difficult to validate the code. I'll give it a try in any case.
## In general, please note there is a lack of code description, which again makes it difficult for other users to understand what the code is doing. I understand that commenting your code could be tedious, but it is necessary and a good practice. You can try using ChatGPT to assist you with that. It can be a good exercise to see if it actually understands the code as it should be.    
  
  # inherit local variables from previous function (for dsa, if applicable)
  strategy <- get("strategy", envir = parent.frame())

  if (strategy == "dsa"){ # only inherit variables if stategy is dsa

    parameter  <- get("parameter", envir = parent.frame())

    if (parameter == "transition_untreated"){
      p_transition <- get("p_transition", envir = parent.frame())
    } else if (parameter == "transition_treated"){
      p_transition <- get("p_transition", envir = parent.frame())
    } else if (parameter == "sensitivity"){
      p_dt <- get("p_dt", envir = parent.frame())
    } else if (parameter == "specificity"){
      p_dt <- get("p_dt", envir = parent.frame())
    } else if (parameter == "prevalence"){
      v_prevalence <- get("v_prevalence", envir = parent.frame())
    } else if (parameter == "incidences_of"){
      v_incidences_of <- get("v_incidences_of", envir = parent.frame())
    } else if (parameter == "incidences_screening"){
      v_incidences_screening <- get("v_incidences_screening", envir = parent.frame())
    } else if (parameter == "utilities_untreated"){
      v_utilities <- get("v_utilities", envir = parent.frame())
    } else if (parameter == "utilities_treated"){
      v_utilities <- get("v_utilities", envir = parent.frame())
    } else if (parameter == "costs_screening" || parameter == "costs_medicine" || parameter == "costs_diagnostics" || parameter == "costs_intervention" || parameter == "costs_burden_disease" || parameter == "costs_productivity"){
      cost_multiplier <- get("cost_multiplier", envir = parent.frame())
    }
  }

  if (strategy == "scenario"){    
    p_dt <- get("p_dt", envir = parent.frame())
    t_total_cohort <- get("t_total_cohort", envir = parent.frame())
    p_transition <- get("p_transition", envir = parent.frame())
    age_categories <- get("age_categories", envir = parent.frame())
    screening_interval <- get("screening_interval", envir = parent.frame())
    v_utilities <- get("v_utilities", envir = parent.frame())
    v_cost_utilisation_diagnostics <- get("v_cost_utilisation_diagnostics", envir = parent.frame())
    v_cost_utilisation_intervention <- get("v_cost_utilisation_intervention", envir = parent.frame())
    p_severity_undiagnosed <- get("p_severity_undiagnosed", envir = parent.frame())
  }

  # Define a list to store the results for each age category
  age_results <- list()

  # Loop through each age category and run the model
  for (i in 1:length(age_categories)) {
    age_results[[i]] <- runModel(cohort = age_categories[i])
  }


  # create combined trace ai 
  #a_trace_ai_uncorrected <- age50_55$ai_trace + ### uncorrected trace (for reference)
  #    padArray(pad = age55_60$ai_trace, pad_to = age50_55$ai_trace) + 
  #    padArray(pad = age60_65$ai_trace, pad_to = age50_55$ai_trace) +
  #    padArray(pad = age65_70$ai_trace, pad_to = age50_55$ai_trace) +
  #    padArray(pad = age70_75$ai_trace, pad_to = age50_55$ai_trace)

  # create combined trace soc
  #a_trace_soc_uncorrected <- age50_55$soc_trace + ### uncorrected trace (for reference)
  #    padArray(pad = age55_60$soc_trace, pad_to = age50_55$soc_trace) + 
  #    padArray(pad = age60_65$soc_trace, pad_to = age50_55$soc_trace) +
  #    padArray(pad = age65_70$soc_trace, pad_to = age50_55$soc_trace) +
  #    padArray(pad = age70_75$soc_trace, pad_to = age50_55$soc_trace)
      
  # ai costs per patient
  # calculate AI costs per patient
  ai_screening_pp <- sum(sapply(age_results, function(x) x$ai_costs$ai_screening_costs)) / 1000
  ai_medicine_pp <- sum(sapply(age_results, function(x) x$ai_costs$ai_medicine_costs)) / 1000
  ai_diagnostic_pp <- sum(sapply(age_results, function(x) x$ai_costs$ai_diagnostic_costs)) / 1000
  ai_intervention_pp <- sum(sapply(age_results, function(x) x$ai_costs$ai_intervention_costs)) / 1000
  ai_burden_pp <- sum(sapply(age_results, function(x) x$ai_costs$ai_burden)) / 1000
  ai_productivity_pp <- sum(sapply(age_results, function(x) x$ai_costs$ai_productivity)) / 1000

  # calculate SoC costs per patient
  soc_medicine_pp <- sum(sapply(age_results, function(x) x$soc_costs$soc_medicine_costs)) / 1000
  soc_diagnostic_pp <- sum(sapply(age_results, function(x) x$soc_costs$soc_diagnostic_costs)) / 1000
  soc_intervention_pp <- sum(sapply(age_results, function(x) x$soc_costs$soc_intervention_costs)) / 1000
  soc_burden_pp <- sum(sapply(age_results, function(x) x$soc_costs$soc_burden)) / 1000
  soc_productivity_pp <- sum(sapply(age_results, function(x) x$soc_costs$soc_productivity)) / 1000


  if (strategy == "dsa"){ # if strategy is dsa, multiply respective costs with the multiplier
    if (parameter == "costs_screening"){
      ai_screening_pp <- ai_screening_pp * cost_multiplier
    } else if (parameter == "costs_medicine"){
      ai_medicine_pp <- ai_medicine_pp * cost_multiplier
      soc_medicine_pp <- soc_medicine_pp * cost_multiplier
    } else if (parameter == "costs_diagnostics"){
      ai_diagnostic_pp <- ai_diagnostic_pp * cost_multiplier
      soc_diagnostic_pp <- soc_diagnostic_pp * cost_multiplier
    } else if (parameter == "costs_intervention"){
      ai_intervention_pp <- ai_intervention_pp * cost_multiplier
      soc_intervention_pp <- soc_intervention_pp * cost_multiplier
    } else if (parameter == "costs_burden_disease"){
      ai_burden_pp <- ai_burden_pp * cost_multiplier
      soc_burden_pp <- soc_burden_pp * cost_multiplier
    } else if (parameter == "costs_productivity"){
      ai_productivity_pp <- ai_productivity_pp * cost_multiplier
      soc_productivity_pp <- soc_productivity_pp * cost_multiplier
    } 
  }
  if (perspective == "healthcare"){
    ai_productivity_pp <- 0
    soc_productivity_pp <- 0
  }

  # total costs per patient
  ai_costs_pp <- ai_screening_pp + ai_medicine_pp + ai_diagnostic_pp + ai_intervention_pp + ai_burden_pp + ai_productivity_pp
  soc_costs_pp <- soc_medicine_pp + soc_diagnostic_pp + soc_intervention_pp + soc_burden_pp + soc_productivity_pp

  # qaly per patient
  ai_qaly_pp <- sum(sapply(age_results, function(x) x$ai_qaly)) / 1000
  soc_qaly_pp <- sum(sapply(age_results, function(x) x$soc_qaly)) / 1000
  
  icer <- (ai_costs_pp - soc_costs_pp) / (ai_qaly_pp - soc_qaly_pp) 

  # print messag to console
  # if parameter variable doesnt exist, set it to NA
  if (!exists("parameter")){
    parameter <- NULL
  }
  
  if (descriptives == TRUE){ # if descriptives is true, print all information & calculate additional descriptives

    ai_time <- sapply(age_results, function(x) x$ai_time_spent)
    ai_time <- rowSums(sapply(age_results, function(x) x$ai_time_spent)) / 1000
    soc_time <- sum(sapply(age_results, function(x) x$soc_time_spent)) / 1000
    vi_prevented <- sum(sapply(age_results, function(x) x$vi_prevented)) / 1000

    print(ai_time) 
    print(soc_time)
    print(paste("years of visual impairment prevented:", vi_prevented))
    print(paste("screening cost pp AI:", round(ai_screening_pp, 2), "screening cost pp SOC:", round(0, 2)))
    print(paste("medicine cost pp AI:", round(ai_medicine_pp, 2), "medicine cost pp SOC:", round(soc_medicine_pp, 2)))
    print(paste("diagnostic cost pp AI:", round(ai_diagnostic_pp, 2), "diagnostic cost pp SOC:", round(soc_diagnostic_pp, 2)))
    print(paste("surgery & laser cost pp AI:", round(ai_intervention_pp, 2), "surgery & laser cost pp SOC:", round(soc_intervention_pp, 2)))
    print(paste("burden cost pp AI:", round(ai_burden_pp, 2), "burden cost pp SOC:", round(soc_burden_pp, 2)))
    print(paste("productivity cost pp AI:", round(ai_productivity_pp, 2), "productivity cost pp SOC:", round(soc_productivity_pp, 2)))
    print(paste("total cost pp AI:", round(ai_costs_pp, 2), "total cost pp SOC:", round(soc_costs_pp, 2)))
    print(paste("QALY pp AI:", round(ai_qaly_pp, 4), "QALY pp SOC:", round(soc_qaly_pp, 4)))
    print(paste("ICER:", round(icer, 2)))
  }
  return(icer) 
}


runModel <- function(cohort){
  # Isaac: I understand why you prefer to run age-dependent cohorts separately. 
  # However, this approach has several issues associated as discussed.
  # I wonder for example if the proportion of patients that are male should also be age-dependent. In fact, the same could be said about 
  # all parameters in the model, such as p_dt, p_severity_undiagnosed, p_transition or even the utilities. 
  # For the utilities, I know we consider age in terms of age-related decrement. However, the values at baseline were obtained 
  # for a cohort of a certain age. It could also be argued that these baseline values could also be age-dependent.
  # The reason I insist on this issue is because I believe it might be criticised by some reviewers. It's totally fine to have
  # different age cohorts, but then each cohort parameters' should be cohort specific (in theory) and I wonder whether that's
  # actually the case. Clear justification has to be provided for the assumptins made. 
  
  # inherit local variables from previous function (for dsa, if applicable)
  strategy <- get("strategy", envir = parent.frame())

  if (strategy == "dsa"){ # only inherit variables if stategy is dsa

    parameter  <- get("parameter", envir = parent.frame())

    if (parameter == "transition_untreated"){
      p_transition <- get("p_transition", envir = parent.frame())
    } else if (parameter == "transition_treated"){
      p_transition <- get("p_transition", envir = parent.frame())
    } else if (parameter == "sensitivity"){
      p_dt <- get("p_dt", envir = parent.frame())
    } else if (parameter == "specificity"){
      p_dt <- get("p_dt", envir = parent.frame())
    } else if (parameter == "prevalence"){
      v_prevalence <- get("v_prevalence", envir = parent.frame())
    } else if (parameter == "incidences_of"){
      v_incidences_of <- get("v_incidences_of", envir = parent.frame())
    } else if (parameter == "incidences_screening"){
      v_incidences_screening <- get("v_incidences_screening", envir = parent.frame())
    } else if (parameter == "utilities_untreated"){
      v_utilities <- get("v_utilities", envir = parent.frame())
    } else if (parameter == "utilities_treated"){
      v_utilities <- get("v_utilities", envir = parent.frame())
    }
  }

  if (strategy == "scenario"){    
    p_dt <- get("p_dt", envir = parent.frame())
    t_total_cohort <- get("t_total_cohort", envir = parent.frame())
    p_transition <- get("p_transition", envir = parent.frame())
    age_categories <- get("age_categories", envir = parent.frame())
    screening_interval <- get("screening_interval", envir = parent.frame())
    v_utilities <- get("v_utilities", envir = parent.frame())
    v_cost_utilisation_diagnostics <- get("v_cost_utilisation_diagnostics", envir = parent.frame())
    v_cost_utilisation_intervention <- get("v_cost_utilisation_intervention", envir = parent.frame())
    p_severity_undiagnosed <- get("p_severity_undiagnosed", envir = parent.frame())
  }
  #------------------------------------------------------------------------------#
  ####                       1 Decision Tree                            ####
  #------------------------------------------------------------------------------#
  v_incidence_of <- calculateIncidence(incidences = v_incidences_of, age_start = 50, age_max = 100) ### (function returns incidence per age year)
  v_incidence_screening <- calculateIncidence(incidences = v_incidences_screening, age_start = 50, age_max = 100) ### (function returns incidence per age year)
  incidences <- list(v_incidence_of = v_incidence_of, v_incidence_screening = v_incidence_screening)
  v_mortality <- calculateMortality(df_mortality = df_mortality, age_start = 50, age_max = 100) ### (function returns mortality per age year)

  # determine prevalence & incidence value for age specific cohort
  cohort_age <- strsplit(cohort, " ")[[1]]  # Split the cohort string by space
  cohort_min <- as.numeric(cohort_age[1])  # Extract the minimum age from the cohort
  cohort_max <- as.numeric(cohort_age[3])  # Extract the maximum age from the cohort
  
  # calculate amount of screening repetitions
  last_screen_age <- as.numeric(strsplit(age_categories[length(age_categories)], " ")[[1]][3])  # Split the cohort string by space and take the first element
  max_repititions <- floor((last_screen_age - cohort_min) / screening_interval)

  prevalence <- filter(v_prevalence, age >= cohort_min & age <= cohort_max)  # Filter prevalence based on cohort age range
  p_prevalence <- prevalence$prevalence  # Extract the prevalence value  p_severity_mild <- severity_distribution$mild

  ################## AI STRATEGY
  ### functions return probability of patients in each health state, seperately for each arm of the decision tree
  arms <- c("soc", "low_risk", "high_risk", "compliant")
  p_dt_ai <- list()

  for (arm in arms) {
    p_dt_ai[[arm]] <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, arm = arm, visualize = F, model_compliance = TRUE, p_prevalence = p_prevalence, cohort = cohort)
  }
  p_dt_ai <- CombineDT(traces = p_dt_ai) ### function combines all arms of the decision tree into single starting distribution per health state
  p_screening <- getScreeningProbabilities(probabilities = p_dt, model_compliance = TRUE, p_prevalence = p_prevalence) ### function returns list of probabilities related to each screening arm (for later use)
  v_cohort_ai <- lapply(p_dt_ai, function(x) x*(unname(t_total_cohort[cohort]) * 1000)) # re-scale to cohort of 1000 patients

  ################## SOC STRATEGY
  p_dt_soc <- getStartDistSoc(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, visualize = F, p_prevalence = p_prevalence, cohort = cohort) # obtain severity distribution 
  v_cohort_soc <- lapply(p_dt_soc, function(x) x*(unname(t_total_cohort[cohort]) * 1000)) # re-scale to cohort of 1000 patients


  # Isaac:  same as above: what we do here is to re-scale the cohorts based on the age distribution, but I'm not sure if that's correct.
  # I would think that the older patients the higher the probability of being severe, but right now the opposite happens. 
  # I think this happens for two reasons: 1. p_dt_soc is not age dependent and 2. t_total_cohort seems incorrect. 

  #------------------------------------------------------------------------------#
  ####                       2 Markov model                            ####
  #------------------------------------------------------------------------------#
  
  ################## AI STRATEGY
  #Isaac: we need to explain the input parameters of these functions (low priority right now)
  # We should also add what the function is returning. I see the traces (LYs, costs and utilities and total patients)
  # Question here: are these results discounted? I cannot see the option of discounting by the way and I think this was implemented in aprevious version of the code.
  a_trace_ai <- getMarkovTrace(scenario = "ai", ### (function returns list of (corrected) markov traces for the age category 50-55 years)
                                    cohort = v_cohort_ai,
                                    p_screening = p_screening, 
                                    df_mortality = v_mortality, 
                                    p_transition =  p_transition , 
                                    age_init = cohort_min,
                                    incidences = incidences,
                                    severity = p_severity_undiagnosed,
                                    interval = screening_interval, 
                                    max_repititions = max_repititions)  

  # Isaac: why is this called uncorrected? Have you checked that all rows sum to 1000 or that does not need to happen?
  # Also, unclear why we need to sum all these when traces will be of different length for each cohort.
  # The same applies for the two objects below.
  # For a_trace_ai_utillity, please check that a_trace_ai_utillity < a_trace_ai_uncorrected, and that they are 
  # equal if all utilities = 1.



  # create list with all traces #Isaac: what is the purpose of this object and why it contains only costs?

  ################## SOC STRATEGY
  a_trace_soc <- getMarkovTrace(scenario = "soc", ### (function returns list of (corrected) markov traces for the age category 50-55 years)
                                    cohort = v_cohort_soc,
                                    df_mortality = v_mortality, 
                                    p_transition =  p_transition, 
                                    age_init = cohort_min,
                                    incidences = incidences,
                                    interval = 0, 
                                    max_repititions = 0)                         

  ################################################################
  ########################### RESULTS ###########################
  ################################################################
  ai_time_spent <- getTimeSpent(a_trace = a_trace_ai$trace) ### (function returns time spent in each health state))
  soc_time_spent <- getTimeSpent(a_trace = a_trace_soc$trace) ### (function returns time spent in each health state))
  vi_prevented <- getBlindnessPrevented(a_trace_ai = a_trace_ai$trace, a_trace_soc = a_trace_soc$trace) ### (function returns amount of years blindness prevented)
  #ai_screening_descriptives <- getScreenignDescriptives(trace = a_trace_ai$trace, ### function returns total amount of fully screenings performed (fully compliant)
  #                                                    screening_probabilities = p_screening,
  #                                                    screening_cost = v_cost_dt, # obtain screening costs
  #                                                    interval = 5, 
  #                                                    max_repititions = 4) # screening repition reflects the amount of repitions IN ADDITION to the screening before the markov model

  #------------------------------------------------------------------------------#
  ####                       3 Utilities                           ####
  #------------------------------------------------------------------------------#
  # Isaac: these two functions are based on the objects above where you combined traces of different lengths.
  # As discussed before, I'm not sure why that step is needed. To validate the results, it'd be easier to calculate
  # total QALYs for each cohort separately and then take the weighted average.
  # It could also be a good exercise to check whether these calculations are correct, since they should be equal I suppose.
  # I've also noticed that changing age_init does not change the results of ai_total_qaly or soc_total_qaly. Is that correct?                        
  ai_total_qaly <- getQALYs(a_trace = a_trace_ai$trace_utility) ### (function returns total amount of QALY'S gained
  soc_total_qaly <- getQALYs(a_trace = a_trace_soc$trace_utility) ### (function returns total amount of QALY'S gained

  #------------------------------------------------------------------------------#
  ####                   4a Costs decision tree screening costs                    ####
  #------------------------------------------------------------------------------#

  # Isaac: I also don't understand this one since this should be different for each cohort. So the question is why the max is set to 4.
  # Again, why don't we get results per age cohort separately and then calculate the weighted average?
  # Is the cost of one screening sum(v_cost_dt)? This is equal to 188€. If ai_screening_costs = 557€, that means that on average there 
  # are 3 AI screenings per patient per lifetime. Do we consider that a valid result? My point is that we need to link that with the 
  # average age of the patient population. If this is 67 years, then 3 AI screenings do not seem correct. However, in the model now
  # I suppose the average age should be a bit above 60 years then. We need to consider whether that's valid or not.
  ai_screening_costs <- getScreeningCosts(trace = a_trace_ai$trace_cost, ### function returns screening costs per screening repetition
                                          screening_probabilities = p_screening,
                                          screening_cost = v_cost_dt, # obtain screening costs
                                          interval = screening_interval, 
                                          max_repititions = max_repititions) # screening repition reflects the amount of repitions IN ADDITION to the screening before the markov model 

  #------------------------------------------------------------------------------#
  ####                   4b Costs medicine                    ####
  #------------------------------------------------------------------------------

  # Isaac: same comments apply here and I suppose below too. One of the advantages of having separating age cohorts is the 
  # ability to show them separately, which is lost when these are combined in this way.  
  ai_medicine_costs <- getMedicineCosts(a_trace = a_trace_ai$trace_cost, ### function returns the total medicine costs
                                        medicine_cost = v_cost_medicine,
                                        medicine_utilisation = df_utilisation_medicine) # obtain medicine costs

  soc_medicine_costs <- getMedicineCosts(a_trace = a_trace_soc$trace_cost, # cohort trace of the patients non-compliant with AI screening
                                          medicine_cost = v_cost_medicine,
                                          medicine_utilisation = df_utilisation_medicine) # obtain medicine costs

  #------------------------------------------------------------------------------#
  ####                   4c Costs diagnostics                    ####
  #------------------------------------------------------------------------------
  # Isaac: please briefly define what these costs are
  ai_diagnostic_costs <- getDiagnosticCosts(trace = a_trace_ai$trace_cost, # cohort trace of the patients non-compliant with AI screening
                                            diagnostics_cost = v_cost_utilisation_diagnostics) # obtain diagnostic costs

  soc_diagnostic_costs <- getDiagnosticCosts(trace = a_trace_soc$trace_cost, # cohort trace of the patients non-compliant with AI screening
                                            diagnostics_cost = v_cost_utilisation_diagnostics) # obtain diagnostic costs

  #------------------------------------------------------------------------------#
  ####                   4d Costs intervention                    ####
  #------------------------------------------------------------------------------

  # Isaac: please briefly define what these costs are
  ai_intervention_costs <- getInterventionCosts(trace = a_trace_ai$trace_cost, # cohort trace of the patients non-compliant with AI screening
                                                intervention_cost = v_cost_utilisation_intervention,
                                                p_transition = p_transition,
                                                v_incidence_of = v_incidence_of,
                                                v_incidence_screening = v_incidence_screening,
                                                age_init = cohort_min,
                                                interval = screening_interval,
                                                max_repititions = max_repititions,
                                                strategy = "ai") # obtain intervention costs

  soc_intervention_costs <- getInterventionCosts(trace = a_trace_soc$trace_cost, # cohort trace of the patients non-compliant with AI screening
                                                intervention_cost = v_cost_utilisation_intervention,
                                                p_transition = p_transition,
                                                v_incidence_of = v_incidence_of,
                                                v_incidence_screening = v_incidence_screening,
                                                age_init = cohort_min,
                                                interval = screening_interval,
                                                max_repititions = max_repititions,
                                                strategy = "soc") # obtain intervention costs

  #------------------------------------------------------------------------------#
  ####                 4e Costs burden of disease visually impaired & blind   ####
  #------------------------------------------------------------------------------

  # Isaac: similar comments as above. In addition: societal_perspective = TRUE -> this is an option that we should have in another 
  # part of the code, so that we can easily change this once allowing to select societal or health care perspective.
  # I see for example that for burden you used a_trace_ai_cost but for productivity you used list_traces_ai_costs. 
  # I understand that list_traces_ai_costs is used for productivity because it's easier to assign 0 costs to some cohorts.
  # But this also has the advantage of getting results separately per cohort. As mentioned above, I find the approach of combining
  # traces of different length confusing and difficult to validate. I'd propose to use something like list_traces_ai_costs all the time
  # and have results separated per cohort (we will need to present these anyway!) and then take the weighted average for the total cohort results.
  # Also noticed that productivity costs change significantly if age_inits is changed as well.
  ai_burden <- getCostsBurdenOfDisease(costs = v_cost_burden_disease, trace = a_trace_ai$trace_cost, societal_perspective = TRUE)
  soc_burden <- getCostsBurdenOfDisease(costs = v_cost_burden_disease, trace = a_trace_soc$trace_cost, societal_perspective = TRUE)

  # productivity costs
  pension_age <- 66.583333333333333 # dutch pension age 2022

  if (cohort_min < pension_age){ # check if pension age is within the cohort

    ai_productivity <- getProductivityCosts(costs = v_cost_burden_disease, trace = a_trace_ai$trace_cost, age_init = cohort_min)
    soc_productivity <- getProductivityCosts(costs = v_cost_burden_disease, trace = a_trace_soc$trace_cost, age_init = cohort_min)

    if (cohort_min == 65){
      ai_productivity <- ai_productivity * (pension_age - 65)
      soc_productivity
    }
  } else {
    ai_productivity <- 0
    soc_productivity <- 0
  }

  #------------------------------------------------------------------------------#
  ####                         4f Total costs                                 ####
  #------------------------------------------------------------------------------#
  ai_costs <- list(ai_screening_costs = ai_screening_costs, ai_medicine_costs = ai_medicine_costs, ai_diagnostic_costs = ai_diagnostic_costs, ai_intervention_costs = ai_intervention_costs, ai_burden = ai_burden, ai_productivity = ai_productivity)
  soc_costs <- list(soc_medicine_costs = soc_medicine_costs, soc_diagnostic_costs = soc_diagnostic_costs, soc_intervention_costs = soc_intervention_costs, soc_burden = soc_burden, soc_productivity = soc_productivity)

  return(list(ai_costs = ai_costs, 
              soc_costs = soc_costs, 
              ai_qaly = ai_total_qaly, 
              soc_qaly = soc_total_qaly, 
              ai_trace = a_trace_ai$trace, 
              soc_trace = a_trace_soc$trace,
              ai_time_spent = ai_time_spent,
              soc_time_spent = soc_time_spent,
              vi_prevented = vi_prevented
              ))
}

