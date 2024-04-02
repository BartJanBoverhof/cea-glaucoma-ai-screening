run <- function(cohort = age_categories[1]){
  # Isaac: I understand why you prefer to run age-dependent cohorts separately. 
  # However, this approach has several issues associated as discussed.
  # I wonder for example if the proportion of patients that are male should also be age-dependent. In fact, the same could be said about 
  # all parameters in the model, such as p_dt, p_severity_undiagnosed, p_transition or even the utilities. 
  # For the utilities, I know we consider age in terms of age-related decrement. However, the values at baseline were obtained 
  # for a cohort of a certain age. It could also be argued that these baseline values could also be age-dependent.
  # The reason I insist on this issue is because I believe it might be criticised by some reviewers. It's totally fine to have
  # different age cohorts, but then each cohort parameters' should be cohort specific (in theory) and I wonder whether that's
  # actually the case. Clear justification has to be provided for the assumptins made. 

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
  
  prevalence <- filter(v_prevalence, age >= cohort_min & age <= cohort_max)  # Filter prevalence based on cohort age range
  p_prevalence <- prevalence$prevalence  # Extract the prevalence value  p_severity_mild <- severity_distribution$mild

  ################## AI STRATEGY
  ### functions return probability of patients in each health state, seperately for each arm of the decision tree
  strategies <- c("soc", "low_risk", "high_risk", "compliant")
  p_dt_ai <- list()

  for (strategy in strategies) {
    p_dt_ai[[strategy]] <- getStartDistAI(probabilities = p_dt, severity_distribution = p_severity_undiagnosed, strategy = strategy, visualize = F, model_compliance = FALSE, p_prevalence = p_prevalence, cohort = cohort)
  }
p_dt_ai <- CombineDT(traces = p_dt_ai) ### function combines all arms of the decision tree into single starting distribution per health state
p_screening <- getScreeningProbabilities(probabilities = p_dt, model_compliance = FALSE, p_prevalence = p_prevalence) ### function returns list of probabilities related to each screening arm (for later use)
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
  # prerequisites
  age_inits <- c(52, 57, 62, 67, 72) # initial age for each age category 
  # Isaac: I understand you have chosen the middle point of each category, right? I think it's OK to start with 50 (not in the middle).

  ################## AI STRATEGY
  #Isaac: we need to explain the input parameters of these functions (low priority right now)
  # We should also add what the function is returning. I see the traces (LYs, costs and utilities and total patients)
  # Question here: are these results discounted? I cannot see the option of discounting by the way and I think this was implemented in aprevious version of the code.
  a_trace_ai <- getMarkovTrace(scenario = "ai", ### (function returns list of (corrected) markov traces for the age category 50-55 years)
                                    cohort = v_cohort_ai,
                                    screening_detection_rate = p_screening$p_fully_compliant, 
                                    df_mortality = v_mortality, 
                                    p_transition =  p_transition , 
                                    age_init = age_inits[1],
                                    incidences = incidences,
                                    interval = 5, 
                                    max_repititions = 4)  

  # Isaac: why is this called uncorrected? Have you checked that all rows sum to 1000 or that does not need to happen?
  # Also, unclear why we need to sum all these when traces will be of different length for each cohort.
  # The same applies for the two objects below.
  # For a_trace_ai_utillity, please check that a_trace_ai_utillity < a_trace_ai_uncorrected, and that they are 
  # equal if all utilities = 1.



  # create list with all traces #Isaac: what is the purpose of this object and why it contains only costs?

  ################## SOC STRATEGY
  a_trace_soc <- getMarkovTrace(scenario = "soc", ### (function returns list of (corrected) markov traces for the age category 50-55 years)
                                    cohort = v_cohort_soc,
                                    screening_detection_rate = 0, 
                                    df_mortality = v_mortality, 
                                    p_transition =  p_transition , 
                                    age_init = age_inits[1],
                                    incidences = incidences,
                                    interval = 0, 
                                    max_repititions = 0)                         

  ################################################################
  ########################### RESTULTS ###########################
  ################################################################
  ai_time_spent <- getTimeSpent(a_trace = a_trace_ai$trace) ### (function returns time spent in each health state))
  soc_time_spent <- getTimeSpent(a_trace = a_trace_soc$trace) ### (function returns time spent in each health state))
  blindness_prevented <- getBlindnessPrevented(a_trace_ai = a_trace_ai$trace, a_trace_soc = a_trace_soc$trace) ### (function returns amount of years blindness prevented)
  #ai_screening_descriptives <- getScreenignDescriptives(trace = a_trace_ai$trace, ### function returns total amount of fully screenings performed (fully compliant)
   #                                                   screening_probabilities = p_screening,
   #                                                   screening_cost = v_cost_dt, # obtain screening costs
    #                                                  interval = 5, 
   #                                                   max_repititions = 4) # screening repition reflects the amount of repitions IN ADDITION to the screening before the markov model

  #------------------------------------------------------------------------------#
  ####                       3 Utilities                           ####
  #------------------------------------------------------------------------------#
  # Isaac: these two functions are based on the objects above where you combined traces of different lengths.
  # As discussed before, I'm not sure why that step is needed. To validate the results, it'd be easier to calculate
  # total QALYs for each cohort separately and then take the weighted average.
  # It could also be a good exercise to check whether these calculations are correct, since they should be equal I suppose.
  # I've also noticed that changing age_init does not change the results of ai_total_qaly or soc_total_qaly. Is that correct?                        
  ai_total_qaly <- getQALYs(a_trace = a_trace_ai$trace_utility, v_utilities = v_utilities) ### (function returns total amount of QALY'S gained
  soc_total_qaly <- getQALYs(a_trace = a_trace_soc$trace_utility, v_utilities = v_utilities) ### (function returns total amount of QALY'S gained

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
                                          interval = 5, 
                                          max_repititions = 4) # screening repition reflects the amount of repitions IN ADDITION to the screening before the markov model 

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
                                              intervention_cost = v_cost_utilisation_intervention) # obtain intervention costs

  soc_intervention_costs <- getInterventionCosts(trace = a_trace_soc$trace_cost, # cohort trace of the patients non-compliant with AI screening
                                                intervention_cost = v_cost_utilisation_intervention) # obtain intervention costs

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

  return(list(ai_costs = ai_costs, soc_costs = soc_costs, ai_qaly = ai_total_qaly, soc_qaly = soc_total_qaly, ai_trace = a_trace_ai$trace, soc_trace = a_trace_soc$trace))
}
