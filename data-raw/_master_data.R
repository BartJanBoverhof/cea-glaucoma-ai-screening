rm(list = ls()) # to clean the workspace
# loading packages
library(tidyverse)
library(readxl)
getwd()

#------------------------------------------------------------------------------#
####                       01 All Cause Mortality                           ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/1_all_cause_mortality.csv" #Init statement
df_mortality  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = ".")) %>% #loading csv
  gather(key = "Attribute", value = "Value", -Topic, -Age) %>%  # Spread Topic and Value into separate columns
  spread(key = Topic, value = Value)

save(df_mortality, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/1_df_mortality.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                              02 DT                                   ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/2_p_dt_ai.csv" #Init statement
p_dt  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = "."))  #loading csv
save(p_dt, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/2_p_dt.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                   03 DT severity undiagnosed                          ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/3a_p_severity_undiagnosed.csv" #Init statement
p_severity_undiagnosed  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = ".")) 
save(p_severity_undiagnosed, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/3a_p_severity_undiagnosed.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    04 transition probabilities                        ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/4_p_transition.csv" #Init statement
p_transition  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = ".")) 
save(p_transition, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/4_p_transition.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    05a utilities                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/5a_utilities.csv" #Init statement
v_utilities  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = ".")) 
save(v_utilities, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/5a_v_utilities.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    05b utilities age decrement                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/5b_utilities_gp.xls" #Init statement
v_utilities_gp  <- read_excel(file.init) #loading csv
save(v_utilities_gp, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/5b_v_utilities_gp.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    06a incidences of                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/6a_incidences_of.csv" #Init statement
v_incidences_of  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = "."))
save(v_incidences_of, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/6a_v_incidences_of.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    06b incidences screening                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/6b_incidences_screening.csv" #Init statement
v_incidences_screening  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = "."))
save(v_incidences_screening, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/6b_v_incidences_screening.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    06c prevelance                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/6c_prevalence.csv" #Init statement
v_prevalence  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = "."))
save(v_prevalence, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/6c_v_prevalence.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    07 utilisation                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/7a_utilisation_medicine.csv" #Init statement
df_utilisation_medicine  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = ".")) 
save(df_utilisation_medicine, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/7a_df_utilisation_medicine.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    08 costs                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/8a_cost_dt.csv" #Init statement
v_cost_dt  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = ".")) 
save(v_cost_dt, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/8a_v_cost_dt.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

file.init <- "data-raw/8b_cost_medicine.csv" #Init statement
v_cost_medicine  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = ".")) 
save(v_cost_medicine, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/8b_v_cost_medicine.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

file.init <- "data-raw/8c_cost_utilisation_diagnostics.xlsx" #Init statement
v_cost_utilisation_diagnostics  <- read_excel(file.init)
save(v_cost_utilisation_diagnostics, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/8c_v_cost_utilisation_diagnostics.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

file.init <- "data-raw/8d_cost_utilisation_intervention.xlsx" #Init statement
v_cost_utilisation_intervention  <- read_excel(file.init)
save(v_cost_utilisation_intervention, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/8d_v_cost_utilisation_intervention.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

file.init <- "data-raw/8f_cost_burden_disease.xlsx" #Init statement
v_cost_burden_disease  <- read_excel(file.init)
save(v_cost_burden_disease, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/8f_v_cost_burden_disease.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

file.init <- "data-raw/9a_dsa_sd.csv" #Init statement
v_dsa_sd  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = "."))
save(v_dsa_sd, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/9a_v_dsa_sd.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

file.init <- "data-raw/9b_psa_sd.csv" #Init statement
v_psa_sd  <- read_delim(file.init, delim = ',', locale = locale(decimal_mark = "."))
save(v_psa_sd, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/9b_v_psa_sd.RData") # Create .rda object for initial set of parameters and store it in 'data' folder





















