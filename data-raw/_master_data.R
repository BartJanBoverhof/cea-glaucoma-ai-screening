rm(list = ls()) # to clean the workspace
# loading packages
library(tidyverse)
library(readxl)

#------------------------------------------------------------------------------#
####                       01 All Cause Mortality                           ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/1_all_cause_mortality.csv" #Init statement
df_mortality  <- read_delim(file.init, delim = ';') %>% #loading csv
  gather(key = "Attribute", value = "Value", -Topic, -Age) %>%  # Spread Topic and Value into separate columns
  spread(key = Topic, value = Value)

save(df_mortality, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/1_df_mortality.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                              02 DT                                   ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/2_p_dt_ai.csv" #Init statement
p_dt  <- read_delim(file.init, delim = ';') #loading csv
save(p_dt, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/2_p_dt.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                   03 DT severity undiagnosed                          ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/3a_p_severity_undiagnosed.csv" #Init statement
p_severity_undiagnosed  <- read_delim(file.init, delim = ';') #loading csv
save(p_severity_undiagnosed, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/3a_p_severity_undiagnosed.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------
####                   03b DT severity diagnosed                          ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/3b_p_severity_diagnosed.csv" #Init statement
p_severity_diagnosed  <- read_delim(file.init, delim = ';') #loading csv
save(p_severity_diagnosed, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/3b_p_severity_diagnosed.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                   03c DT severity low_risk                          ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/3c_p_severity_low_risk.csv" #Init statement
p_severity_low_risk  <- read_delim(file.init, delim = ';') #loading csv
save(p_severity_low_risk, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/3c_p_severity_low_risk.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    04 transition probabilities                        ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/4_p_transition.csv" #Init statement
p_transition  <- read_delim(file.init, delim = ';') #loading csv
save(p_transition, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/4_p_transition.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    05a utilities                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/5a_utilities.csv" #Init statement
v_utilities  <- read_delim(file.init, delim = ';') #loading csv
save(v_utilities, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/5a_v_utilities.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    05b utilities age decrement                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/5b_utilities_age_decrement.xls" #Init statement
v_utilities_age_decrement  <- read_excel(file.init) #loading csv
save(v_utilities_age_decrement, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/5b_v_utilities_age_decrement.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    06 incidences                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/6_incidences.csv" #Init statement
v_incidences  <- read_delim(file.init, delim = ';') #loading csv
save(v_incidences, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/6_v_incidences.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    07 utilisation                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/7a_utilisation_medicine.csv" #Init statement
df_utilisation_medicine  <- read_delim(file.init, delim = ';') #loading csv
save(df_utilisation_medicine, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/7a_df_utilisation_medicine.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    08 costs                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/8a_cost_dt.csv" #Init statement
v_cost_dt  <- read_delim(file.init, delim = ';') #loading csv
save(v_cost_dt, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/8a_v_cost_dt.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

file.init <- "data-raw/8b_cost_medicine.csv" #Init statement
v_cost_medicine  <- read_delim(file.init, delim = ';') #loading csv
save(v_cost_medicine, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/8b_v_cost_medicine.RData") # Create .rda object for initial set of parameters and store it in 'data' folder
