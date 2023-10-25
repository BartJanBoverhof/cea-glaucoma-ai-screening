rm(list = ls()) # to clean the workspace
# loading packages
library(tidyverse)

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
####                   03 DT severity distribution                          ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/3_p_severity_undiagnosed.csv" #Init statement
p_severity_undiagnosed  <- read_delim(file.init, delim = ';') #loading csv
save(p_severity_undiagnosed, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/3_p_severity_undiagnosed.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    04a transition probabilities AI                        ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/4a_p_transition_ai.csv" #Init statement
p_transition_ai  <- read_delim(file.init, delim = ';') #loading csv
save(p_transition_ai, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/4a_p_transition_ai.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    04b transition probabilities SOC                        ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/4b_p_transition_soc.csv" #Init statement
p_transition_soc  <- read_delim(file.init, delim = ';') #loading csv
save(p_transition_soc, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/4b_p_transition_soc.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    05 utilities                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/5_utilities.csv" #Init statement
v_utilities  <- read_delim(file.init, delim = ';') #loading csv
save(v_utilities, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/5_v_utilities.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    06 incidences                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/6_incidences.csv" #Init statement
v_incidences  <- read_delim(file.init, delim = ';') #loading csv
save(v_incidences, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/6_v_incidences.RData") # Create .rda object for initial set of parameters and store it in 'data' folder
