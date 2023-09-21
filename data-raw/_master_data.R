rm(list = ls()) # to clean the workspace

# Loading packages
library(tidyverse)

#------------------------------------------------------------------------------#
####                       01 All Cause Mortality                           ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/1_all_cause_mortality.csv" #Init statement
all_cause_mortality  <- read_delim(file.init, delim = ';') %>% #loading csv
  gather(key = "Attribute", value = "Value", -Topic, -Age) %>%  # Spread Topic and Value into separate columns
  spread(key = Topic, value = Value)

save(all_cause_mortality, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/1_all_cause_mortality.RData") # Create .rda object for initial set of parameters and store it in 'data' folder


#------------------------------------------------------------------------------#
####                              02 DT AI                                  ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/2_p_dt_ai.csv" #Init statement
p_dt_ai  <- read_delim(file.init, delim = ';') #loading csv
save(p_dt_ai, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/2_p_dt_ai.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                   03 DT severity distribution                          ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/3_p_severity_undiagnosed.csv" #Init statement
p_severity_undiagnosed  <- read_delim(file.init, delim = ';') #loading csv
save(p_severity_undiagnosed, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/3_p_severity_undiagnosed.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                    04 transition probabilities                         ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/4a_p_transition_ai.csv" #Init statement
p_transition_ai  <- read_delim(file.init, delim = ';') #loading csv
save(p_transition_ai, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/4a_p_transition_ai.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

