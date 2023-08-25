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
file.init <- "data-raw/2_dt_ai.csv" #Init statement
dt_ai  <- read_delim(file.init, delim = ';') #loading csv
save(dt_ai, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/2_dt_ai.RData") # Create .rda object for initial set of parameters and store it in 'data' folder

#------------------------------------------------------------------------------#
####                   03 DT severity distribution                          ####
#------------------------------------------------------------------------------#
file.init <- "data-raw/3_severity_undiagnosed.csv" #Init statement
severity_undiagnosed  <- read_delim(file.init, delim = ';') #loading csv
save(severity_undiagnosed, file = "~/Documents/GitHub/cea-glaucoma-ai-screening/data/3_severity_undiagnosed.RData") # Create .rda object for initial set of parameters and store it in 'data' folder
